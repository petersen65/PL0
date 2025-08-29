// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emitter

import (
	"container/list"
	"math"
	"unicode/utf8"

	cor "github.com/petersen65/pl0/v3/core"
	dbg "github.com/petersen65/pl0/v3/debugging"
	elf "github.com/petersen65/pl0/v3/emitter/elf"
	x64 "github.com/petersen65/pl0/v3/emitter/x86_64"
	ic "github.com/petersen65/pl0/v3/generator/intermediate"
)

// Callstack must be aligned to 16 bytes before each function call.
const callStackAlignment = 16

// Sign bit mask for 64-bit double precision floating-point numbers in IEEE 754 binary representation.
const float64SignBitMask uint64 = 0x8000000000000000

// Sign bit mask for 32-bit single precision floating-point numbers in IEEE 754 binary representation.
const float32SignBitMask uint32 = 0x80000000

// Standard library symbols used in the intermediate code generation.
const (
	readStatementSymbol  = "@read"  // call the read function from the standard library
	writeStatementSymbol = "@write" // call the write function from the standard library
)

// Implementation of the assembly code emitter.
type emitter struct {
	intermediateCode ic.IntermediateCodeUnit // intermediate code unit to generate assembly code for
	assemblyCode     x64.AssemblyCodeUnit    // assembly code unit for the target platform
	debugInformation dbg.DebugInformation    // debug information collected during the code generation
	variableOffsets  map[string]int32        // 32-bit offset of local variables in their activation record
}

var (
	// Map intermediate code symbol names to standard library symbol names.
	standardLibrarySymbols = map[string]string{
		readStatementSymbol:  "pl0_read",
		writeStatementSymbol: "pl0_write",
	}

	// Map intermediate code data types to their sizes in bytes in the assembly code.
	dataTypeSize = map[ic.DataType]int32{
		ic.Integer64:  8,
		ic.Integer32:  4,
		ic.Integer16:  2,
		ic.Integer8:   1,
		ic.Float64:    8,
		ic.Float32:    4,
		ic.Unsigned64: 8,
		ic.Unsigned32: 4,
		ic.Unsigned16: 2,
		ic.Unsigned8:  1,
		ic.Boolean:    1,
		ic.Character:  4,
		ic.String:     16,
	}

	// Map intermediate code data types to their alignment in bytes in the assembly code.
	dataTypeAlignment = map[ic.DataType]int32{
		ic.Integer64:  8,
		ic.Integer32:  4,
		ic.Integer16:  2,
		ic.Integer8:   1,
		ic.Float64:    8,
		ic.Float32:    4,
		ic.Unsigned64: 8,
		ic.Unsigned32: 4,
		ic.Unsigned16: 2,
		ic.Unsigned8:  1,
		ic.Boolean:    1,
		ic.Character:  4,
		ic.String:     8,
	}

	// Map intermediate code data types to their return registers in the assembly code.
	dataTypeReturn = map[ic.DataType][]x64.Register{
		ic.Integer64:  {x64.Rax},
		ic.Integer32:  {x64.Eax},
		ic.Integer16:  {x64.Ax},
		ic.Integer8:   {x64.Al},
		ic.Float64:    {x64.Xmm0},
		ic.Float32:    {x64.Xmm0},
		ic.Unsigned64: {x64.Rax},
		ic.Unsigned32: {x64.Eax},
		ic.Unsigned16: {x64.Ax},
		ic.Unsigned8:  {x64.Al},
		ic.Boolean:    {x64.Al},
		ic.Character:  {x64.Eax},
		ic.String:     {x64.Rax, x64.Rdx},
	}

	// Map return registers to their zero masks required for zeroing out the upper bits of the register. 0 means that the register is not zeroed out.
	zeroMaskReturn = map[x64.Register]uint64{
		x64.Rax:  0,
		x64.Eax:  0,
		x64.Ax:   0x800000000000FFFF,
		x64.Al:   0x80000000000000FF,
		x64.Xmm0: 0,
		x64.Rdx:  0,
	}

	// For returning a pointer or reference, the address is returned in the Rax register.
	addressTypeReturn = x64.Rax

	// Map intermediate code data types to their DWARF attribute encodings for base types.
	// The string data type is intentionally excluded from this mapping because it is a composite type without encoding.
	dataTypeEncoding = map[ic.DataType]elf.DwarfAttributeEncoding{
		ic.Integer64:  elf.DW_ATE_signed,
		ic.Integer32:  elf.DW_ATE_signed,
		ic.Integer16:  elf.DW_ATE_signed,
		ic.Integer8:   elf.DW_ATE_signed,
		ic.Float64:    elf.DW_ATE_float,
		ic.Float32:    elf.DW_ATE_float,
		ic.Unsigned64: elf.DW_ATE_unsigned,
		ic.Unsigned32: elf.DW_ATE_unsigned,
		ic.Unsigned16: elf.DW_ATE_unsigned,
		ic.Unsigned8:  elf.DW_ATE_unsigned,
		ic.Boolean:    elf.DW_ATE_boolean,
		ic.Character:  elf.DW_ATE_UTF,
	}
)

// Return the interface of the emitter implementation.
func newEmitter(intermediateCodeUnit ic.IntermediateCodeUnit, buildConfiguration cor.BuildConfiguration, debugInformation dbg.DebugInformation) Emitter {
	targetPlatform := buildConfiguration.TargetPlatform

	// check if the target platform is supported by the emitter implementation
	if targetPlatform.OperatingSystem != cor.Linux ||
		targetPlatform.InstructionSetArchitecture != cor.X86_64 ||
		targetPlatform.InstructionSet != cor.ISA_SSE2 ||
		targetPlatform.StringEncoding != cor.UTF32 ||
		targetPlatform.ApplicationBinaryInterface != cor.ABI_SystemV_AMD64 {
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedTargetPlatform, targetPlatform, nil))
	}

	return &emitter{
		intermediateCode: intermediateCodeUnit,
		assemblyCode:     x64.NewAssemblyCodeUnit(buildConfiguration, debugInformation),
		debugInformation: debugInformation,
		variableOffsets:  make(map[string]int32),
	}
}

// Emit assembly code for the target platform.
func (e *emitter) Emit() {
	iterator := e.intermediateCode.GetIterator()
	comparison := x64.ComparisonNone

	// debugger flags to augment the directive and assembly code generation
	debugger := elf.DebuggerNone

	// compile-time parameters list for function calls
	parameters := list.New()

	// perform an assembly instruction selection for each intermediate code instruction
	for i, l := iterator.First(), make([]string, 0); i != nil; i = iterator.Next() {
		// panic if the intermediate code instruction has not a valid addresses contract
		c := i.Quadruple.ValidateAddressesContract()

		switch i.Quadruple.Operation {
		case ic.Odd: // check if the top of the call stack is an odd number and set the Zero Flag (ZF clear if odd, set if even)
			// extract the data type of the value to check for oddness
			dataType := i.Quadruple.Arg1.DataType

			// emit assembly code to check if the top of the call stack is odd
			e.odd(dataType, l, debugger, i.TokenStreamIndex)

			// business logic after the prologue has been emitted
			debugger &^= elf.DebuggerPrologueEnd

		case ic.Negate: // negate the top of the call stack and leave the result on the stack
			// extract the data type of the value to negate
			dataType := i.Quadruple.Arg1.DataType

			// emit assembly code to negate the top of the call stack
			e.negate(dataType, l, debugger, i.TokenStreamIndex)

			// business logic after the prologue has been emitted
			debugger &^= elf.DebuggerPrologueEnd

		case ic.Plus, ic.Minus, ic.Times, ic.Divide: // perform an arithmetic operation on the top two elements of the call stack and replace them with one result on the stack
			// extract the data type of the first value to perform the arithmetic operation on
			dataType := i.Quadruple.Arg1.DataType

			// it is required that the first and second value are of the same data type
			if dataType != i.Quadruple.Arg2.DataType {
				panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeInArithmeticOperation, dataType, nil))
			}

			// emit assembly code to perform the arithmetic operation on the top two elements of the call stack
			if i.Quadruple.Operation == ic.Divide && dataType.IsInteger() {
				e.divideInteger(dataType, l, debugger, i.TokenStreamIndex)
			} else {
				e.arithmeticOperation(dataType, i.Quadruple.Operation, l, debugger, i.TokenStreamIndex)
			}

			// business logic after the prologue has been emitted
			debugger &^= elf.DebuggerPrologueEnd

		case ic.Equal, ic.NotEqual, ic.Less, ic.LessEqual, ic.Greater, ic.GreaterEqual: // compare the top two elements of the call stack, remove them, and leave the result in the CPU flags register
			// extract the data type of the first value to perform the comparison operation on
			dataType := i.Quadruple.Arg1.DataType

			// it is required that the first and second value are of the same data type
			if dataType != i.Quadruple.Arg2.DataType {
				panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeInComparisonOperation, dataType, nil))
			}

			// emit assembly code to compare the top two elements of the call stack and remember the comparison type
			comparison = e.comparison(dataType, l, debugger, i.TokenStreamIndex)

			// business logic after the prologue has been emitted
			debugger &^= elf.DebuggerPrologueEnd

		case ic.Jump: // unconditionally jump to a label resolved at link-time
			// panic if the label name is not a string
			name := i.Quadruple.Result.Value.(string)

			// emit assembly code to perform an unconditional jump to the specified label
			e.unconditionalJump(name, l, debugger, i.TokenStreamIndex)

			// business logic after the prologue has been emitted
			debugger &^= elf.DebuggerPrologueEnd

		case ic.JumpEqual, ic.JumpNotEqual, ic.JumpLess, ic.JumpLessEqual, ic.JumpGreater, ic.JumpGreaterEqual: // conditional jump to a label resolved at link-time based on the CPU flags set by the previous comparison
			// panic if the label name is not a string
			name := i.Quadruple.Result.Value.(string)

			// emit assembly code to perform a conditional jump based on the CPU flags set by the previous comparison
			e.conditionalJump(comparison, i.Quadruple.Operation, name, l, debugger, i.TokenStreamIndex)

			// reset the comparison type after a conditional jump
			comparison = x64.ComparisonNone

			// business logic after the prologue has been emitted
			debugger &^= elf.DebuggerPrologueEnd

		case ic.BranchTarget: // target for any branching operation
			// append branch target labels for the directly following non 'BranchTarget' instruction
			l = append(l, i.Quadruple.Result.Value.(string))

		case ic.Parameter: // push a parameter onto the compile-time parameters list for a function call
			parameters.PushBack(i.Quadruple.Arg1)

		case ic.Call: // call a function by jumping to the function's label
			// panic if the label name is not a string
			name := i.Quadruple.Arg1.Value.(string)

			// extract the depth difference between function call and function declaration and panic if it is not a signed integer
			depthDifference := i.Quadruple.Arg2.Value.(int32)

			// emit assembly code to call the function with the given name
			e.callFunction(name, parameters, depthDifference, l, debugger, i.TokenStreamIndex)

			// reset the compile-time parameters list for the next function call
			parameters = list.New()

			// business logic after the prologue has been emitted
			debugger &^= elf.DebuggerPrologueEnd

		case ic.Return: // return from a function to its caller
			// extract the branch target label for the function return
			endOfFunctionLabel := i.Quadruple.Arg1.Value.(string)

			// check if the function returns a value by using the current addresses contract
			if c.Result == ic.Temporary {
				// emit assembly code to return from the function with a return value
				e.returnFromFunction(i.Quadruple.Result.DataType, l, endOfFunctionLabel, debugger, i.TokenStreamIndex)
			} else {
				// emit assembly code to return from the function without a return value
				e.returnFromFunction(ic.Untyped, l, endOfFunctionLabel, debugger, i.TokenStreamIndex)
			}

			// business logic after the prologue has been emitted
			debugger &^= elf.DebuggerPrologueEnd

		case ic.Prologue: // function entry sequence
			// extract the branch target label for the function start
			beginOfFunctionLabel := i.Quadruple.Arg1.Value.(string)

			// emit assembly code to prepare the activation record for the function call
			e.prologue(l, beginOfFunctionLabel, debugger, i.TokenStreamIndex)

		case ic.Epilogue: // function exit sequence
			// the epilogue will be emitted
			debugger |= elf.DebuggerEpilogueBegin

			// emit assembly code to restore the activation record of the caller
			e.epilogue(l, debugger, i.TokenStreamIndex)

			// the begin of the epilogue has been emitted
			debugger &^= elf.DebuggerEpilogueBegin

			// business logic after the prologue has been emitted
			debugger &^= elf.DebuggerPrologueEnd

		case ic.Setup: // initialize memory space and internal data structures
			// panic if the depth of the block is not a valid signed integer
			depth := i.Quadruple.Arg1.Value.(int32)

			// emit assembly code to setup a function call and do not end the prologue
			e.setup(depth, l, debugger&^elf.DebuggerPrologueEnd, i.TokenStreamIndex)

		case ic.AllocateVariable: // allocate memory for all variables in their memory space
			// emit assembly code to allocate space for local variables in the activation record
			e.allocateVariables(iterator, l, debugger, i.TokenStreamIndex)

			// the prologue has been emitted
			debugger |= elf.DebuggerPrologueEnd

		case ic.CopyLiteral: // copy an immediate value to an address
			// extract the literal value from the quadruple
			literal := i.Quadruple.Arg1.Value

			// extract the data type of the literal value
			dataType := i.Quadruple.Arg1.DataType

			// extract the literal data label from the quadruple
			ldLabel := i.Quadruple.Arg2.Value.(string)

			// emit assembly code to copy the literal onto the top of the call stack
			e.copyLiteral(dataType, literal, ldLabel, l, debugger, i.TokenStreamIndex)

			// business logic after the prologue has been emitted
			debugger &^= elf.DebuggerPrologueEnd

		case ic.LoadVariable: // load a variable from its call stack address onto the top of the stack
			// extract the data type of the variable
			dataType := i.Quadruple.Arg1.DataType

			// determine offset of the local variable in its activation record
			offset := e.variableOffsets[i.Quadruple.Arg1.Name]

			// extract the depth difference between variable use and variable declaration
			depthDifference := i.Quadruple.Arg2.Value.(int32)

			// emit assembly code to load the variable onto the call stack
			e.loadVariable(dataType, offset, depthDifference, l, debugger, i.TokenStreamIndex)

			// business logic after the prologue has been emitted
			debugger &^= elf.DebuggerPrologueEnd

		case ic.StoreVariable: // store the top of the call stack into a variable's stack address
			// extract the data type of the variable
			dataType := i.Quadruple.Result.DataType

			// determine offset of the local variable in its activation record
			offset := e.variableOffsets[i.Quadruple.Result.Name]

			// extract the depth difference between variable use and variable declaration
			depthDifference := i.Quadruple.Arg2.Value.(int32)

			// emit assembly code to store the top of the call stack into the variable's activation record
			e.storeVariable(dataType, offset, depthDifference, l, debugger, i.TokenStreamIndex)

			// business logic after the prologue has been emitted
			debugger &^= elf.DebuggerPrologueEnd

		default:
			panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unknownIntermediateCodeOperation, i.Quadruple.Operation, nil))
		}

		// collected branch target labels must be used by the directly following instruction (one instruction consumes all collected labels)
		if i.Quadruple.Operation != ic.BranchTarget {
			l = make([]string, 0)
		}
	}

	// update all simple data type sizes in the debug information
	for dataType, size := range dataTypeSize {
		e.debugInformation.UpdateSimpleDataTypeSize(dataType.String(), size)
	}

	// update all simple data type encodings in the debug information
	for dataType, encoding := range dataTypeEncoding {
		e.debugInformation.UpdateSimpleDataTypeEncoding(dataType.String(), int(encoding))
	}

	// update all pointer data type sizes in the debug information
	e.debugInformation.UpdatePointerDataTypeSizes(x64.PointerSize)

	// update all composite data type sizes in the debug information
	e.debugInformation.UpdateCompositeDataTypeSizes()

	// update all composite data type offsets in the debug information
	e.debugInformation.UpdateCompositeDataTypeOffsets()

	// update all variable offsets in the debug information
	for name, offset := range e.variableOffsets {
		e.debugInformation.UpdateVariableOffset(name, offset)
	}
}

// Get access to the generated assembly code.
func (e *emitter) GetAssemblyCodeUnit() x64.AssemblyCodeUnit {
	return e.assemblyCode
}

// Check if the top of the call stack is odd depending on the data type.
func (e *emitter) odd(dataType ic.DataType, btLabels []string, debugger elf.Debugger, index int) {
	// depending on the data type, the top of the call stack is popped into the correct register and checked for oddness
	switch dataType {
	case ic.Integer64, ic.Integer32, ic.Integer16, ic.Integer8:
		// all integer 64-bit, 32-bit, 16-bit and 8-bit values are checked for oddness the same way
		// note: all integer values must be correctly sign-extended to 64 bits before checking if they are odd
		e.assemblyCode.AppendInstruction(x64.Pop, btLabels, index,
			x64.NewRegisterOperand(x64.R10)).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		// test the least significant bit to determine oddness (64-bit 'Test' instruction is sufficient)
		e.assemblyCode.AppendInstruction(x64.Test, nil, index, x64.NewRegisterOperand(x64.R10), x64.NewImmediateOperand(uint64(1)))

	default:
		// panic if the data type is not supported for the intermediate code operation
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeForIntermediateCodeOperation, dataType, nil))
	}
}

// Negate the top of the call stack and leave the result on top of the stack.
func (e *emitter) negate(dataType ic.DataType, btLabels []string, debugger elf.Debugger, index int) {
	// depending on the data type, the top of the call stack is popped into the correct register and negated
	switch dataType {
	case ic.Integer64, ic.Integer32, ic.Integer16, ic.Integer8:
		// all integer 64-bit, 32-bit, 16-bit and 8-bit values are negated the same way
		// note: all integer values must be correctly sign-extended to 64 bits before negation
		e.assemblyCode.AppendInstruction(x64.Pop, btLabels, index,
			x64.NewRegisterOperand(x64.R10)).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		e.assemblyCode.AppendInstruction(x64.Neg, nil, index, x64.NewRegisterOperand(x64.R10))
		e.assemblyCode.AppendInstruction(x64.Push, nil, index, x64.NewRegisterOperand(x64.R10))

	case ic.Float64:
		// move the 64-bit floating-point value (IEEE 754 double precision) to the XMM0 register
		e.assemblyCode.AppendInstruction(x64.Movsd, btLabels, index,
			x64.NewRegisterOperand(x64.Xmm0),
			x64.NewMemoryOperand(x64.Rsp, x64.Bits64)).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		// move the 64-bit floating-point sign bit mask into the R10 register
		e.assemblyCode.AppendInstruction(x64.Mov, nil, index,
			x64.NewRegisterOperand(x64.R10),
			x64.NewImmediateOperand(float64SignBitMask))

		// move the 64-bit floating-point sign bit mask into the XMM1 register
		e.assemblyCode.AppendInstruction(x64.Movq, nil, index,
			x64.NewRegisterOperand(x64.Xmm1),
			x64.NewRegisterOperand(x64.R10))

		// perform a bitwise XOR operation between the XMM0 register and the XMM1 register
		// note: this flips the sign bit of the floating-point value in the XMM0 register
		e.assemblyCode.AppendInstruction(x64.Xorpd, nil, index,
			x64.NewRegisterOperand(x64.Xmm0),
			x64.NewRegisterOperand(x64.Xmm1))

		// move the negated 64-bit floating-point value back onto the top of the call stack
		e.assemblyCode.AppendInstruction(x64.Movsd, nil, index,
			x64.NewMemoryOperand(x64.Rsp, x64.Bits64),
			x64.NewRegisterOperand(x64.Xmm0))

	case ic.Float32:
		// move the 32-bit floating-point value (IEEE 754 single precision) to the XMM0 register
		// note: the Bits32 is redundant here because the 'Movss' instruction always expects a 32-bit value
		e.assemblyCode.AppendInstruction(x64.Movss, btLabels, index,
			x64.NewRegisterOperand(x64.Xmm0),
			x64.NewMemoryOperand(x64.Rsp, x64.Bits32)).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		// move the 32-bit floating-point sign bit mask into the R10d register
		e.assemblyCode.AppendInstruction(x64.Mov, nil, index,
			x64.NewRegisterOperand(x64.R10d),
			x64.NewImmediateOperand(float32SignBitMask))

		// move the 32-bit floating-point sign bit mask into the XMM1 register
		e.assemblyCode.AppendInstruction(x64.Movd, nil, index,
			x64.NewRegisterOperand(x64.Xmm1),
			x64.NewRegisterOperand(x64.R10d))

		// perform a bitwise XOR operation between the XMM0 register and the XMM1 register
		// note: this flips the sign bit of the single precision floating-point value in the XMM0 register
		e.assemblyCode.AppendInstruction(x64.Xorps, nil, index,
			x64.NewRegisterOperand(x64.Xmm0),
			x64.NewRegisterOperand(x64.Xmm1))

		// move the negated 32-bit floating-point value back onto the top of the call stack
		e.assemblyCode.AppendInstruction(x64.Movss, nil, index,
			x64.NewMemoryOperand(x64.Rsp, x64.Bits32),
			x64.NewRegisterOperand(x64.Xmm0))

		// clear the upper 32 bits from [RSP] to maintain a clean 64-bit call stack slot
		e.assemblyCode.AppendInstruction(x64.Mov, nil, index,
			x64.NewMemoryOperand(x64.Rsp, x64.Bits32, x64.DoubleWordSize),
			x64.NewImmediateOperand(uint32(0)))

	default:
		// panic if the data type is not supported for the intermediate code operation
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeForIntermediateCodeOperation, dataType, nil))
	}
}

// Perform the arithmetic operations 'Plus', 'Minus', 'Times' on the top two elements of the call stack and replace them with one result on the stack.
func (e *emitter) arithmeticOperation(dataType ic.DataType, operation ic.Operation, btLabels []string, debugger elf.Debugger, index int) {
	// depending on the data type, the top two elements of the call stack are popped into the correct registers and the arithmetic operation is performed
	switch dataType {
	case ic.Integer64, ic.Integer32, ic.Integer16, ic.Integer8, ic.Unsigned64, ic.Unsigned32, ic.Unsigned16, ic.Unsigned8:
		// pop the right-hand integer value from the call stack into the R11 register
		// note: all integer values must be correctly sign-extended to 64 bits before arithmetic operations
		e.assemblyCode.AppendInstruction(x64.Pop, btLabels, index,
			x64.NewRegisterOperand(x64.R11)).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		// pop the left-hand integer value from the call stack into the R10 register
		// note: all integer values must be correctly sign-extended to 64 bits before arithmetic operations
		e.assemblyCode.AppendInstruction(x64.Pop, nil, index, x64.NewRegisterOperand(x64.R10))

		// the R10 and R11 registers are used to perform the arithmetic operation which leaves the result in the R10 register
		switch operation {
		case ic.Plus:
			// add the right-hand integer value in the R11 register to the left-hand integer value in the R10 register
			e.assemblyCode.AppendInstruction(x64.Add, nil, index, x64.NewRegisterOperand(x64.R10), x64.NewRegisterOperand(x64.R11))

		case ic.Minus:
			// subtract the right-hand integer value in the R11 register from the left-hand integer value in the R10 register
			e.assemblyCode.AppendInstruction(x64.Sub, nil, index, x64.NewRegisterOperand(x64.R10), x64.NewRegisterOperand(x64.R11))

		case ic.Times:
			// multiply the left-hand integer value in the R10 register with the right-hand integer value in the R11 register
			e.assemblyCode.AppendInstruction(x64.Imul, nil, index, x64.NewRegisterOperand(x64.R10), x64.NewRegisterOperand(x64.R11))
		}

		// push the result of the arithmetic operation onto the call stack
		// note: flags from the 'Add', 'Sub', and 'Imul' instructions are intentionally ignored and will be overwritten by the 'Push' instruction
		e.assemblyCode.AppendInstruction(x64.Push, nil, index, x64.NewRegisterOperand(x64.R10))

	case ic.Float64:
		// move the right-hand 64-bit floating-point value (IEEE 754 double precision) from the call stack into the XMM1 register
		e.assemblyCode.AppendInstruction(x64.Movsd, btLabels, index,
			x64.NewRegisterOperand(x64.Xmm1),
			x64.NewMemoryOperand(x64.Rsp, x64.Bits64)).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		// move the left-hand 64-bit floating-point value (IEEE 754 double precision) from the call stack into the XMM0 register
		e.assemblyCode.AppendInstruction(x64.Movsd, nil, index,
			x64.NewRegisterOperand(x64.Xmm0),
			x64.NewMemoryOperand(x64.Rsp, x64.Bits64, x64.PointerSize))

		// the XMM0 and XMM1 registers are used to perform the arithmetic operation which leaves the result in the XMM0 register
		switch operation {
		case ic.Plus:
			// add the right-hand floating-point value in the XMM1 register to the left-hand floating-point value in the XMM0 register
			e.assemblyCode.AppendInstruction(x64.Addsd, nil, index,
				x64.NewRegisterOperand(x64.Xmm0),
				x64.NewRegisterOperand(x64.Xmm1))

		case ic.Minus:
			// subtract the right-hand floating-point value in the XMM1 register from the left-hand floating-point value in the XMM0 register
			e.assemblyCode.AppendInstruction(x64.Subsd, nil, index,
				x64.NewRegisterOperand(x64.Xmm0),
				x64.NewRegisterOperand(x64.Xmm1))

		case ic.Times:
			// multiply the right-hand floating-point value in the XMM1 register with the left-hand floating-point value in the XMM0 register
			e.assemblyCode.AppendInstruction(x64.Mulsd, nil, index,
				x64.NewRegisterOperand(x64.Xmm0),
				x64.NewRegisterOperand(x64.Xmm1))

		case ic.Divide:
			// divide the left-hand floating-point value in the XMM0 register by the right-hand floating-point value in the XMM1 register
			e.assemblyCode.AppendInstruction(x64.Divsd, nil, index,
				x64.NewRegisterOperand(x64.Xmm0),
				x64.NewRegisterOperand(x64.Xmm1))
		}

		// remove the top element of the call stack (the right-hand value) by adjusting the stack pointer by 1 times the pointer size
		e.assemblyCode.AppendInstruction(x64.Add, nil, index,
			x64.NewRegisterOperand(x64.Rsp),
			x64.NewImmediateOperand(int32(x64.PointerSize)))

		// move the result of the arithmetic operation back onto the top of the call stack, overwriting the left-hand value
		e.assemblyCode.AppendInstruction(x64.Movsd, nil, index,
			x64.NewMemoryOperand(x64.Rsp, x64.Bits64),
			x64.NewRegisterOperand(x64.Xmm0))

	case ic.Float32:
		// move the right-hand 32-bit floating-point value (IEEE 754 single precision) from the call stack into the XMM1 register
		e.assemblyCode.AppendInstruction(x64.Movss, btLabels, index,
			x64.NewRegisterOperand(x64.Xmm1),
			x64.NewMemoryOperand(x64.Rsp, x64.Bits32)).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		// move the left-hand 32-bit floating-point value (IEEE 754 single precision) from the call stack into the XMM0 register
		e.assemblyCode.AppendInstruction(x64.Movss, nil, index,
			x64.NewRegisterOperand(x64.Xmm0),
			x64.NewMemoryOperand(x64.Rsp, x64.Bits32, x64.PointerSize))

		// the XMM0 and XMM1 registers are used to perform the arithmetic operation which leaves the result in the XMM0 register
		switch operation {
		case ic.Plus:
			// add the right-hand floating-point value in the XMM1 register to the left-hand floating-point value in the XMM0 register
			e.assemblyCode.AppendInstruction(x64.Addss, nil, index,
				x64.NewRegisterOperand(x64.Xmm0),
				x64.NewRegisterOperand(x64.Xmm1))

		case ic.Minus:
			// subtract the right-hand floating-point value in the XMM1 register from the left-hand floating-point value in the XMM0 register
			e.assemblyCode.AppendInstruction(x64.Subss, nil, index,
				x64.NewRegisterOperand(x64.Xmm0),
				x64.NewRegisterOperand(x64.Xmm1))

		case ic.Times:
			// multiply the right-hand floating-point value in the XMM1 register with the left-hand floating-point value in the XMM0 register
			e.assemblyCode.AppendInstruction(x64.Mulss, nil, index,
				x64.NewRegisterOperand(x64.Xmm0),
				x64.NewRegisterOperand(x64.Xmm1))

		case ic.Divide:
			// divide the left-hand floating-point value in the XMM0 register by the right-hand floating-point value in the XMM1 register
			e.assemblyCode.AppendInstruction(x64.Divss, nil, index,
				x64.NewRegisterOperand(x64.Xmm0),
				x64.NewRegisterOperand(x64.Xmm1))
		}

		// remove the top element of the call stack (the right-hand value) by adjusting the stack pointer by 1 times the pointer size
		e.assemblyCode.AppendInstruction(x64.Add, nil, index,
			x64.NewRegisterOperand(x64.Rsp),
			x64.NewImmediateOperand(int32(x64.PointerSize)))

		// // move the result of the arithmetic operation back onto the top of the call stack and overwrite the left-hand value's lower 32 bits
		e.assemblyCode.AppendInstruction(x64.Movss, nil, index,
			x64.NewMemoryOperand(x64.Rsp, x64.Bits32),
			x64.NewRegisterOperand(x64.Xmm0))

		// clear the upper 32 bits from [RSP] to maintain a clean 64-bit call stack slot
		e.assemblyCode.AppendInstruction(x64.Mov, nil, index,
			x64.NewMemoryOperand(x64.Rsp, x64.Bits32, x64.DoubleWordSize),
			x64.NewImmediateOperand(uint32(0)))

	default:
		// panic if the data type is not supported for the intermediate code operation
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeForIntermediateCodeOperation, dataType, nil))
	}
}

// Perform the arithmetic operation 'Divide' on the top two elements of the call stack and replace them with one result on the stack.
func (e *emitter) divideInteger(dataType ic.DataType, btLabels []string, debugger elf.Debugger, index int) {
	// depending on the data type, the top two elements of the call stack are popped into the correct registers and the division operation is performed
	switch dataType {
	case ic.Integer64, ic.Integer32, ic.Integer16, ic.Integer8:
		// pop the right-hand integer value (divisor) from the call stack into the R11 register
		// note: all integer values must be correctly sign-extended to 64 bits before arithmetic operations
		e.assemblyCode.AppendInstruction(x64.Pop, btLabels, index,
			x64.NewRegisterOperand(x64.R11)).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		// pop the left-hand integer value (dividend) from the call stack into the RAX register (required by IDIV)
		// note: all integer values must be correctly sign-extended to 64 bits before arithmetic operations
		e.assemblyCode.AppendInstruction(x64.Pop, nil, index, x64.NewRegisterOperand(x64.Rax))

		// sign-extend RAX to RDX:RAX by using the 'Cqo' instruction for a signed division (convert quadword to octword)
		// note: CQO operates on 64-bit operands but is safe to use with smaller types, as long as the value in RAX is properly sign-extended to 64 bits beforehand
		e.assemblyCode.AppendInstruction(x64.Cqo, nil, index)

		// divide the 128-bit dividend in RDX:RAX by the 64-bit divisor in R11 (signed division)
		// note: the quotient is stored in RAX and the remainder in RDX
		e.assemblyCode.AppendInstruction(x64.Idiv, nil, index, x64.NewRegisterOperand(x64.R11))

		// push the result onto the stack (quotient)
		e.assemblyCode.AppendInstruction(x64.Push, nil, index, x64.NewRegisterOperand(x64.Rax))

	case ic.Unsigned64, ic.Unsigned32, ic.Unsigned16, ic.Unsigned8:
		// pop the right-hand integer value (divisor) from the call stack into the R11 register
		// note: all unsigned integer values must be correctly zero-extended to 64 bits before arithmetic operations
		e.assemblyCode.AppendInstruction(x64.Pop, btLabels, index,
			x64.NewRegisterOperand(x64.R11)).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		// pop the left-hand integer value (dividend) from the call stack into the RAX register (required by DIV)
		// note: all unsigned integer values must be correctly zero-extended to 64 bits before arithmetic operations
		e.assemblyCode.AppendInstruction(x64.Pop, nil, index, x64.NewRegisterOperand(x64.Rax))

		// zero-extend RAX to RDX:RAX by using the 'Xor' instruction for an unsigned division (clear RDX)
		e.assemblyCode.AppendInstruction(x64.Xor, nil, index, x64.NewRegisterOperand(x64.Rdx), x64.NewRegisterOperand(x64.Rdx))

		// divide the 128-bit dividend in RDX:RAX by the 64-bit divisor in R11 (unsigned division)
		// note: the quotient is stored in RAX and the remainder in RDX
		e.assemblyCode.AppendInstruction(x64.Div, nil, index, x64.NewRegisterOperand(x64.R11))

		// push the result onto the stack (quotient)
		e.assemblyCode.AppendInstruction(x64.Push, nil, index, x64.NewRegisterOperand(x64.Rax))

	default:
		// panic if the data type is not supported for the intermediate code operation
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeForIntermediateCodeOperation, dataType, nil))
	}
}

// Compare the top two elements of the call stack and update the CPU flags.
func (e *emitter) comparison(dataType ic.DataType, btLabels []string, debugger elf.Debugger, index int) x64.ComparisonType {
	var comparisonType x64.ComparisonType

	// depending on the data type, evaluate the comparison type for the conditional jump instructions
	// note: the comparison type is used to determine how the CPU flags are interpreted by following conditional jump instructions
	if dataType.IsPointer() || dataType.IsReference() {
		// pointer and reference types are compared as 64-bit unsigned integers
		// no dereferencing is performed, the raw address values are compared directly
		comparisonType = x64.ComparisonIntegerUnsigned
	} else {
		// for all other data types, the comparison type is determined by the data type
		switch dataType {
		case ic.Integer64, ic.Integer32, ic.Integer16, ic.Integer8, ic.Character:
			comparisonType = x64.ComparisonIntegerSigned

		case ic.Unsigned64, ic.Unsigned32, ic.Unsigned16, ic.Unsigned8, ic.Boolean:
			comparisonType = x64.ComparisonIntegerUnsigned

		case ic.Float64, ic.Float32:
			comparisonType = x64.ComparisonFloat

		default:
			// panic if the data type is not supported for the intermediate code operation
			panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeForIntermediateCodeOperation, dataType, nil))
		}
	}

	// depending on the data type, the top two elements of the call stack are popped into the correct registers and compared
	if dataType.IsPointer() || dataType.IsReference() {
		// pop the right-hand pointer/reference value from the call stack into the R11 register
		e.assemblyCode.AppendInstruction(x64.Pop, btLabels, index,
			x64.NewRegisterOperand(x64.R11)).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		// pop the left-hand pointer/reference value from the call stack into the R10 register
		e.assemblyCode.AppendInstruction(x64.Pop, nil, index, x64.NewRegisterOperand(x64.R10))

		// compare the left-hand pointer/reference value in R10 with the right-hand pointer/reference value in R11 (R10 - R11)
		e.assemblyCode.AppendInstruction(x64.Cmp, nil, index, x64.NewRegisterOperand(x64.R10), x64.NewRegisterOperand(x64.R11))
	} else {
		switch dataType {
		case ic.Integer64, ic.Integer32, ic.Integer16, ic.Integer8, ic.Character, ic.Unsigned64, ic.Unsigned32, ic.Unsigned16, ic.Unsigned8, ic.Boolean:
			// pop the right-hand integer value from the call stack into the R11 register
			// note: all integer values must be extended to 64 bits before arithmetic operations
			//       - sign-extended for signed integers (includes Unicode)
			//       - zero-extended for unsigned integers (includes Boolean)
			e.assemblyCode.AppendInstruction(x64.Pop, btLabels, index,
				x64.NewRegisterOperand(x64.R11)).
				AppendDirective(e.assemblyCode.Location(index, debugger))

			// pop the left-hand integer value from the call stack into the R10 register
			// note: all integer values must be extended to 64 bits before arithmetic operations
			//       - sign-extended for signed integers (includes Unicode)
			//       - zero-extended for unsigned integers (includes Boolean)
			e.assemblyCode.AppendInstruction(x64.Pop, nil, index, x64.NewRegisterOperand(x64.R10))

			// compare the left-hand integer value in R10 with the right-hand integer value in R11 (R10 - R11)
			// note: the 'Cmp' instruction itself does not distinguish between signed and unsigned values
			//       - conditional jump instructions interpret the CPU flags according to signedness
			e.assemblyCode.AppendInstruction(x64.Cmp, nil, index, x64.NewRegisterOperand(x64.R10), x64.NewRegisterOperand(x64.R11))

		case ic.Float64:
			// move the right-hand 64-bit floating-point value (IEEE 754 double precision) from the call stack into the XMM1 register
			e.assemblyCode.AppendInstruction(x64.Movsd, btLabels, index,
				x64.NewRegisterOperand(x64.Xmm1),
				x64.NewMemoryOperand(x64.Rsp, x64.Bits64)).
				AppendDirective(e.assemblyCode.Location(index, debugger))

			// move the left-hand 64-bit floating-point value (IEEE 754 double precision) from the call stack into the XMM0 register
			e.assemblyCode.AppendInstruction(x64.Movsd, nil, index,
				x64.NewRegisterOperand(x64.Xmm0),
				x64.NewMemoryOperand(x64.Rsp, x64.Bits64, x64.PointerSize))

			// remove both top elements of the call stack (right-hand and left-hand value) by adjusting the stack pointer by 2 times the pointer size
			e.assemblyCode.AppendInstruction(x64.Add, nil, index,
				x64.NewRegisterOperand(x64.Rsp),
				x64.NewImmediateOperand(int32(2*x64.PointerSize)))

			// compare the left-hand floating-point value in the XMM0 register with the right-hand floating-point value in the XMM1 register
			// note: the 'Ucomisd' instruction compares the two values and sets the CPU flags accordingly
			//       it does not raise an exception for NaN values, but sets the CPU flags to indicate unordered comparisons
			e.assemblyCode.AppendInstruction(x64.Ucomisd, nil, index, x64.NewRegisterOperand(x64.Xmm0), x64.NewRegisterOperand(x64.Xmm1))

		case ic.Float32:
			// move the right-hand 32-bit floating-point value (IEEE 754 single precision) from the call stack into the XMM1 register
			// note: the value is stored in a 64-bit stack slot (lower 32 bits contain the float)
			e.assemblyCode.AppendInstruction(x64.Movss, btLabels, index,
				x64.NewRegisterOperand(x64.Xmm1),
				x64.NewMemoryOperand(x64.Rsp, x64.Bits32)).
				AppendDirective(e.assemblyCode.Location(index, debugger))

			// move the left-hand 32-bit floating-point value (IEEE 754 single precision) from the call stack into the XMM0 register
			// note: the value is stored in a 64-bit stack slot (lower 32 bits contain the float)
			e.assemblyCode.AppendInstruction(x64.Movss, nil, index,
				x64.NewRegisterOperand(x64.Xmm0),
				x64.NewMemoryOperand(x64.Rsp, x64.Bits32, x64.PointerSize))

			// remove both top elements of the call stack (right-hand and left-hand value) by adjusting the stack pointer by 2 times the pointer size
			e.assemblyCode.AppendInstruction(x64.Add, nil, index,
				x64.NewRegisterOperand(x64.Rsp),
				x64.NewImmediateOperand(int32(2*x64.PointerSize)))

			// compare the left-hand floating-point value in the XMM0 register with the right-hand floating-point value in the XMM1 register
			// note: the 'Ucomiss' instruction compares the two values and sets the CPU flags accordingly
			//       it does not raise an exception for NaN values, but sets the CPU flags to indicate unordered comparisons
			e.assemblyCode.AppendInstruction(x64.Ucomiss, nil, index, x64.NewRegisterOperand(x64.Xmm0), x64.NewRegisterOperand(x64.Xmm1))

		default:
			// panic if the data type is not supported for the intermediate code operation
			panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeForIntermediateCodeOperation, dataType, nil))
		}
	}

	// return the comparison type that was used to set the CPU flags
	return comparisonType
}

// Perform an unconditional jump to the specified label.
func (e *emitter) unconditionalJump(name string, btLabels []string, debugger elf.Debugger, index int) {
	e.assemblyCode.AppendInstruction(x64.Jmp, btLabels, index,
		x64.NewLabelOperand(name)).
		AppendDirective(e.assemblyCode.Location(index, debugger))
}

// Perform a conditional jump based on the CPU flags set by the previous comparison operation.
func (e *emitter) conditionalJump(comparisonType x64.ComparisonType, jump ic.Operation, name string, btLabels []string, debugger elf.Debugger, index int) {
	// Select the correct jump instruction based on comparison type and operation
	var opcode x64.OperationCode

	// determine the opcode for the conditional jump instruction based on the comparison type and jump operation
	switch jump {
	case ic.JumpEqual:
		// left-hand value is equal to right-hand value
		// note: valid for all comparison types (ZF=1)
		opcode = x64.Je

	case ic.JumpNotEqual:
		// left-hand value is not equal to right-hand value
		// note: valid for all comparison types (ZF=0)
		opcode = x64.Jne

	case ic.JumpLess:
		// left-hand value is less than right-hand value
		switch comparisonType {
		case x64.ComparisonIntegerUnsigned, x64.ComparisonFloat:
			// note: valid for unsigned integers and floats (CF=1)
			opcode = x64.Jb

		case x64.ComparisonIntegerSigned:
			// note: valid for signed integers (SF≠OF)
			opcode = x64.Jl
		}

	case ic.JumpLessEqual:
		// left-hand value is less than or equal to right-hand value
		switch comparisonType {
		case x64.ComparisonIntegerUnsigned, x64.ComparisonFloat:
			// note: valid for unsigned integers and floats (CF=1 or ZF=1)
			opcode = x64.Jbe

		case x64.ComparisonIntegerSigned:
			// note: valid for signed integers (SF≠OF or ZF=1)
			opcode = x64.Jle
		}

	case ic.JumpGreater:
		// left-hand value is greater than right-hand value
		switch comparisonType {
		case x64.ComparisonIntegerUnsigned, x64.ComparisonFloat:
			// note: valid for unsigned integers and floats (CF=0 and ZF=0)
			opcode = x64.Ja

		case x64.ComparisonIntegerSigned:
			// note: valid for signed integers (SF=OF and ZF=0)
			opcode = x64.Jg
		}

	case ic.JumpGreaterEqual:
		// left-hand value is greater than or equal to right-hand value
		switch comparisonType {
		case x64.ComparisonIntegerUnsigned, x64.ComparisonFloat:
			// note: valid for unsigned integers and floats (CF=0)
			opcode = x64.Jae

		case x64.ComparisonIntegerSigned:
			// note: valid for signed integers (SF=OF)
			opcode = x64.Jge
		}

	default:
		// panic if the jump operation is not supported for the intermediate code operation
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedJumpOperationForConditionalJump, jump, nil))
	}

	// emit assembly code for the conditional jump instruction
	e.assemblyCode.AppendInstruction(opcode, btLabels, index,
		x64.NewLabelOperand(name)).
		AppendDirective(e.assemblyCode.Location(index, debugger))
}

// Call a user-defined or standard library function by its intermediate code symbol name.
func (e *emitter) callFunction(intermediateCodeName string, _ *list.List, depthDifference int32, btLabels []string, debugger elf.Debugger, index int) {
	// depending on the intermediate code symbol name, handle cases for standard library functions or user-defined functions
	if standardLibraryName, ok := standardLibrarySymbols[intermediateCodeName]; ok {
		// append the standard library name to the assembly code as an external symbol
		e.assemblyCode.Insert(x64.NewSymbol([]string{standardLibraryName}, x64.FunctionEntry, x64.External))

		switch intermediateCodeName {
		case readStatementSymbol:
			// call read function from the standard library
			e.assemblyCode.AppendInstruction(x64.Call, btLabels, index,
				x64.NewLabelOperand(standardLibraryName)).
				AppendDirective(e.assemblyCode.Location(index, debugger))

			// push the return value from the read function onto the call stack
			// note: the read function returns the read value in the Rax register
			e.assemblyCode.AppendInstruction(x64.Push, nil, index, x64.NewRegisterOperand(x64.Rax))

		case writeStatementSymbol:
			// pop the write value from the call stack as parameter for the write function
			// note: the Rdi register is used for the first argument in the 'write' function call
			e.assemblyCode.AppendInstruction(x64.Pop, btLabels, index,
				x64.NewRegisterOperand(x64.Rdi)).
				AppendDirective(e.assemblyCode.Location(index, debugger))

			// call write function from the standard library
			e.assemblyCode.AppendInstruction(x64.Call, nil, index, x64.NewLabelOperand(standardLibraryName))
		}
	} else {
		// move the difference between use depth and declaration depth as 32-bit signed integer into the R10d register
		e.assemblyCode.AppendInstruction(x64.Mov, btLabels, index,
			x64.NewRegisterOperand(x64.R10d),
			x64.NewImmediateOperand(depthDifference)).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		// push return address on call stack and jump to callee
		e.assemblyCode.AppendInstruction(x64.Call, nil, index, x64.NewLabelOperand(intermediateCodeName))
	}
}

// Return from a function and move the return value into the correct register(s) from top of the call stack.
func (e *emitter) returnFromFunction(dataType ic.DataType, btLabels []string, endOfFunctionLabel string, debugger elf.Debugger, index int) {
	// check whether data type of the return value has modifiers and if so, treat the return value as a pointer or reference
	if dataType.IsPointer() || dataType.IsReference() {
		// pop the top of the call stack into the specified address-type return register
		e.assemblyCode.AppendInstruction(x64.Pop, btLabels, index,
			x64.NewRegisterOperand(addressTypeReturn)).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		// only use labels for the first instruction
		btLabels = nil

		// only emit the debugger prologue end directive for the first instruction
		debugger &^= elf.DebuggerPrologueEnd
	} else if dataType.IsSupported() {
		// if the return value has a supported data type and is not a pointer or reference, it must be moved into the correct register from the call stack
		for _, register := range dataTypeReturn[dataType] {
			switch {
			case register.IsSse() && dataType == ic.Float64:
				// move the top of the call stack into the specified floating-point return register
				e.assemblyCode.AppendInstruction(x64.Movsd, btLabels, index,
					x64.NewRegisterOperand(register),
					x64.NewMemoryOperand(x64.Rsp, x64.Bits64)).
					AppendDirective(e.assemblyCode.Location(index, debugger))

			case register.IsSse() && dataType == ic.Float32:
				// move the top of the call stack into the specified floating-point return register
				e.assemblyCode.AppendInstruction(x64.Movss, btLabels, index,
					x64.NewRegisterOperand(register),
					x64.NewMemoryOperand(x64.Rsp, x64.Bits32)).
					AppendDirective(e.assemblyCode.Location(index, debugger))

			case register.IsGeneralPurpose() && (dataType.IsInteger() || dataType.IsCharacter() || dataType.IsBoolean() || dataType.IsString()):
				// pop the top of the call stack into the specified general-purpose return register
				e.assemblyCode.AppendInstruction(x64.Pop, btLabels, index,
					x64.NewRegisterOperand(register)).
					AppendDirective(e.assemblyCode.Location(index, debugger))

			default:
				// panic if the data type is not supported for the intermediate code operation
				panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeForIntermediateCodeOperation, dataType, nil))
			}

			// only use labels for the first instruction
			btLabels = nil

			// only emit the debugger prologue end directive for the first instruction
			debugger &^= elf.DebuggerPrologueEnd

			// zero out return register, if necessary (typically only for 8-bit and 16-bit integers)
			if zeroMaskReturn[register] != 0 {
				e.assemblyCode.AppendInstruction(x64.And, nil, index,
					x64.NewRegisterOperand(register),
					x64.NewImmediateOperand(zeroMaskReturn[register]))
			}
		}
	} else if !dataType.IsUntyped() {
		// if the data type of the return value is not a pointer, reference, supported data type, or untyped, then it is unexpected
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeForIntermediateCodeOperation, dataType, nil))
	}

	// return from the function
	// note: in the case of the untyped return type (also named void in some languages), return nothing
	e.assemblyCode.AppendInstruction(x64.Ret, btLabels, index).
		AppendDirective(e.assemblyCode.Location(index, debugger)).
		AppendDirective(e.assemblyCode.Filter(elf.NewCfiEndProcedure())).
		AppendDirective(elf.NewSizeStartEndLabel(endOfFunctionLabel))
}

// The function entry sequence is called prologue and prepares the activation record for the function call.
func (e *emitter) prologue(btLabels []string, beginOfFunctionLabel string, debugger elf.Debugger, index int) {
	// save caller's base pointer because it will be changed
	// this creates a 'dynamic link' chain of base pointers so that each callee knows the base pointer of its caller
	// an alternative naming from literature is 'control link' that points to the activation record of the caller
	e.assemblyCode.AppendInstruction(x64.Push, btLabels, index,
		x64.NewRegisterOperand(x64.Rbp)).
		AppendDirective(elf.NewTypeFunction(beginOfFunctionLabel)).
		AppendDirective(e.assemblyCode.Filter(elf.NewCfiStartProcedure())).
		AppendDirective(e.assemblyCode.Location(index, debugger))

	e.assemblyCode.AppendInstruction(x64.Mov, nil, index,
		x64.NewRegisterOperand(x64.Rbp),
		x64.NewRegisterOperand(x64.Rsp)).
		AppendDirective(e.assemblyCode.Filter(elf.NewCfiDefCfaOffset(elf.DwarfCfaOffset))).
		AppendDirective(e.assemblyCode.Filter(elf.NewCfiOffset(x64.Rbp.String(), -elf.DwarfCfaOffset))).
		AppendDirective(e.assemblyCode.Filter(elf.NewCfiDefCfaRegister(x64.Rbp.String())))
}

// The function exit sequence is called epilogue and restores the activation record of the caller.
func (e *emitter) epilogue(btLabels []string, debugger elf.Debugger, index int) {
	// clean allocated local variables from the activation record and restore caller's base pointer
	e.assemblyCode.AppendInstruction(x64.Mov, btLabels, index,
		x64.NewRegisterOperand(x64.Rsp),
		x64.NewRegisterOperand(x64.Rbp)).
		AppendDirective(e.assemblyCode.Location(index, debugger))

	e.assemblyCode.AppendInstruction(x64.Pop, nil, index, x64.NewRegisterOperand(x64.Rbp))
}

// Setup a function call by initializing its memory space and internal data structures.
func (e *emitter) setup(depth int32, btLabels []string, debugger elf.Debugger, index int) {
	// only blocks with a depth greater than 0 have a static link
	// the main block has depth 0, no lexical parent and therefore no static link
	if depth > 0 {
		// call runtime function to create static link which provides the compile-time block nesting hierarchy at runtime
		e.assemblyCode.AppendInstruction(x64.Call, btLabels, index,
			x64.NewLabelOperand(x64.CreateStaticLinkLabel)).
			AppendDirective(e.assemblyCode.Location(index, debugger))
	}
}

// Allocate space for local variables in the activation record of a function and remember their offsets.
func (e *emitter) allocateVariables(iterator ic.Iterator, btLabels []string, debugger elf.Debugger, index int) {
	// group consecutive intermediate code allocate operations into one space allocation instruction
	for j, offset := 0, int32(0); iterator.Peek(j) != nil; j++ {
		if iterator.Peek(j).Quadruple.Operation == ic.AllocateVariable {
			// memory size of the local variable
			var byteSize int32

			// local variable to allocate space for
			result := iterator.Peek(j).Quadruple.Result

			// check whether data type of the local variable has modifiers
			if result.DataType.IsPointer() || result.DataType.IsReference() {
				// pointer or reference data types always have the size of a pointer
				byteSize = x64.PointerSize
			} else {
				// all other data types have a size that is determined by the data type itself
				byteSize = dataTypeSize[result.DataType.AsPlain()]
			}

			// align the offset for the variable's alignment requirement
			alignment := dataTypeAlignment[result.DataType.AsPlain()]
			offset = x64.Align(offset-byteSize, alignment)

			// remember offset of the local variable in its activation record
			e.variableOffsets[result.Name] = offset
		}

		// break if all local variables int the activiation record have been allocated
		if iterator.Peek(j+1) != nil && iterator.Peek(j+1).Quadruple.Operation != ic.AllocateVariable {
			// align the offset and use it as the size required for storing all local variables
			offset = x64.Align(offset, callStackAlignment)

			// grow the call stack downwards to provide space for all local variables int the activiation record (2GB maximum)
			e.assemblyCode.AppendInstruction(x64.Sub, btLabels, index,
				x64.NewRegisterOperand(x64.Rsp),
				x64.NewImmediateOperand(-offset)).
				AppendDirective(e.assemblyCode.Location(index, debugger))

			// zero out the allocated space for local variables in the activation record
			e.assemblyCode.AppendInstruction(x64.Cld, nil, index)
			e.assemblyCode.AppendInstruction(x64.Mov, nil, index, x64.NewRegisterOperand(x64.Rax), x64.NewImmediateOperand(int32(0)))
			e.assemblyCode.AppendInstruction(x64.Mov, nil, index, x64.NewRegisterOperand(x64.Rcx), x64.NewImmediateOperand(-offset/x64.QuadWordSize))
			e.assemblyCode.AppendInstruction(x64.Mov, nil, index, x64.NewRegisterOperand(x64.Rdi), x64.NewRegisterOperand(x64.Rsp))
			e.assemblyCode.AppendPrefixedInstruction(x64.Rep, x64.Stosq, nil, index)

			// set last processed intermediate code instruction and break
			iterator.Skip(j)
			break
		}
	}
}

// Copy an immediate value onto the top of the call stack.
func (e *emitter) copyLiteral(dataType ic.DataType, value any, ldLabel string, btLabels []string, debugger elf.Debugger, index int) {
	// depending on the data type, the value is copied onto the call stack as an immediate value or as a 64-bit value in the R10 register
	switch dataType {
	case ic.Integer64:
		// move the 64-bit signed integer into the R10 register without sign extension
		e.assemblyCode.AppendInstruction(x64.MovAbs, btLabels, index,
			x64.NewRegisterOperand(x64.R10),
			x64.NewImmediateOperand(value.(int64))).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		// push the R10 register onto the call stack
		e.assemblyCode.AppendInstruction(x64.Push, nil, index, x64.NewRegisterOperand(x64.R10))

	case ic.Integer32:
		// push the 32-bit signed integer onto the call stack and sign-extend it to 64 bits
		e.assemblyCode.AppendInstruction(x64.Push, btLabels, index,
			x64.NewImmediateOperand(value.(int32))).
			AppendDirective(e.assemblyCode.Location(index, debugger))

	case ic.Integer16:
		// convert the 16-bit signed integer to a 32-bit signed integer before pushing it onto the call stack and sign-extend it to 64 bits
		e.assemblyCode.AppendInstruction(x64.Push, btLabels, index,
			x64.NewImmediateOperand(int32(value.(int16)))).
			AppendDirective(e.assemblyCode.Location(index, debugger))

	case ic.Integer8:
		// push the 8-bit signed integer onto the call stack and sign-extend it to 64 bits
		e.assemblyCode.AppendInstruction(x64.Push, btLabels, index,
			x64.NewImmediateOperand(value.(int8))).
			AppendDirective(e.assemblyCode.Location(index, debugger))

	case ic.Unsigned64:
		// move the 64-bit unsigned integer into the R10 register without sign extension
		e.assemblyCode.AppendInstruction(x64.MovAbs, btLabels, index,
			x64.NewRegisterOperand(x64.R10),
			x64.NewImmediateOperand(value.(uint64))).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		// push the R10 register onto the call stack
		e.assemblyCode.AppendInstruction(x64.Push, nil, index, x64.NewRegisterOperand(x64.R10))

	case ic.Unsigned32:
		// move the 32-bit unsigned integer into the R10d register and zero-extend to 64 bits
		e.assemblyCode.AppendInstruction(x64.Mov, btLabels, index,
			x64.NewRegisterOperand(x64.R10d),
			x64.NewImmediateOperand(value.(uint32))).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		// push the R10 register onto the call stack
		// note: writing to R10d has already zeroed the upper 32 bits of R10, so pushing R10 pushes the correct zero-extended 64-bit value
		e.assemblyCode.AppendInstruction(x64.Push, nil, index, x64.NewRegisterOperand(x64.R10))

	case ic.Unsigned16:
		// move the 16-bit unsigned integer, converted to 32 bits, into the R10d register and zero-extend to 64 bits
		e.assemblyCode.AppendInstruction(x64.Mov, btLabels, index,
			x64.NewRegisterOperand(x64.R10d),
			x64.NewImmediateOperand(uint32(value.(uint16)))).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		// push the R10 register onto the call stack
		// note: writing to R10d has already zeroed the upper 32 bits of R10, so pushing R10 pushes the correct zero-extended 64-bit value
		e.assemblyCode.AppendInstruction(x64.Push, nil, index, x64.NewRegisterOperand(x64.R10))

	case ic.Unsigned8:
		// move the 8-bit unsigned integer, converted to 32 bits, into the R10d register and zero-extend to 64 bits
		e.assemblyCode.AppendInstruction(x64.Mov, btLabels, index,
			x64.NewRegisterOperand(x64.R10d),
			x64.NewImmediateOperand(uint32(value.(uint8)))).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		// push the R10 register onto the call stack
		// note: writing to R10d has already zeroed the upper 32 bits of R10, so pushing R10 pushes the correct zero-extended 64-bit value
		e.assemblyCode.AppendInstruction(x64.Push, nil, index, x64.NewRegisterOperand(x64.R10))

	case ic.Float64:
		// convert the 64-bit float value to its IEEE 754 binary representation
		binaryRepresentationIEEE754 := math.Float64bits(value.(float64))

		// move the 64-bit float value into the R10 register without any extension
		e.assemblyCode.AppendInstruction(x64.MovAbs, btLabels, index,
			x64.NewRegisterOperand(x64.R10),
			x64.NewImmediateOperand(binaryRepresentationIEEE754)).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		// push the R10 register onto the call stack
		e.assemblyCode.AppendInstruction(x64.Push, nil, index, x64.NewRegisterOperand(x64.R10))

	case ic.Float32:
		// convert the 32-bit float value to its IEEE 754 binary representation
		binaryRepresentationIEEE754 := math.Float32bits(value.(float32))

		// move the 32-bit float value into the lower 32 bits of the R10 register (named R10d) and zero-extend the upper 32 bits
		e.assemblyCode.AppendInstruction(x64.Mov, btLabels, index,
			x64.NewRegisterOperand(x64.R10d),
			x64.NewImmediateOperand(binaryRepresentationIEEE754)).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		// push the 64-bit R10 register onto the call stack (32-bit float value was zero-extended to 64 bits)
		e.assemblyCode.AppendInstruction(x64.Push, nil, index, x64.NewRegisterOperand(x64.R10))

	case ic.Character:
		// convert the Unicode code point to a 32-bit signed integer before pushing it onto the call stack
		e.assemblyCode.AppendInstruction(x64.Push, btLabels, index,
			x64.NewImmediateOperand(int32(value.(rune)))).
			AppendDirective(e.assemblyCode.Location(index, debugger))

	case ic.Boolean:
		// convert the boolean value to an 8-bit unsigned integer before pushing it onto the call stack
		// note: sign extension will have no effect because the boolean value is either 0 or 1
		if value.(bool) {
			e.assemblyCode.AppendInstruction(x64.Push, btLabels, index,
				x64.NewImmediateOperand(uint8(1))).
				AppendDirective(e.assemblyCode.Location(index, debugger))
		} else {
			e.assemblyCode.AppendInstruction(x64.Push, btLabels, index,
				x64.NewImmediateOperand(uint8(0))).
				AppendDirective(e.assemblyCode.Location(index, debugger))
		}

	case ic.String:
		// get the descriptor label from the literal data label
		ldDescriptor := elf.ToDescriptor(ldLabel)

		// calculate the 64-bit unsigned integer length of the string
		runeCount := uint64(utf8.RuneCountInString(value.(string)))

		// append the string value to a read-only data section of the assembly code
		e.assemblyCode.AppendReadOnlyDataItem(elf.ReadOnlyUtf32, []string{ldLabel}, value.(string))

		// append the string descriptor to a read-only data section of the assembly code (string address and length)
		e.assemblyCode.AppendReadOnlyDataItem(elf.ReadOnlyStrDesc, []string{ldDescriptor}, []any{ldLabel, runeCount})

		// load the address of the string descriptor into the Rsi register
		e.assemblyCode.AppendInstruction(x64.Lea, btLabels, index,
			x64.NewRegisterOperand(x64.Rsi),
			x64.NewMemoryOperand(x64.Rip, x64.Bits64, ldDescriptor)).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		// load the address of the string into the R10 register
		// note: the string address is stored at the first 8 bytes of the string descriptor (64-bit address)
		e.assemblyCode.AppendInstruction(x64.Mov, nil, index,
			x64.NewRegisterOperand(x64.R10),
			x64.NewMemoryOperand(x64.Rsi, x64.Bits64))

		// load the length of the string into the R11 register
		// note: the string length is stored at the second 8 bytes of the string descriptor (64-bit unsigned integer)
		e.assemblyCode.AppendInstruction(x64.Mov, nil, index,
			x64.NewRegisterOperand(x64.R11),
			x64.NewMemoryOperand(x64.Rsi, x64.Bits64, x64.PointerSize))

		// push the R11 register onto the call stack (contains the length of the string)
		e.assemblyCode.AppendInstruction(x64.Push, nil, index, x64.NewRegisterOperand(x64.R11))

		// push the R10 register onto the call stack (contains the address of the string)
		e.assemblyCode.AppendInstruction(x64.Push, nil, index, x64.NewRegisterOperand(x64.R10))

	default:
		// panic if the data type is not supported for the intermediate code operation
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeForIntermediateCodeOperation, dataType, nil))
	}
}

// Load a variable's value from its activation record onto the top of the call stack.
// If the variable's data type has the pointer or reference modifier set, the value is loaded as 64-bit address.
func (e *emitter) loadVariable(dataType ic.DataType, offset, depthDifference int32, btLabels []string, debugger elf.Debugger, index int) {
	var basePointer x64.Register

	// determine the correct activation record from which to load the variable
	if depthDifference == 0 {
		// use the variables base pointer of the current activation record
		basePointer = x64.Rbp
	} else {
		// block nesting depth difference between variable use and variable declaration
		e.assemblyCode.AppendInstruction(x64.Mov, btLabels, index,
			x64.NewRegisterOperand(x64.Edi),
			x64.NewImmediateOperand(depthDifference)).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		// follow the static link to determine the 'variables base' pointer of the correct lexical parent activation record
		e.assemblyCode.AppendInstruction(x64.Call, nil, index, x64.NewLabelOperand(x64.FollowStaticLinkLabel))

		// take the variables base pointer from the Rax register that is returned from the runtime function call
		basePointer = x64.Rax

		// only use labels for the first instruction
		btLabels = nil

		// only emit the debugger prologue end directive for the first instruction
		debugger &^= elf.DebuggerPrologueEnd
	}

	// check whether the data type of the variable has modifiers and if so, treat the variable's value as a pointer or reference
	if dataType.IsPointer() || dataType.IsReference() {
		// move the 64-bit address of the variable from the activation record into the R10 register
		e.assemblyCode.AppendInstruction(x64.Mov, btLabels, index,
			x64.NewRegisterOperand(x64.R10),
			x64.NewMemoryOperand(basePointer, x64.Bits64, offset)).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		// push the R10 register onto the call stack
		e.assemblyCode.AppendInstruction(x64.Push, nil, index, x64.NewRegisterOperand(x64.R10))
	} else {
		// depending on the data type, the variable is loaded from the activation record into the R10 register and then pushed onto the call stack
		switch dataType {
		case ic.Integer64, ic.Unsigned64, ic.Float64:
			// move the 64-bit integer/float bitwise from the activation record into the R10 register
			e.assemblyCode.AppendInstruction(x64.Mov, btLabels, index,
				x64.NewRegisterOperand(x64.R10),
				x64.NewMemoryOperand(basePointer, x64.Bits64, offset)).
				AppendDirective(e.assemblyCode.Location(index, debugger))

			// push the R10 register onto the call stack
			e.assemblyCode.AppendInstruction(x64.Push, nil, index, x64.NewRegisterOperand(x64.R10))

		case ic.Integer32, ic.Character:
			// move the 32-bit signed integer/rune from the activation record into the R10 register and sign-extend it to 64 bits
			e.assemblyCode.AppendInstruction(x64.Movsxd, btLabels, index,
				x64.NewRegisterOperand(x64.R10),
				x64.NewMemoryOperand(basePointer, x64.Bits32, offset)).
				AppendDirective(e.assemblyCode.Location(index, debugger))

			// push the R10 register onto the call stack
			e.assemblyCode.AppendInstruction(x64.Push, nil, index, x64.NewRegisterOperand(x64.R10))

		case ic.Unsigned32, ic.Float32:
			// move the 32-bit unsigned integer/float bitwise from the activation record into the R10d register and zero-extend it to 64 bits
			e.assemblyCode.AppendInstruction(x64.Mov, btLabels, index,
				x64.NewRegisterOperand(x64.R10d),
				x64.NewMemoryOperand(basePointer, x64.Bits32, offset)).
				AppendDirective(e.assemblyCode.Location(index, debugger))

			// push the R10 register onto the call stack
			// note: writing to R10d has already zeroed the upper 32 bits of R10, so pushing R10 pushes the correct zero-extended 64-bit value
			e.assemblyCode.AppendInstruction(x64.Push, nil, index, x64.NewRegisterOperand(x64.R10))

		case ic.Integer16:
			// move the 16-bit signed integer from the activation record into the R10 register and sign-extend it to 64 bits
			e.assemblyCode.AppendInstruction(x64.Movsx, btLabels, index,
				x64.NewRegisterOperand(x64.R10),
				x64.NewMemoryOperand(basePointer, x64.Bits16, offset)).
				AppendDirective(e.assemblyCode.Location(index, debugger))

			// push the R10 register onto the call stack
			e.assemblyCode.AppendInstruction(x64.Push, nil, index, x64.NewRegisterOperand(x64.R10))

		case ic.Unsigned16:
			// move the 16-bit unsigned integer from the activation record into the R10d register and zero-extend it to 32 bits
			e.assemblyCode.AppendInstruction(x64.Movzx, btLabels, index,
				x64.NewRegisterOperand(x64.R10d),
				x64.NewMemoryOperand(basePointer, x64.Bits16, offset)).
				AppendDirective(e.assemblyCode.Location(index, debugger))

			// push the R10 register onto the call stack
			// note: writing to R10d has already zeroed the upper 32 bits of R10, so pushing R10 pushes the correct zero-extended 64-bit value
			e.assemblyCode.AppendInstruction(x64.Push, nil, index, x64.NewRegisterOperand(x64.R10))

		case ic.Integer8:
			// move the 8-bit signed integer from the activation record into the R10 register and sign-extend it to 64 bits
			e.assemblyCode.AppendInstruction(x64.Movsx, btLabels, index,
				x64.NewRegisterOperand(x64.R10),
				x64.NewMemoryOperand(basePointer, x64.Bits8, offset)).
				AppendDirective(e.assemblyCode.Location(index, debugger))

			// push the R10 register onto the call stack
			e.assemblyCode.AppendInstruction(x64.Push, nil, index, x64.NewRegisterOperand(x64.R10))

		case ic.Unsigned8, ic.Boolean:
			// move the 8-bit unsigned integer from the activation record into the R10d register and zero-extend it to 32 bits
			e.assemblyCode.AppendInstruction(x64.Movzx, btLabels, index,
				x64.NewRegisterOperand(x64.R10d),
				x64.NewMemoryOperand(basePointer, x64.Bits8, offset)).
				AppendDirective(e.assemblyCode.Location(index, debugger))

			// push the R10 register onto the call stack
			// note: writing to R10d has already zeroed the upper 32 bits of R10, so pushing R10 pushes the correct zero-extended 64-bit value
			e.assemblyCode.AppendInstruction(x64.Push, nil, index, x64.NewRegisterOperand(x64.R10))

		default:
			// panic if the data type is not supported for the intermediate code operation
			panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeForIntermediateCodeOperation, dataType, nil))
		}
	}
}

// Store the top of the call stack as value into a variable's activation record.
// If the variable's data type has the pointer or reference modifier set, the value is assumed to be a 64-bit address and is stored as-is.
func (e *emitter) storeVariable(dataType ic.DataType, offset, depthDifference int32, btLabels []string, debugger elf.Debugger, index int) {
	var basePointer x64.Register

	// determine the correct activation record to which to store the variable
	if depthDifference == 0 {
		// use the variables base pointer of the current activation record
		basePointer = x64.Rbp
	} else {
		// block nesting depth difference between variable use and variable declaration
		e.assemblyCode.AppendInstruction(x64.Mov, btLabels, index,
			x64.NewRegisterOperand(x64.Edi),
			x64.NewImmediateOperand(depthDifference)).
			AppendDirective(e.assemblyCode.Location(index, debugger))

		// follow the static link to determine the 'variables base' pointer of the correct lexical parent activation record
		e.assemblyCode.AppendInstruction(x64.Call, nil, index, x64.NewLabelOperand(x64.FollowStaticLinkLabel))

		// take the variables base pointer from the Rax register that is returned from the runtime function call
		basePointer = x64.Rax

		// only use labels for the first instruction
		btLabels = nil

		// only emit the debugger prologue end directive for the first instruction
		debugger &^= elf.DebuggerPrologueEnd
	}

	// pop the top of the call stack into the R10 register
	e.assemblyCode.AppendInstruction(x64.Pop, btLabels, index,
		x64.NewRegisterOperand(x64.R10)).
		AppendDirective(e.assemblyCode.Location(index, debugger))

	// check whether the data type of the variable has modifiers and if so, treat the variable's value as a pointer or reference
	if dataType.IsPointer() || dataType.IsReference() {
		// move the 64-bit address of the variable from the R10 register into the activation record
		e.assemblyCode.AppendInstruction(x64.Mov, nil, index,
			x64.NewMemoryOperand(basePointer, x64.Bits64, offset),
			x64.NewRegisterOperand(x64.R10))
	} else {
		// depending on the data type, the R10 register is stored into the activation record of the variable
		switch dataType {
		case ic.Integer64, ic.Unsigned64, ic.Float64:
			// move the 64-bit integers/float bitwise from the R10 register into the activation record
			e.assemblyCode.AppendInstruction(x64.Mov, nil, index,
				x64.NewMemoryOperand(basePointer, x64.Bits64, offset),
				x64.NewRegisterOperand(x64.R10))

		case ic.Integer32, ic.Unsigned32, ic.Float32, ic.Character:
			// move the 32-bit signed integer and unsigned integer/rune/float bitwise from the R10d register into the activation record
			e.assemblyCode.AppendInstruction(x64.Mov, nil, index,
				x64.NewMemoryOperand(basePointer, x64.Bits32, offset),
				x64.NewRegisterOperand(x64.R10d))

		case ic.Integer16, ic.Unsigned16:
			// move the 16-bit integers bitwise from the R10w register into the activation record
			e.assemblyCode.AppendInstruction(x64.Mov, nil, index,
				x64.NewMemoryOperand(basePointer, x64.Bits16, offset),
				x64.NewRegisterOperand(x64.R10w))

		case ic.Integer8, ic.Unsigned8, ic.Boolean:
			// move the 8-bit integers/boolean bitwise from the R10b register into the activation record
			e.assemblyCode.AppendInstruction(x64.Mov, nil, index,
				x64.NewMemoryOperand(basePointer, x64.Bits8, offset),
				x64.NewRegisterOperand(x64.R10b))

		default:
			// panic if the data type is not supported for the intermediate code operation
			panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeForIntermediateCodeOperation, dataType, nil))
		}
	}
}
