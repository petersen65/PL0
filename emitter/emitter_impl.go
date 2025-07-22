// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emitter

import (
	"container/list"
	"math"

	cor "github.com/petersen65/PL0/v2/core"
	"github.com/petersen65/PL0/v2/emitter/elf"
	x64 "github.com/petersen65/PL0/v2/emitter/x86_64"
	ic "github.com/petersen65/PL0/v2/generator/intermediate"
)

// Callstack must be aligned to 16 bytes before each function call.
const callStackAlignment = 16

// Sign bit mask for 64-bit double precision floating-point numbers in IEEE 754 binary representation.
const float64SignBitMask uint64 = 0x8000000000000000

// Sign bit mask for 32-bit single precision floating-point numbers in IEEE 754 binary representation.
const float32SignBitMask uint32 = 0x80000000

// Implementation of the assembly code emitter.
type emitter struct {
	intermediateCode ic.IntermediateCodeUnit // intermediate code unit to generate assembly code for
	assemblyCode     x64.AssemblyCodeUnit    // assembly code unit for the target platform
	targetPlatform   cor.TargetPlatform      // target platform for the emitter
	offsetTable      map[string]int32        // 32-bit offset of local variables in their activation record
}

var (
	// Map intermediate code datatypes to their sizes in bytes in the assembly code.
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

	// Map intermediate code datatypes to their alignment in bytes in the assembly code.
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

	// Map intermediate code datatypes to their return registers in the assembly code.
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

	// Map return registers to their zero masks required for zeroing out the upper bits of the register.
	// Note: 0 means that the register is not zeroed out.
	zeroMaskReturn = map[x64.Register]uint64{
		x64.Rax:  0,
		x64.Eax:  0,
		x64.Ax:   0x800000000000FFFF,
		x64.Al:   0x80000000000000FF,
		x64.Xmm0: 0,
		x64.Rdx:  0,
	}
)

// Return the interface of the emitter implementation.
func newEmitter(targetPlatform cor.TargetPlatform, intermediateCodeUnit ic.IntermediateCodeUnit) Emitter {
	if targetPlatform.OperatingSystem != cor.Linux ||
		targetPlatform.InstructionSetArchitecture != cor.X86_64 ||
		targetPlatform.InstructionSet != cor.ISA_SSE2 {
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedTargetPlatform, targetPlatform, nil))
	}

	return &emitter{
		intermediateCode: intermediateCodeUnit,
		assemblyCode:     x64.NewAssemblyCodeUnit(targetPlatform, x64.Application),
		targetPlatform:   targetPlatform,
		offsetTable:      make(map[string]int32),
	}
}

// Emit assembly code for the target platform.
func (e *emitter) Emit() {
	iterator := e.intermediateCode.GetIterator()
	comparison := x64.ComparisonNone

	// compile-time parameters list for function calls
	parameters := list.New()

	// perform an assembly instruction selection for each intermediate code instruction
	for i, l := iterator.First(), make([]string, 0); i != nil; i = iterator.Next() {
		// panic if the intermediate code instruction has not a valid addresses contract
		c := i.Quadruple.ValidateAddressesContract()

		switch i.Quadruple.Operation {
		case ic.BranchTarget: // target for any branching operation
			// append branch target labels for the directly following non 'BranchTarget' instruction
			l = append(l, i.Quadruple.Result.Value.(string))

		case ic.AllocateVariable: // allocate memory for all variables in their logical memory space
			// emit assembly code to allocate space for local variables in the activation record
			e.allocateVariables(iterator, l)

		case ic.Prologue: // function entry sequence
			// emit assembly code to prepare the activation record for the function call
			e.prologue(l)

		case ic.Epilogue: // function exit sequence
			// emit assembly code to restore the activation record of the caller
			e.epilogue(l)

		case ic.Setup: // initialize logical memory space and internal data structures
			// panic if the depth of the block is not a valid signed integer
			depth := i.Quadruple.Arg1.Value.(int32)

			// emit assembly code to setup a function call
			e.setup(depth)

		case ic.CopyLiteral: // copy an immediate value to an address
			// extract the literal value from the quadruple
			literal := i.Quadruple.Arg1.Value

			// extract the data type of the literal value
			dataType := i.Quadruple.Arg1.DataType

			// extract the literal data label from the quadruple
			ldLabel := i.Quadruple.Arg2.Value.(string)

			// emit assembly code to copy the literal onto the top of the call stack
			e.copyLiteral(dataType, literal, ldLabel, l)

		case ic.LoadVariable: // load a variable from its call stack address onto the top of the stack
			// extract the data type of the variable
			dataType := i.Quadruple.Arg1.DataType

			// determine offset of the local variable in its activation record
			offset := e.offsetTable[i.Quadruple.Arg1.Name]

			// extract the depth difference between variable use and variable declaration
			depthDifference := i.Quadruple.Arg2.Value.(int32)

			// emit assembly code to load the variable onto the call stack
			e.loadVariable(dataType, offset, depthDifference, l)

		case ic.StoreVariable: // store the top of the call stack into a variable's stack address
			// extract the data type of the variable
			dataType := i.Quadruple.Result.DataType

			// determine offset of the local variable in its activation record
			offset := e.offsetTable[i.Quadruple.Result.Name]

			// extract the depth difference between variable use and variable declaration
			depthDifference := i.Quadruple.Arg2.Value.(int32)

			// emit assembly code to store the top of the call stack into the variable's activation record
			e.storeVariable(dataType, offset, depthDifference, l)

		case ic.Negate: // negate the top of the call stack and leave the result on the stack
			// extract the data type of the value to negate
			dataType := i.Quadruple.Arg1.DataType

			// emit assembly code to negate the top of the call stack
			e.negate(dataType, l)

		case ic.Odd: // check if the top of the call stack is an odd number and set the Zero Flag (ZF clear if odd, set if even)
			// extract the data type of the value to check for oddness
			dataType := i.Quadruple.Arg1.DataType

			// emit assembly code to check if the top of the call stack is odd
			e.odd(dataType, l)

		case ic.Plus, ic.Minus, ic.Times, ic.Divide: // perform an arithmetic operation on the top two elements of the call stack and replace them with one result on the stack
			// extract the data type of the first value to perform the arithmetic operation on
			dataType := i.Quadruple.Arg1.DataType

			// it is required that the first and second value are of the same data type
			if dataType != i.Quadruple.Arg2.DataType {
				panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeInArithmeticOperation, dataType, nil))
			}

			// emit assembly code to perform the arithmetic operation on the top two elements of the call stack
			if i.Quadruple.Operation == ic.Divide && dataType.IsInteger() {
				e.divideInteger(dataType, l)
			} else {
				e.arithmeticOperation(dataType, i.Quadruple.Operation, l)
			}

		case ic.Equal, ic.NotEqual, ic.Less, ic.LessEqual, ic.Greater, ic.GreaterEqual: // compare the top two elements of the call stack, remove them, and leave the result in the CPU flags register
			// extract the data type of the first value to perform the comparison operation on
			dataType := i.Quadruple.Arg1.DataType

			// it is required that the first and second value are of the same data type
			if dataType != i.Quadruple.Arg2.DataType {
				panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeInComparisonOperation, dataType, nil))
			}

			// emit assembly code to compare the top two elements of the call stack and remember the comparison type
			comparison = e.compare(dataType, l)

		case ic.Jump: // unconditionally jump to a label resolved at link-time
			// panic if the label name is not a string
			name := i.Quadruple.Arg1.Value.(string)

			// emit assembly code to perform an unconditional jump to the specified label
			e.unconditionalJump(name, l)

		case ic.JumpEqual, ic.JumpNotEqual, ic.JumpLess, ic.JumpLessEqual, ic.JumpGreater, ic.JumpGreaterEqual: // conditional jump to a label resolved at link-time based on the CPU flags set by the previous comparison
			// panic if the label name is not a string
			name := i.Quadruple.Arg1.Value.(string)

			// emit assembly code to perform a conditional jump based on the CPU flags set by the previous comparison
			e.conditionalJump(comparison, i.Quadruple.Operation, name, l)

			// reset the comparison type after a conditional jump
			comparison = x64.ComparisonNone

		case ic.Parameter: // push a parameter onto the compile-time parameters list for a function call
			parameters.PushBack(i)

		case ic.Call: // call a function by jumping to the function's label
			// panic if the label name is not a string
			name := i.Quadruple.Arg1.Value.(string)

			// extract the depth difference between function call and function declaration and panic if it is not a signed integer
			depthDifference := i.Quadruple.Arg2.Value.(int32)

			// emit assembly code to call the function with the given name
			e.callFunction(name, depthDifference, l)

		case ic.Return: // return from a function to its caller
			if c.Arg1 == ic.Register {
				// emit assembly code to return from the function with a return value
				e.returnFromFunction(i.Quadruple.Arg1.DataType, l)
			} else {
				// emit assembly code to return from the function without a return value
				e.returnFromFunction(ic.Untyped, l)
			}

		default:
			panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unknownIntermediateCodeOperation, i.Quadruple.Operation, nil))
		}

		// collected branch target labels must be used by the directly following instruction (one instruction consumes all collected labels)
		if i.Quadruple.Operation != ic.BranchTarget {
			l = make([]string, 0)
		}
	}
}

// Get access to the generated assembly code.
func (e *emitter) GetAssemblyCodeUnit() x64.AssemblyCodeUnit {
	return e.assemblyCode
}

// Allocate space for local variables in the activation record of a function and remember their offsets.
func (e *emitter) allocateVariables(iterator ic.Iterator, btLabels []string) {
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
			e.offsetTable[result.Name] = offset
		}

		// break if all local variables int the activiation record have been allocated
		if iterator.Peek(j+1) != nil && iterator.Peek(j+1).Quadruple.Operation != ic.AllocateVariable {
			// align the offset and use it as the size required for storing all local variables
			offset = x64.Align(offset, callStackAlignment)

			// grow the call stack downwards to provide space for all local variables int the activiation record (2GB maximum)
			e.assemblyCode.AppendInstruction(x64.Sub, btLabels,
				x64.NewRegisterOperand(x64.Rsp),
				x64.NewImmediateOperand(-offset))

			// zero out the allocated space for local variables in the activation record
			e.assemblyCode.AppendInstruction(x64.Cld, nil)
			e.assemblyCode.AppendInstruction(x64.Mov, nil, x64.NewRegisterOperand(x64.Rax), x64.NewImmediateOperand(int32(0)))
			e.assemblyCode.AppendInstruction(x64.Mov, nil, x64.NewRegisterOperand(x64.Rcx), x64.NewImmediateOperand(-offset/x64.QuadWordSize))
			e.assemblyCode.AppendInstruction(x64.Mov, nil, x64.NewRegisterOperand(x64.Rdi), x64.NewRegisterOperand(x64.Rsp))
			e.assemblyCode.AppendPrefixedInstruction(x64.Rep, x64.Stosq, nil)

			// set last processed intermediate code instruction and break
			iterator.Skip(j)
			break
		}
	}
}

// The function entry sequence is called prologue and prepares the activation record for the function call.
func (e *emitter) prologue(labels []string) {
	// save caller's base pointer because it will be changed
	// this creates a 'dynamic link' chain of base pointers so that each callee knows the base pointer of its caller
	// an alternative naming from literature is 'control link' that points to the activation record of the caller
	e.assemblyCode.AppendInstruction(x64.Push, labels, x64.NewRegisterOperand(x64.Rbp))
	e.assemblyCode.AppendInstruction(x64.Mov, nil, x64.NewRegisterOperand(x64.Rbp), x64.NewRegisterOperand(x64.Rsp))
}

// The function exit sequence is called epilogue and restores the activation record of the caller.
func (e *emitter) epilogue(labels []string) {
	// clean allocated local variables from the activation record and restore caller's base pointer
	e.assemblyCode.AppendInstruction(x64.Mov, labels, x64.NewRegisterOperand(x64.Rsp), x64.NewRegisterOperand(x64.Rbp))
	e.assemblyCode.AppendInstruction(x64.Pop, nil, x64.NewRegisterOperand(x64.Rbp))
}

// Setup a function call by initializing the logical memory space and internal data structures.
func (e *emitter) setup(depth int32) {
	// only blocks with a depth greater than 0 have a static link
	// the main block has depth 0, no lexical parent and therefore no static link
	if depth > 0 {
		// call runtime function to create static link which provides the compile-time block nesting hierarchy at runtime
		e.assemblyCode.AppendInstruction(x64.Call, nil, x64.NewLabelOperand(x64.CreateStaticLinkLabel))
	}
}

// Copy an immediate value onto the top of the call stack.
func (e *emitter) copyLiteral(dataType ic.DataType, value any, ldLabel string, btLabels []string) {
	// depending on the data type, the value is copied onto the call stack as an immediate value or as a 64-bit value in the R10 register
	switch dataType {
	case ic.Integer64:
		// move the 64-bit signed integer into the R10 register without sign extension
		e.assemblyCode.AppendInstruction(x64.MovAbs, btLabels,
			x64.NewRegisterOperand(x64.R10),
			x64.NewImmediateOperand(value.(int64)))

		// push the R10 register onto the call stack
		e.assemblyCode.AppendInstruction(x64.Push, nil, x64.NewRegisterOperand(x64.R10))

	case ic.Integer32:
		// push the 32-bit signed integer onto the call stack and sign-extend it to 64 bits
		e.assemblyCode.AppendInstruction(x64.Push, btLabels, x64.NewImmediateOperand(value.(int32)))

	case ic.Integer16:
		// convert the 16-bit signed integer to a 32-bit signed integer before pushing it onto the call stack and sign-extend it to 64 bits
		e.assemblyCode.AppendInstruction(x64.Push, btLabels, x64.NewImmediateOperand(int32(value.(int16))))

	case ic.Integer8:
		// push the 8-bit signed integer onto the call stack and sign-extend it to 64 bits
		e.assemblyCode.AppendInstruction(x64.Push, btLabels, x64.NewImmediateOperand(value.(int8)))

	case ic.Unsigned64:
		// move the 64-bit unsigned integer into the R10 register without sign extension
		e.assemblyCode.AppendInstruction(x64.MovAbs, btLabels,
			x64.NewRegisterOperand(x64.R10),
			x64.NewImmediateOperand(value.(uint64)))

		// push the R10 register onto the call stack
		e.assemblyCode.AppendInstruction(x64.Push, nil, x64.NewRegisterOperand(x64.R10))

	case ic.Unsigned32:
		// move the 32-bit unsigned integer into the R10d register and zero-extend to 64 bits
		e.assemblyCode.AppendInstruction(x64.Mov, btLabels,
			x64.NewRegisterOperand(x64.R10d),
			x64.NewImmediateOperand(value.(uint32)))

		// push the R10 register onto the call stack
		// note: writing to R10d has already zeroed the upper 32 bits of R10, so pushing R10 pushes the correct zero-extended 64-bit value
		e.assemblyCode.AppendInstruction(x64.Push, nil, x64.NewRegisterOperand(x64.R10))

	case ic.Unsigned16:
		// move the 16-bit unsigned integer, converted to 32 bits, into the R10d register and zero-extend to 64 bits
		e.assemblyCode.AppendInstruction(x64.Mov, btLabels,
			x64.NewRegisterOperand(x64.R10d),
			x64.NewImmediateOperand(uint32(value.(uint16))))

		// push the R10 register onto the call stack
		// note: writing to R10d has already zeroed the upper 32 bits of R10, so pushing R10 pushes the correct zero-extended 64-bit value
		e.assemblyCode.AppendInstruction(x64.Push, nil, x64.NewRegisterOperand(x64.R10))

	case ic.Unsigned8:
		// move the 8-bit unsigned integer, converted to 32 bits, into the R10d register and zero-extend to 64 bits
		e.assemblyCode.AppendInstruction(x64.Mov, btLabels,
			x64.NewRegisterOperand(x64.R10d),
			x64.NewImmediateOperand(uint32(value.(uint8))))

		// push the R10 register onto the call stack
		// note: writing to R10d has already zeroed the upper 32 bits of R10, so pushing R10 pushes the correct zero-extended 64-bit value
		e.assemblyCode.AppendInstruction(x64.Push, nil, x64.NewRegisterOperand(x64.R10))

	case ic.Float64:
		// convert the 64-bit float value to its IEEE 754 binary representation
		binaryRepresentationIEEE754 := math.Float64bits(value.(float64))

		// move the 64-bit float value into the R10 register without any extension
		e.assemblyCode.AppendInstruction(x64.MovAbs, btLabels,
			x64.NewRegisterOperand(x64.R10),
			x64.NewImmediateOperand(binaryRepresentationIEEE754))

		// push the R10 register onto the call stack
		e.assemblyCode.AppendInstruction(x64.Push, nil, x64.NewRegisterOperand(x64.R10))

	case ic.Float32:
		// convert the 32-bit float value to its IEEE 754 binary representation
		binaryRepresentationIEEE754 := math.Float32bits(value.(float32))

		// move the 32-bit float value into the lower 32 bits of the R10 register (named R10d) and zero-extend the upper 32 bits
		e.assemblyCode.AppendInstruction(x64.Mov, btLabels,
			x64.NewRegisterOperand(x64.R10d),
			x64.NewImmediateOperand(binaryRepresentationIEEE754))

		// push the 64-bit R10 register onto the call stack (32-bit float value was zero-extended to 64 bits)
		e.assemblyCode.AppendInstruction(x64.Push, nil, x64.NewRegisterOperand(x64.R10))

	case ic.Character:
		// convert the Unicode code point to a 32-bit signed integer before pushing it onto the call stack
		e.assemblyCode.AppendInstruction(x64.Push, btLabels, x64.NewImmediateOperand(int32(value.(rune))))

	case ic.Boolean:
		// convert the boolean value to an 8-bit unsigned integer before pushing it onto the call stack
		// note: sign extension will have no effect because the boolean value is either 0 or 1
		if value.(bool) {
			e.assemblyCode.AppendInstruction(x64.Push, btLabels, x64.NewImmediateOperand(uint8(1)))
		} else {
			e.assemblyCode.AppendInstruction(x64.Push, btLabels, x64.NewImmediateOperand(uint8(0)))
		}

	case ic.String:
		e.assemblyCode.AppendReadOnlyDataItem(elf.ReadOnlyUtf32, []string{ldLabel}, value.(string))

	default:
		// panic if the data type is not supported for the intermediate code operation
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeForIntermediateCodeOperation, dataType, nil))
	}
}

// Load a variable from its activation record onto the top of the call stack.
func (e *emitter) loadVariable(dataType ic.DataType, offset, depthDifference int32, labels []string) {
	var basePointer x64.Register

	// determine the correct activation record from which to load the variable
	if depthDifference == 0 {
		// use the variables base pointer of the current activation record
		basePointer = x64.Rbp
	} else {
		// block nesting depth difference between variable use and variable declaration
		e.assemblyCode.AppendInstruction(x64.Mov, labels,
			x64.NewRegisterOperand(x64.Edi),
			x64.NewImmediateOperand(depthDifference))

		// follow the static link to determine the 'variables base' pointer of the correct lexical parent activation record
		e.assemblyCode.AppendInstruction(x64.Call, nil, x64.NewLabelOperand(x64.FollowStaticLinkLabel))

		// take the variables base pointer from the Rax register that is returned from the runtime function call
		labels = nil
		basePointer = x64.Rax
	}

	// depending on the data type, the variable is loaded from the activation record into the R10 register and then pushed onto the call stack
	switch dataType {
	case ic.Integer64, ic.Unsigned64, ic.Float64:
		// move the 64-bit integer/float bitwise from the activation record into the R10 register
		e.assemblyCode.AppendInstruction(x64.Mov, labels,
			x64.NewRegisterOperand(x64.R10),
			x64.NewMemoryOperand(basePointer, x64.Bits64, offset))

		// push the R10 register onto the call stack
		e.assemblyCode.AppendInstruction(x64.Push, nil, x64.NewRegisterOperand(x64.R10))

	case ic.Integer32, ic.Character:
		// move the 32-bit signed integer/rune from the activation record into the R10 register and sign-extend it to 64 bits
		e.assemblyCode.AppendInstruction(x64.Movsxd, labels,
			x64.NewRegisterOperand(x64.R10),
			x64.NewMemoryOperand(basePointer, x64.Bits32, offset))

		// push the R10 register onto the call stack
		e.assemblyCode.AppendInstruction(x64.Push, nil, x64.NewRegisterOperand(x64.R10))

	case ic.Unsigned32, ic.Float32:
		// move the 32-bit unsigned integer/float bitwise from the activation record into the R10d register and zero-extend it to 64 bits
		e.assemblyCode.AppendInstruction(x64.Mov, labels,
			x64.NewRegisterOperand(x64.R10d),
			x64.NewMemoryOperand(basePointer, x64.Bits32, offset))

		// push the R10 register onto the call stack
		// note: writing to R10d has already zeroed the upper 32 bits of R10, so pushing R10 pushes the correct zero-extended 64-bit value
		e.assemblyCode.AppendInstruction(x64.Push, nil, x64.NewRegisterOperand(x64.R10))

	case ic.Integer16:
		// move the 16-bit signed integer from the activation record into the R10 register and sign-extend it to 64 bits
		e.assemblyCode.AppendInstruction(x64.Movsx, labels,
			x64.NewRegisterOperand(x64.R10),
			x64.NewMemoryOperand(basePointer, x64.Bits16, offset))

		// push the R10 register onto the call stack
		e.assemblyCode.AppendInstruction(x64.Push, nil, x64.NewRegisterOperand(x64.R10))

	case ic.Unsigned16:
		// move the 16-bit unsigned integer from the activation record into the R10d register and zero-extend it to 32 bits
		e.assemblyCode.AppendInstruction(x64.Movzx, labels,
			x64.NewRegisterOperand(x64.R10d),
			x64.NewMemoryOperand(basePointer, x64.Bits16, offset))

		// push the R10 register onto the call stack
		// note: writing to R10d has already zeroed the upper 32 bits of R10, so pushing R10 pushes the correct zero-extended 64-bit value
		e.assemblyCode.AppendInstruction(x64.Push, nil, x64.NewRegisterOperand(x64.R10))

	case ic.Integer8:
		// move the 8-bit signed integer from the activation record into the R10 register and sign-extend it to 64 bits
		e.assemblyCode.AppendInstruction(x64.Movsx, labels,
			x64.NewRegisterOperand(x64.R10),
			x64.NewMemoryOperand(basePointer, x64.Bits8, offset))

		// push the R10 register onto the call stack
		e.assemblyCode.AppendInstruction(x64.Push, nil, x64.NewRegisterOperand(x64.R10))

	case ic.Unsigned8, ic.Boolean:
		// move the 8-bit unsigned integer from the activation record into the R10d register and zero-extend it to 32 bits
		e.assemblyCode.AppendInstruction(x64.Movzx, labels,
			x64.NewRegisterOperand(x64.R10d),
			x64.NewMemoryOperand(basePointer, x64.Bits8, offset))

		// push the R10 register onto the call stack
		// note: writing to R10d has already zeroed the upper 32 bits of R10, so pushing R10 pushes the correct zero-extended 64-bit value
		e.assemblyCode.AppendInstruction(x64.Push, nil, x64.NewRegisterOperand(x64.R10))

	default:
		// panic if the data type is not supported for the intermediate code operation
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeForIntermediateCodeOperation, dataType, nil))
	}
}

// Store the top of the call stack into a variable's activation record.
func (e *emitter) storeVariable(dataType ic.DataType, offset, depthDifference int32, labels []string) {
	var basePointer x64.Register

	// determine the correct activation record to which to store the variable
	if depthDifference == 0 {
		// use the variables base pointer of the current activation record
		basePointer = x64.Rbp
	} else {
		// block nesting depth difference between variable use and variable declaration
		e.assemblyCode.AppendInstruction(x64.Mov, labels,
			x64.NewRegisterOperand(x64.Edi),
			x64.NewImmediateOperand(depthDifference))

		// follow the static link to determine the 'variables base' pointer of the correct lexical parent activation record
		e.assemblyCode.AppendInstruction(x64.Call, nil, x64.NewLabelOperand(x64.FollowStaticLinkLabel))

		// take the variables base pointer from the Rax register that is returned from the runtime function call
		labels = nil
		basePointer = x64.Rax
	}

	// pop the top of the call stack into the R10 register
	e.assemblyCode.AppendInstruction(x64.Pop, labels, x64.NewRegisterOperand(x64.R10))

	// depending on the data type, the R10 register is stored into the activation record of the variable
	switch dataType {
	case ic.Integer64, ic.Unsigned64, ic.Float64:
		// move the 64-bit integers/float bitwise from the R10 register into the activation record
		e.assemblyCode.AppendInstruction(x64.Mov, nil,
			x64.NewMemoryOperand(basePointer, x64.Bits64, offset),
			x64.NewRegisterOperand(x64.R10))

	case ic.Integer32, ic.Unsigned32, ic.Float32, ic.Character:
		// move the 32-bit signed integer and unsigned integer/rune/float bitwise from the R10d register into the activation record
		e.assemblyCode.AppendInstruction(x64.Mov, nil,
			x64.NewMemoryOperand(basePointer, x64.Bits32, offset),
			x64.NewRegisterOperand(x64.R10d))

	case ic.Integer16, ic.Unsigned16:
		// move the 16-bit integers bitwise from the R10w register into the activation record
		e.assemblyCode.AppendInstruction(x64.Mov, nil,
			x64.NewMemoryOperand(basePointer, x64.Bits16, offset),
			x64.NewRegisterOperand(x64.R10w))

	case ic.Integer8, ic.Unsigned8, ic.Boolean:
		// move the 8-bit integers/boolean bitwise from the R10b register into the activation record
		e.assemblyCode.AppendInstruction(x64.Mov, nil,
			x64.NewMemoryOperand(basePointer, x64.Bits8, offset),
			x64.NewRegisterOperand(x64.R10b))

	default:
		// panic if the data type is not supported for the intermediate code operation
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeForIntermediateCodeOperation, dataType, nil))
	}
}

// Negate the top of the call stack and leave the result on top of the stack.
func (e *emitter) negate(dataType ic.DataType, labels []string) {
	// depending on the data type, the top of the call stack is popped into the correct register and negated
	switch dataType {
	case ic.Integer64, ic.Integer32, ic.Integer16, ic.Integer8:
		// all integer 64-bit, 32-bit, 16-bit and 8-bit values are negated the same way
		// note: all integer values must be correctly sign-extended to 64 bits before negation
		e.assemblyCode.AppendInstruction(x64.Pop, labels, x64.NewRegisterOperand(x64.R10))
		e.assemblyCode.AppendInstruction(x64.Neg, nil, x64.NewRegisterOperand(x64.R10))
		e.assemblyCode.AppendInstruction(x64.Push, nil, x64.NewRegisterOperand(x64.R10))

	case ic.Float64:
		// move the 64-bit floating-point value (IEEE 754 double precision) to the XMM0 register
		e.assemblyCode.AppendInstruction(x64.Movsd, labels,
			x64.NewRegisterOperand(x64.Xmm0),
			x64.NewMemoryOperand(x64.Rsp, x64.Bits64, 0))

		// move the 64-bit floating-point sign bit mask into the R10 register
		e.assemblyCode.AppendInstruction(x64.Mov, nil,
			x64.NewRegisterOperand(x64.R10),
			x64.NewImmediateOperand(float64SignBitMask))

		// move the 64-bit floating-point sign bit mask into the XMM1 register
		e.assemblyCode.AppendInstruction(x64.Movq, nil,
			x64.NewRegisterOperand(x64.Xmm1),
			x64.NewRegisterOperand(x64.R10))

		// perform a bitwise XOR operation between the XMM0 register and the XMM1 register
		// note: this flips the sign bit of the floating-point value in the XMM0 register
		e.assemblyCode.AppendInstruction(x64.Xorpd, nil,
			x64.NewRegisterOperand(x64.Xmm0),
			x64.NewRegisterOperand(x64.Xmm1))

		// move the negated 64-bit floating-point value back onto the top of the call stack
		e.assemblyCode.AppendInstruction(x64.Movsd, nil,
			x64.NewMemoryOperand(x64.Rsp, x64.Bits64, 0),
			x64.NewRegisterOperand(x64.Xmm0))

	case ic.Float32:
		// move the 32-bit floating-point value (IEEE 754 single precision) to the XMM0 register
		// note: the Bits32 is redundant here because the 'Movss' instruction always expects a 32-bit value
		e.assemblyCode.AppendInstruction(x64.Movss, labels,
			x64.NewRegisterOperand(x64.Xmm0),
			x64.NewMemoryOperand(x64.Rsp, x64.Bits32, 0))

		// move the 32-bit floating-point sign bit mask into the R10d register
		e.assemblyCode.AppendInstruction(x64.Mov, nil,
			x64.NewRegisterOperand(x64.R10d),
			x64.NewImmediateOperand(float32SignBitMask))

		// move the 32-bit floating-point sign bit mask into the XMM1 register
		e.assemblyCode.AppendInstruction(x64.Movd, nil,
			x64.NewRegisterOperand(x64.Xmm1),
			x64.NewRegisterOperand(x64.R10d))

		// perform a bitwise XOR operation between the XMM0 register and the XMM1 register
		// note: this flips the sign bit of the single precision floating-point value in the XMM0 register
		e.assemblyCode.AppendInstruction(x64.Xorps, nil,
			x64.NewRegisterOperand(x64.Xmm0),
			x64.NewRegisterOperand(x64.Xmm1))

		// move the negated 32-bit floating-point value back onto the top of the call stack
		e.assemblyCode.AppendInstruction(x64.Movss, nil,
			x64.NewMemoryOperand(x64.Rsp, x64.Bits32, 0),
			x64.NewRegisterOperand(x64.Xmm0))

		// clear the upper 32 bits from [RSP] to maintain a clean 64-bit call stack slot
		e.assemblyCode.AppendInstruction(x64.Mov, nil,
			x64.NewMemoryOperand(x64.Rsp, x64.Bits32, x64.DoubleWordSize),
			x64.NewImmediateOperand(uint32(0)))

	default:
		// panic if the data type is not supported for the intermediate code operation
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeForIntermediateCodeOperation, dataType, nil))
	}
}

// Check if the top of the call stack is odd depending on the data type.
func (e *emitter) odd(dataType ic.DataType, labels []string) {
	// depending on the data type, the top of the call stack is popped into the correct register and checked for oddness
	switch dataType {
	case ic.Integer64, ic.Integer32, ic.Integer16, ic.Integer8:
		// all integer 64-bit, 32-bit, 16-bit and 8-bit values are checked for oddness the same way
		// note: all integer values must be correctly sign-extended to 64 bits before checking if they are odd
		e.assemblyCode.AppendInstruction(x64.Pop, labels, x64.NewRegisterOperand(x64.R10))

		// test the least significant bit to determine oddness (64-bit 'Test' instruction is sufficient)
		e.assemblyCode.AppendInstruction(x64.Test, nil, x64.NewRegisterOperand(x64.R10), x64.NewImmediateOperand(uint64(1)))

	default:
		// panic if the data type is not supported for the intermediate code operation
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeForIntermediateCodeOperation, dataType, nil))
	}
}

// Perform the arithmetic operations 'Plus', 'Minus', 'Times' on the top two elements of the call stack and replace them with one result on the stack.
func (e *emitter) arithmeticOperation(dataType ic.DataType, operation ic.Operation, labels []string) {
	// depending on the data type, the top two elements of the call stack are popped into the correct registers and the arithmetic operation is performed
	switch dataType {
	case ic.Integer64, ic.Integer32, ic.Integer16, ic.Integer8, ic.Unsigned64, ic.Unsigned32, ic.Unsigned16, ic.Unsigned8:
		// pop the right-hand integer value from the call stack into the R11 register
		// note: all integer values must be correctly sign-extended to 64 bits before arithmetic operations
		e.assemblyCode.AppendInstruction(x64.Pop, labels, x64.NewRegisterOperand(x64.R11))

		// pop the left-hand integer value from the call stack into the R10 register
		// note: all integer values must be correctly sign-extended to 64 bits before arithmetic operations
		e.assemblyCode.AppendInstruction(x64.Pop, nil, x64.NewRegisterOperand(x64.R10))

		// the R10 and R11 registers are used to perform the arithmetic operation which leaves the result in the R10 register
		switch operation {
		case ic.Plus:
			// add the right-hand integer value in the R11 register to the left-hand integer value in the R10 register
			e.assemblyCode.AppendInstruction(x64.Add, nil, x64.NewRegisterOperand(x64.R10), x64.NewRegisterOperand(x64.R11))

		case ic.Minus:
			// subtract the right-hand integer value in the R11 register from the left-hand integer value in the R10 register
			e.assemblyCode.AppendInstruction(x64.Sub, nil, x64.NewRegisterOperand(x64.R10), x64.NewRegisterOperand(x64.R11))

		case ic.Times:
			// multiply the left-hand integer value in the R10 register with the right-hand integer value in the R11 register
			e.assemblyCode.AppendInstruction(x64.Imul, nil, x64.NewRegisterOperand(x64.R10), x64.NewRegisterOperand(x64.R11))
		}

		// push the result of the arithmetic operation onto the call stack
		// note: flags from the 'Add', 'Sub', and 'Imul' instructions are intentionally ignored and will be overwritten by the 'Push' instruction
		e.assemblyCode.AppendInstruction(x64.Push, nil, x64.NewRegisterOperand(x64.R10))

	case ic.Float64:
		// move the right-hand 64-bit floating-point value (IEEE 754 double precision) from the call stack into the XMM1 register
		e.assemblyCode.AppendInstruction(x64.Movsd, labels,
			x64.NewRegisterOperand(x64.Xmm1),
			x64.NewMemoryOperand(x64.Rsp, x64.Bits64, 0))

		// move the left-hand 64-bit floating-point value (IEEE 754 double precision) from the call stack into the XMM0 register
		e.assemblyCode.AppendInstruction(x64.Movsd, nil,
			x64.NewRegisterOperand(x64.Xmm0),
			x64.NewMemoryOperand(x64.Rsp, x64.Bits64, x64.PointerSize))

		// the XMM0 and XMM1 registers are used to perform the arithmetic operation which leaves the result in the XMM0 register
		switch operation {
		case ic.Plus:
			// add the right-hand floating-point value in the XMM1 register to the left-hand floating-point value in the XMM0 register
			e.assemblyCode.AppendInstruction(x64.Addsd, nil,
				x64.NewRegisterOperand(x64.Xmm0),
				x64.NewRegisterOperand(x64.Xmm1))

		case ic.Minus:
			// subtract the right-hand floating-point value in the XMM1 register from the left-hand floating-point value in the XMM0 register
			e.assemblyCode.AppendInstruction(x64.Subsd, nil,
				x64.NewRegisterOperand(x64.Xmm0),
				x64.NewRegisterOperand(x64.Xmm1))

		case ic.Times:
			// multiply the right-hand floating-point value in the XMM1 register with the left-hand floating-point value in the XMM0 register
			e.assemblyCode.AppendInstruction(x64.Mulsd, nil,
				x64.NewRegisterOperand(x64.Xmm0),
				x64.NewRegisterOperand(x64.Xmm1))

		case ic.Divide:
			// divide the left-hand floating-point value in the XMM0 register by the right-hand floating-point value in the XMM1 register
			e.assemblyCode.AppendInstruction(x64.Divsd, nil,
				x64.NewRegisterOperand(x64.Xmm0),
				x64.NewRegisterOperand(x64.Xmm1))
		}

		// remove the top element of the call stack (the right-hand value) by adjusting the stack pointer by 1 times the pointer size
		e.assemblyCode.AppendInstruction(x64.Add, nil,
			x64.NewRegisterOperand(x64.Rsp),
			x64.NewImmediateOperand(int32(x64.PointerSize)))

		// move the result of the arithmetic operation back onto the top of the call stack, overwriting the left-hand value
		e.assemblyCode.AppendInstruction(x64.Movsd, nil,
			x64.NewMemoryOperand(x64.Rsp, x64.Bits64, 0),
			x64.NewRegisterOperand(x64.Xmm0))

	case ic.Float32:
		// move the right-hand 32-bit floating-point value (IEEE 754 single precision) from the call stack into the XMM1 register
		e.assemblyCode.AppendInstruction(x64.Movss, labels,
			x64.NewRegisterOperand(x64.Xmm1),
			x64.NewMemoryOperand(x64.Rsp, x64.Bits32, 0))

		// move the left-hand 32-bit floating-point value (IEEE 754 single precision) from the call stack into the XMM0 register
		e.assemblyCode.AppendInstruction(x64.Movss, nil,
			x64.NewRegisterOperand(x64.Xmm0),
			x64.NewMemoryOperand(x64.Rsp, x64.Bits32, x64.PointerSize))

		// the XMM0 and XMM1 registers are used to perform the arithmetic operation which leaves the result in the XMM0 register
		switch operation {
		case ic.Plus:
			// add the right-hand floating-point value in the XMM1 register to the left-hand floating-point value in the XMM0 register
			e.assemblyCode.AppendInstruction(x64.Addss, nil,
				x64.NewRegisterOperand(x64.Xmm0),
				x64.NewRegisterOperand(x64.Xmm1))

		case ic.Minus:
			// subtract the right-hand floating-point value in the XMM1 register from the left-hand floating-point value in the XMM0 register
			e.assemblyCode.AppendInstruction(x64.Subss, nil,
				x64.NewRegisterOperand(x64.Xmm0),
				x64.NewRegisterOperand(x64.Xmm1))

		case ic.Times:
			// multiply the right-hand floating-point value in the XMM1 register with the left-hand floating-point value in the XMM0 register
			e.assemblyCode.AppendInstruction(x64.Mulss, nil,
				x64.NewRegisterOperand(x64.Xmm0),
				x64.NewRegisterOperand(x64.Xmm1))

		case ic.Divide:
			// divide the left-hand floating-point value in the XMM0 register by the right-hand floating-point value in the XMM1 register
			e.assemblyCode.AppendInstruction(x64.Divss, nil,
				x64.NewRegisterOperand(x64.Xmm0),
				x64.NewRegisterOperand(x64.Xmm1))
		}

		// remove the top element of the call stack (the right-hand value) by adjusting the stack pointer by 1 times the pointer size
		e.assemblyCode.AppendInstruction(x64.Add, nil,
			x64.NewRegisterOperand(x64.Rsp),
			x64.NewImmediateOperand(int32(x64.PointerSize)))

		// // move the result of the arithmetic operation back onto the top of the call stack and overwrite the left-hand value's lower 32 bits
		e.assemblyCode.AppendInstruction(x64.Movss, nil,
			x64.NewMemoryOperand(x64.Rsp, x64.Bits32, 0),
			x64.NewRegisterOperand(x64.Xmm0))

		// clear the upper 32 bits from [RSP] to maintain a clean 64-bit call stack slot
		e.assemblyCode.AppendInstruction(x64.Mov, nil,
			x64.NewMemoryOperand(x64.Rsp, x64.Bits32, x64.DoubleWordSize),
			x64.NewImmediateOperand(uint32(0)))

	default:
		// panic if the data type is not supported for the intermediate code operation
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeForIntermediateCodeOperation, dataType, nil))
	}
}

// Perform the arithmetic operation 'Divide' on the top two elements of the call stack and replace them with one result on the stack.
func (e *emitter) divideInteger(dataType ic.DataType, labels []string) {
	// depending on the data type, the top two elements of the call stack are popped into the correct registers and the division operation is performed
	switch dataType {
	case ic.Integer64, ic.Integer32, ic.Integer16, ic.Integer8:
		// pop the right-hand integer value (divisor) from the call stack into the R11 register
		// note: all integer values must be correctly sign-extended to 64 bits before arithmetic operations
		e.assemblyCode.AppendInstruction(x64.Pop, labels, x64.NewRegisterOperand(x64.R11))

		// pop the left-hand integer value (dividend) from the call stack into the RAX register (required by IDIV)
		// note: all integer values must be correctly sign-extended to 64 bits before arithmetic operations
		e.assemblyCode.AppendInstruction(x64.Pop, nil, x64.NewRegisterOperand(x64.Rax))

		// sign-extend RAX to RDX:RAX by using the 'Cqo' instruction for a signed division (convert quadword to octword)
		// note: CQO operates on 64-bit operands but is safe to use with smaller types, as long as the value in RAX is properly sign-extended to 64 bits beforehand
		e.assemblyCode.AppendInstruction(x64.Cqo, nil)

		// divide the 128-bit dividend in RDX:RAX by the 64-bit divisor in R11 (signed division)
		// note: the quotient is stored in RAX and the remainder in RDX
		e.assemblyCode.AppendInstruction(x64.Idiv, nil, x64.NewRegisterOperand(x64.R11))

		// push the result onto the stack (quotient)
		e.assemblyCode.AppendInstruction(x64.Push, nil, x64.NewRegisterOperand(x64.Rax))

	case ic.Unsigned64, ic.Unsigned32, ic.Unsigned16, ic.Unsigned8:
		// pop the right-hand integer value (divisor) from the call stack into the R11 register
		// note: all unsigned integer values must be correctly zero-extended to 64 bits before arithmetic operations
		e.assemblyCode.AppendInstruction(x64.Pop, labels, x64.NewRegisterOperand(x64.R11))

		// pop the left-hand integer value (dividend) from the call stack into the RAX register (required by DIV)
		// note: all unsigned integer values must be correctly zero-extended to 64 bits before arithmetic operations
		e.assemblyCode.AppendInstruction(x64.Pop, nil, x64.NewRegisterOperand(x64.Rax))

		// zero-extend RAX to RDX:RAX by using the 'Xor' instruction for an unsigned division (clear RDX)
		e.assemblyCode.AppendInstruction(x64.Xor, nil, x64.NewRegisterOperand(x64.Rdx), x64.NewRegisterOperand(x64.Rdx))

		// divide the 128-bit dividend in RDX:RAX by the 64-bit divisor in R11 (unsigned division)
		// note: the quotient is stored in RAX and the remainder in RDX
		e.assemblyCode.AppendInstruction(x64.Div, nil, x64.NewRegisterOperand(x64.R11))

		// push the result onto the stack (quotient)
		e.assemblyCode.AppendInstruction(x64.Push, nil, x64.NewRegisterOperand(x64.Rax))

	default:
		// panic if the data type is not supported for the intermediate code operation
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeForIntermediateCodeOperation, dataType, nil))
	}
}

// Compare the top two elements of the call stack and update the CPU flags.
func (e *emitter) compare(dataType ic.DataType, labels []string) x64.ComparisonType {
	var comparisonType x64.ComparisonType

	// depending on the data type, evaluate the comparison type for the conditional jump instructions
	// note: the comparison type is used to determine how the CPU flags are interpreted by following conditional jump instructions
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

	// depending on the data type, the top two elements of the call stack are popped into the correct registers and compared
	switch dataType {
	case ic.Integer64, ic.Integer32, ic.Integer16, ic.Integer8, ic.Character, ic.Unsigned64, ic.Unsigned32, ic.Unsigned16, ic.Unsigned8, ic.Boolean:
		// pop the right-hand integer value from the call stack into the R11 register
		// note: all integer values must be extended to 64 bits before arithmetic operations
		//       - sign-extended for signed integers (includes Unicode)
		//       - zero-extended for unsigned integers (includes Boolean)
		e.assemblyCode.AppendInstruction(x64.Pop, labels, x64.NewRegisterOperand(x64.R11))

		// pop the left-hand integer value from the call stack into the R10 register
		// note: all integer values must be extended to 64 bits before arithmetic operations
		//       - sign-extended for signed integers (includes Unicode)
		//       - zero-extended for unsigned integers (includes Boolean)
		e.assemblyCode.AppendInstruction(x64.Pop, nil, x64.NewRegisterOperand(x64.R10))

		// compare the left-hand integer value in R10 with the right-hand integer value in R11 (R10 - R11)
		// note: the 'Cmp' instruction itself does not distinguish between signed and unsigned values
		//       conditional jump instructions interpret the CPU flags according to signedness
		e.assemblyCode.AppendInstruction(x64.Cmp, nil, x64.NewRegisterOperand(x64.R10), x64.NewRegisterOperand(x64.R11))

	case ic.Float64:
		// move the right-hand 64-bit floating-point value (IEEE 754 double precision) from the call stack into the XMM1 register
		e.assemblyCode.AppendInstruction(x64.Movsd, labels,
			x64.NewRegisterOperand(x64.Xmm1),
			x64.NewMemoryOperand(x64.Rsp, x64.Bits64, 0))

		// move the left-hand 64-bit floating-point value (IEEE 754 double precision) from the call stack into the XMM0 register
		e.assemblyCode.AppendInstruction(x64.Movsd, nil,
			x64.NewRegisterOperand(x64.Xmm0),
			x64.NewMemoryOperand(x64.Rsp, x64.Bits64, x64.PointerSize))

		// remove both top elements of the call stack (right-hand and left-hand value) by adjusting the stack pointer by 2 times the pointer size
		e.assemblyCode.AppendInstruction(x64.Add, nil,
			x64.NewRegisterOperand(x64.Rsp),
			x64.NewImmediateOperand(int32(2*x64.PointerSize)))

		// compare the left-hand floating-point value in the XMM0 register with the right-hand floating-point value in the XMM1 register
		// note: the 'Ucomisd' instruction compares the two values and sets the CPU flags accordingly
		//       it does not raise an exception for NaN values, but sets the CPU flags to indicate unordered comparisons
		e.assemblyCode.AppendInstruction(x64.Ucomisd, nil, x64.NewRegisterOperand(x64.Xmm0), x64.NewRegisterOperand(x64.Xmm1))

	case ic.Float32:
		// move the right-hand 32-bit floating-point value (IEEE 754 single precision) from the call stack into the XMM1 register
		// note: the value is stored in a 64-bit stack slot (lower 32 bits contain the float)
		e.assemblyCode.AppendInstruction(x64.Movss, labels,
			x64.NewRegisterOperand(x64.Xmm1),
			x64.NewMemoryOperand(x64.Rsp, x64.Bits32, 0))

		// move the left-hand 32-bit floating-point value (IEEE 754 single precision) from the call stack into the XMM0 register
		// note: the value is stored in a 64-bit stack slot (lower 32 bits contain the float)
		e.assemblyCode.AppendInstruction(x64.Movss, nil,
			x64.NewRegisterOperand(x64.Xmm0),
			x64.NewMemoryOperand(x64.Rsp, x64.Bits32, x64.PointerSize))

		// remove both top elements of the call stack (right-hand and left-hand value) by adjusting the stack pointer by 2 times the pointer size
		e.assemblyCode.AppendInstruction(x64.Add, nil,
			x64.NewRegisterOperand(x64.Rsp),
			x64.NewImmediateOperand(int32(2*x64.PointerSize)))

		// compare the left-hand floating-point value in the XMM0 register with the right-hand floating-point value in the XMM1 register
		// note: the 'Ucomiss' instruction compares the two values and sets the CPU flags accordingly
		//       it does not raise an exception for NaN values, but sets the CPU flags to indicate unordered comparisons
		e.assemblyCode.AppendInstruction(x64.Ucomiss, nil, x64.NewRegisterOperand(x64.Xmm0), x64.NewRegisterOperand(x64.Xmm1))

	default:
		// panic if the data type is not supported for the intermediate code operation
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeForIntermediateCodeOperation, dataType, nil))
	}

	// return the comparison type that was used to set the CPU flags
	return comparisonType
}

// Perform an unconditional jump to the specified label.
func (e *emitter) unconditionalJump(name string, labels []string) {
	e.assemblyCode.AppendInstruction(x64.Jmp, labels, x64.NewLabelOperand(name))
}

// Perform a conditional jump based on the CPU flags set by the previous comparison operation.
func (e *emitter) conditionalJump(comparisonType x64.ComparisonType, jump ic.Operation, name string, labels []string) {
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
	e.assemblyCode.AppendInstruction(opcode, labels, x64.NewLabelOperand(name))
}

func (e *emitter) callFunction(name string, depthDifference int32, labels []string) {
	// move the difference between use depth and declaration depth as 32-bit signed integer into the R10d register
	e.assemblyCode.AppendInstruction(x64.Mov, labels,
		x64.NewRegisterOperand(x64.R10d),
		x64.NewImmediateOperand(depthDifference))

	// push return address on call stack and jump to callee
	e.assemblyCode.AppendInstruction(x64.Call, nil, x64.NewLabelOperand(name))
}

func (e *emitter) returnFromFunction(dataType ic.DataType, labels []string) {
	// check whether data type of the return value has modifiers and if so, treat the return value as a pointer or reference
	if dataType.IsPointer() || dataType.IsReference() {
	} else if dataType.IsSupported() {
		// if the return value has a supported data type and is not a pointer or reference, it must be moved into the correct register from the call stack
		for _, register := range dataTypeReturn[dataType] {
			switch {
			case register.IsSse() && dataType.AsPlain() == ic.Float64:
				// move the top of the call stack into the specified floating-point return register
				e.assemblyCode.AppendInstruction(x64.Movsd, labels,
					x64.NewRegisterOperand(register),
					x64.NewMemoryOperand(x64.Rsp, x64.Bits64, 0))

			case register.IsSse() && dataType.AsPlain() == ic.Float32:
				// move the top of the call stack into the specified floating-point return register
				e.assemblyCode.AppendInstruction(x64.Movss, labels,
					x64.NewRegisterOperand(register),
					x64.NewMemoryOperand(x64.Rsp, x64.Bits32, 0))

			case register.IsGeneralPurpose() && (dataType.IsInteger() || dataType.IsCharacter() || dataType.IsBoolean() || dataType.IsString()):
				// pop the top of the call stack into the specified general-purpose return register
				e.assemblyCode.AppendInstruction(x64.Pop, labels, x64.NewRegisterOperand(register))

			default:
				// panic if the data type is not supported for the intermediate code operation
				panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedDataTypeForIntermediateCodeOperation, dataType, nil))
			}

			// only use labels for the first instruction
			labels = nil

			// zero out return register, if necessary (typically only for 8-bit and 16-bit integers)
			if zeroMaskReturn[register] != 0 {
				e.assemblyCode.AppendInstruction(x64.And, nil,
					x64.NewRegisterOperand(register),
					x64.NewImmediateOperand(zeroMaskReturn[register]))
			}
		}
	}

	// return from the function
	e.assemblyCode.AppendInstruction(x64.Ret, nil)
}
