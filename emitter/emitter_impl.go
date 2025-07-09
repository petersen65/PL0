// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emitter

import (
	"container/list"
	"math"

	cor "github.com/petersen65/PL0/v2/core"
	ac "github.com/petersen65/PL0/v2/emitter/assembly"
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
	assemblyCode     ac.AssemblyCodeUnit     // assembly code unit for the CPU target
	cpu              CentralProcessingUnit   // target CPU for the emitter
	offsetTable      map[string]int32        // 32-bit offset of local variables in their activation record
}

var (
	// Map CPU targets to their string representation.
	cpuNames = map[CentralProcessingUnit]string{
		Amd64: "amd64",
	}

	// Map intermediate code datatypes to assembly code bit sizes.
	dataTypeToBits = map[ic.DataType]ac.OperandSize{
		ic.Integer64:  ac.Bits64,
		ic.Integer32:  ac.Bits32,
		ic.Integer16:  ac.Bits16,
		ic.Integer8:   ac.Bits8,
		ic.Float64:    ac.Bits64,
		ic.Float32:    ac.Bits32,
		ic.Unsigned64: ac.Bits64,
		ic.Unsigned32: ac.Bits32,
		ic.Unsigned16: ac.Bits16,
		ic.Unsigned8:  ac.Bits8,
		ic.Unicode:    ac.Bits32,
		ic.Boolean:    ac.Bits8,
	}
)

// Return the interface of the emitter implementation.
func newEmitter(cpu CentralProcessingUnit, intermediateCodeUnit ic.IntermediateCodeUnit) Emitter {
	if cpu != Amd64 {
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedCpuTarget, cpu, nil))
	}

	return &emitter{
		intermediateCode: intermediateCodeUnit,
		assemblyCode:     ac.NewAssemblyCodeUnit(ac.Application),
		cpu:              cpu,
		offsetTable:      make(map[string]int32),
	}
}

// Emit assembly code for the CPU target.
func (e *emitter) Emit() {
	iterator := e.intermediateCode.GetIterator()

	// compile-time parameters list for standard library function calls (procedures do not support parameters yet)
	parameters := list.New()

	// perform an assembly instruction selection for each intermediate code instruction
	for i, l := iterator.First(), make([]string, 0); i != nil; i = iterator.Next() {
		// panic if the intermediate code instruction has not a valid addresses contract
		c := i.ThreeAddressCode.ValidateAddressesContract()

		switch i.ThreeAddressCode.Operation {
		case ic.Target: // target for any branching operation
			// append labels for the directly following non 'Target' instruction
			l = append(l, i.Label)

		case ic.Allocate: // allocate memory for all variables in their logical memory space
			// emit assembly code to allocate space for local variables in the activation record
			e.allocate(iterator, l)

		case ic.Prologue: // function entry sequence
			// emit assembly code to prepare the activation record for the function call
			e.prologue(l)

		case ic.Epilogue: // function exit sequence
			// emit assembly code to restore the activation record of the caller
			e.epilogue(l)

		case ic.Setup: // initialize logical memory space and internal data structures
			// panic if parsing of the metadata into its value fails (unsupported value or data type)
			depth := i.ThreeAddressCode.Arg1.Parse().(int32)

			// emit assembly code to setup a function call
			e.setup(depth)

		case ic.ValueCopy: // copy an immediate value to an address
			// panic if parsing of the literal into its value fails (unsupported value or data type)
			value := i.ThreeAddressCode.Arg1.Parse()

			// emit assembly code to copy the value onto the top of the call stack
			e.valueCopy(i.ThreeAddressCode.Arg1.DataType, value, l)

		case ic.VariableLoad: // load a variable from its call stack address onto the top of the stack
			// panic if parsing of the variable into nil fails (unsupported data type)
			i.ThreeAddressCode.Arg1.Parse()

			// determine offset of the local variable in its activation record
			offset := e.offsetTable[i.ThreeAddressCode.Arg1.Name]

			// emit assembly code to load the variable onto the call stack
			e.variableLoad(i.ThreeAddressCode.Arg1.DataType, offset, i.DepthDifference, l)

		case ic.VariableStore: // store the top of the call stack into a variable's stack address
			// panic if parsing of the variable into nil fails (unsupported data type)
			i.ThreeAddressCode.Result.Parse()

			// determine offset of the local variable in its activation record
			offset := e.offsetTable[i.ThreeAddressCode.Result.Name]

			// emit assembly code to store the top of the call stack into the variable's activation record
			e.variableStore(i.ThreeAddressCode.Result.DataType, offset, i.DepthDifference, l)

		case ic.Negate: // negate the top of the call stack and leave the result on the stack
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.ThreeAddressCode.Arg1.Parse()

			// emit assembly code to negate the top of the call stack
			e.negate(i.ThreeAddressCode.Arg1.DataType, l)

		case ic.Odd: // check if the top of the call stack is an odd number and set the Zero Flag (ZF clear if odd, set if even)
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.ThreeAddressCode.Arg1.Parse()

			// emit assembly code to check if the top of the call stack is odd
			e.odd(i.ThreeAddressCode.Arg1.DataType, l)

		case ic.Plus, ic.Minus, ic.Times, ic.Divide: // perform an arithmetic operation on the top two elements of the call stack and replace them with one result on the stack
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.ThreeAddressCode.Arg1.Parse()
			_ = i.ThreeAddressCode.Arg2.Parse()

			// it is required that both temporaries are of the same data type
			dataType := i.ThreeAddressCode.Arg1.DataType

			// emit assembly code to perform the arithmetic operation on the top two elements of the call stack
			if i.ThreeAddressCode.Operation == ic.Divide && dataType.IsInteger() {
				e.integerDivide(dataType, l)
			} else {
				e.arithmeticOperation(dataType, i.ThreeAddressCode.Operation, l)
			}

		case ic.Equal, ic.NotEqual, ic.Less, ic.LessEqual, ic.Greater, ic.GreaterEqual: // compare the top two elements of the call stack, remove them, and leave the result in the CPU flags register
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.ThreeAddressCode.Arg1.Parse()
			_ = i.ThreeAddressCode.Arg2.Parse()

			// it is required that both temporaries are of the same data type
			dataType := i.ThreeAddressCode.Arg1.DataType

			// emit assembly code to compare the top two elements of the call stack
			e.compare(dataType, l)

		case ic.Jump: // unconditionally jump to a label resolved at link-time
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.ThreeAddressCode.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(ac.Jmp, l, ac.NewLabelOperand(name))

		case ic.JumpEqual: // jump if the Zero Flag (ZF) is set from the previous comparison
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.ThreeAddressCode.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(ac.Je, l, ac.NewLabelOperand(name))

		case ic.JumpNotEqual: // jump if the Zero Flag (ZF) is clear from the previous comparison
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.ThreeAddressCode.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(ac.Jne, l, ac.NewLabelOperand(name))

		case ic.JumpLess: // jump if the Sign Flag (SF) and Overflow Flag (OF) indicate a signed less-than from the previous comparison
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.ThreeAddressCode.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(ac.Jl, l, ac.NewLabelOperand(name))

		case ic.JumpLessEqual: // jump if the previous comparison indicates less-than or equal (ZF set or SF ≠ OF), signed comparison
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.ThreeAddressCode.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(ac.Jle, l, ac.NewLabelOperand(name))

		case ic.JumpGreater: // jump if the previous comparison indicates greater-than (ZF clear and SF = OF), signed comparison
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.ThreeAddressCode.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(ac.Jg, l, ac.NewLabelOperand(name))

		case ic.JumpGreaterEqual: // jump if the previous comparison indicates greater-than or equal (SF = OF), signed comparison
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.ThreeAddressCode.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(ac.Jge, l, ac.NewLabelOperand(name))

		case ic.Parameter: // push a parameter onto the compile-time parameters list for a standard library function call
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.ThreeAddressCode.Arg1.Parse()
			parameters.PushBack(i)

		case ic.Call: // call a function with 0 arguments by jumping to the function's label
			// panic if parsing of the parameters count into an unsigned integer fails (unsupported value or data type)
			count := i.ThreeAddressCode.Arg1.Parse().(uint64)

			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.ThreeAddressCode.Arg2.Parse().(string)

			if count != 0 {
				// procedures do not support parameters yet
				panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unexpectedNumberOfFunctionArguments, nil, nil))
			} else {
				// push difference between use depth and declaration depth on call stack
				// e.assemblyCode.AppendInstruction(ac.Push, l, ac.NewImmediateOperand(ac.Bits64, int64(i.DepthDifference)))

				// move the difference between use depth and declaration depth as 32-bit signed integer into the R10d register
				e.assemblyCode.AppendInstruction(ac.Mov, l,
					ac.NewRegisterOperand(ac.R10d),
					ac.NewImmediateOperand(ac.Bits32, int32(i.DepthDifference)))

				// push return address on call stack and jump to callee
				e.assemblyCode.AppendInstruction(ac.Call, nil, ac.NewLabelOperand(name))

				// remove difference from call stack after return from callee
				// e.assemblyCode.AppendInstruction(ac.Add, nil, ac.NewRegisterOperand(ac.Rsp), ac.NewImmediateOperand(ac.Bits64, int64(1)))
			}

		case ic.Return: // return from a function to its caller
			// check if the function has a literal return value
			if c.Arg1 == ic.Literal {
				// panic if parsing of the literal into its value fails (unsupported value or data type)
				value := i.ThreeAddressCode.Arg1.Parse().(int32)

				// move the literal return value into the EAX register as the return value of the function (what the C runtime expects)
				e.assemblyCode.AppendInstruction(ac.Mov, nil, ac.NewRegisterOperand(ac.Eax), ac.NewImmediateOperand(ac.Bits32, value))
			}

			e.assemblyCode.AppendInstruction(ac.Ret, nil)

		case ic.Standard:
			// a standard function represents a function that is provided by the standard library of the programming language
			// the standard function has 2 direct parameters that are part of the intermediate code instruction at compile-time
			// the first parameter holds the number of parameters that the standard function expects
			// the second parameter holds the call code of the standard function in the programming language's standard library

			// panic if parsing of the parameters count into an unsigned integer fails (unsupported value or data type)
			count := i.ThreeAddressCode.Arg1.Parse().(uint64)

			// panic if parsing of the call code into an integer fails (unsupported value or data type)
			code := i.ThreeAddressCode.Arg2.Parse().(int64)

			// parameter instruction for the standard library function call
			pi := parameters.Back().Value.(*ic.Instruction)
			parameters.Remove(parameters.Back())

			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = pi.ThreeAddressCode.Arg1.Parse()

			if count != 1 {
				// current standard library functions expect 1 parameter on the call stack
				panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unexpectedNumberOfFunctionArguments, nil, nil))
			} else {
				// the top element of the call stack is either consumed or updated by the standard library function
				e.assemblyCode.AppendInstruction(ac.StdCall, l, ac.NewImmediateOperand(ac.Bits64, code))
			}

		default:
			panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unknownIntermediateCodeOperation, i.ThreeAddressCode.Operation, nil))
		}

		// collected labels must be used by the directly following instruction (one instruction consumes all collected labels)
		if i.ThreeAddressCode.Operation != ic.Target {
			l = make([]string, 0)
		}
	}
}

// Get access to the generated assembly code.
func (e *emitter) GetAssemblyCodeUnit() ac.AssemblyCodeUnit {
	return e.assemblyCode
}

// Allocate space for local variables in the activation record of a function and remember their offsets.
func (e *emitter) allocate(iterator ic.Iterator, labels []string) {
	// group consecutive intermediate code allocate operations into one space allocation instruction
	for j, offset := 0, int32(0); iterator.Peek(j) != nil; j++ {
		if iterator.Peek(j).ThreeAddressCode.Operation == ic.Allocate {
			// local variable to allocate space for
			result := iterator.Peek(j).ThreeAddressCode.Result

			// calculate memory size and allignment of the local variable
			byteSize := int32(dataTypeToBits[result.DataType]) / 8
			offset = dataTypeToBits[result.DataType].Alignment(offset - byteSize)

			// remember offset of the local variable in its activation record
			e.offsetTable[result.Name] = offset
		}

		// break if all local variables int the activiation record have been allocated
		if iterator.Peek(j+1) != nil && iterator.Peek(j+1).ThreeAddressCode.Operation != ic.Allocate {
			// align the offset and use it as the size required for storing all local variables
			offset = ac.Align(offset, callStackAlignment)

			// grow the call stack downwards to provide space for all local variables int the activiation record (2GB maximum)
			e.assemblyCode.AppendInstruction(ac.Sub, labels,
				ac.NewRegisterOperand(ac.Rsp),
				ac.NewImmediateOperand(ac.Bits32, -offset))

			// zero out the allocated space for local variables in the activation record
			e.assemblyCode.AppendInstruction(ac.Cld, nil)
			e.assemblyCode.AppendInstruction(ac.Mov, nil, ac.NewRegisterOperand(ac.Rax), ac.NewImmediateOperand(ac.Bits32, int32(0)))
			e.assemblyCode.AppendInstruction(ac.Mov, nil, ac.NewRegisterOperand(ac.Rcx), ac.NewImmediateOperand(ac.Bits32, -offset/ac.QuadWordSize))
			e.assemblyCode.AppendInstruction(ac.Mov, nil, ac.NewRegisterOperand(ac.Rdi), ac.NewRegisterOperand(ac.Rsp))
			e.assemblyCode.AppendPrefixedInstruction(ac.Rep, ac.Stosq, nil)

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
	e.assemblyCode.AppendInstruction(ac.Push, labels, ac.NewRegisterOperand(ac.Rbp))
	e.assemblyCode.AppendInstruction(ac.Mov, nil, ac.NewRegisterOperand(ac.Rbp), ac.NewRegisterOperand(ac.Rsp))
}

// The function exit sequence is called epilogue and restores the activation record of the caller.
func (e *emitter) epilogue(labels []string) {
	// clean allocated local variables from the activation record and restore caller's base pointer
	e.assemblyCode.AppendInstruction(ac.Mov, labels, ac.NewRegisterOperand(ac.Rsp), ac.NewRegisterOperand(ac.Rbp))
	e.assemblyCode.AppendInstruction(ac.Pop, nil, ac.NewRegisterOperand(ac.Rbp))
}

// Setup a function call by initializing the logical memory space and internal data structures.
func (e *emitter) setup(depth int32) {
	// only blocks with a depth greater than 0 have a static link
	// the main block has depth 0, no lexical parent and therefore no static link
	if depth > 0 {
		// call runtime function to create static link which provides the compile-time block nesting hierarchy at runtime
		e.assemblyCode.AppendInstruction(ac.Call, nil, ac.NewLabelOperand(ac.CreateStaticLinkLabel))
	}
}

// Copy an immediate value onto the top of the call stack.
func (e *emitter) valueCopy(dataType ic.DataType, value any, labels []string) {
	// depending on the data type, the value is copied onto the call stack as an immediate value or as a 64-bit value in the R10 register
	switch dataType {
	case ic.Integer64:
		// move the 64-bit signed integer into the R10 register without sign extension
		e.assemblyCode.AppendInstruction(ac.MovAbs, labels,
			ac.NewRegisterOperand(ac.R10),
			ac.NewImmediateOperand(ac.Bits64, value.(int64)))

		// push the R10 register onto the call stack
		e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.R10))

	case ic.Integer32:
		// push the 32-bit signed integer onto the call stack and sign-extend it to 64 bits
		e.assemblyCode.AppendInstruction(ac.Push, labels, ac.NewImmediateOperand(ac.Bits32, value.(int32)))

	case ic.Integer16:
		// convert the 16-bit signed integer to a 32-bit signed integer before pushing it onto the call stack and sign-extend it to 64 bits
		e.assemblyCode.AppendInstruction(ac.Push, labels, ac.NewImmediateOperand(ac.Bits32, int32(value.(int16))))

	case ic.Integer8:
		// push the 8-bit signed integer onto the call stack and sign-extend it to 64 bits
		e.assemblyCode.AppendInstruction(ac.Push, labels, ac.NewImmediateOperand(ac.Bits8, value.(int8)))

	case ic.Unsigned64:
		// move the 64-bit unsigned integer into the R10 register without sign extension
		e.assemblyCode.AppendInstruction(ac.MovAbs, labels,
			ac.NewRegisterOperand(ac.R10),
			ac.NewImmediateOperand(ac.Bits64, value.(uint64)))

		// push the R10 register onto the call stack
		e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.R10))

	case ic.Unsigned32:
		// move the 32-bit unsigned integer into the R10d register and zero-extend to 64 bits
		e.assemblyCode.AppendInstruction(ac.Mov, labels,
			ac.NewRegisterOperand(ac.R10d),
			ac.NewImmediateOperand(ac.Bits32, value.(uint32)))

		// push the R10 register onto the call stack
		// note: writing to R10d has already zeroed the upper 32 bits of R10, so pushing R10 pushes the correct zero-extended 64-bit value
		e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.R10))

	case ic.Unsigned16:
		// move the 16-bit unsigned integer, converted to 32 bits, into the R10d register and zero-extend to 64 bits
		e.assemblyCode.AppendInstruction(ac.Mov, labels,
			ac.NewRegisterOperand(ac.R10d),
			ac.NewImmediateOperand(ac.Bits32, uint32(value.(uint16))))

		// push the R10 register onto the call stack
		// note: writing to R10d has already zeroed the upper 32 bits of R10, so pushing R10 pushes the correct zero-extended 64-bit value
		e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.R10))

	case ic.Unsigned8:
		// move the 8-bit unsigned integer, converted to 32 bits, into the R10d register and zero-extend to 64 bits
		e.assemblyCode.AppendInstruction(ac.Mov, labels,
			ac.NewRegisterOperand(ac.R10d),
			ac.NewImmediateOperand(ac.Bits32, uint32(value.(uint8))))

		// push the R10 register onto the call stack
		// note: writing to R10d has already zeroed the upper 32 bits of R10, so pushing R10 pushes the correct zero-extended 64-bit value
		e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.R10))

	case ic.Float64:
		// convert the 64-bit float value to its IEEE 754 binary representation
		binaryRepresentationIEEE754 := math.Float64bits(value.(float64))

		// move the 64-bit float value into the R10 register without any extension
		e.assemblyCode.AppendInstruction(ac.MovAbs, labels,
			ac.NewRegisterOperand(ac.R10),
			ac.NewImmediateOperand(ac.Bits64, binaryRepresentationIEEE754))

		// push the R10 register onto the call stack
		e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.R10))

	case ic.Float32:
		// convert the 32-bit float value to its IEEE 754 binary representation
		binaryRepresentationIEEE754 := math.Float32bits(value.(float32))

		// move the 32-bit float value into the lower 32 bits of the R10 register (named R10d) and zero-extend the upper 32 bits
		e.assemblyCode.AppendInstruction(ac.Mov, labels,
			ac.NewRegisterOperand(ac.R10d),
			ac.NewImmediateOperand(ac.Bits32, binaryRepresentationIEEE754))

		// push the 64-bit R10 register onto the call stack (32-bit float value was zero-extended to 64 bits)
		e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.R10))

	case ic.Unicode:
		// convert the Unicode code point to a 32-bit signed integer before pushing it onto the call stack
		e.assemblyCode.AppendInstruction(ac.Push, labels, ac.NewImmediateOperand(ac.Bits32, int32(value.(rune))))

	case ic.Boolean:
		// convert the boolean value to an 8-bit unsigned integer before pushing it onto the call stack
		// note: sign extension will have no effect because the boolean value is either 0 or 1
		if value.(bool) {
			e.assemblyCode.AppendInstruction(ac.Push, labels, ac.NewImmediateOperand(ac.Bits8, uint8(1)))
		} else {
			e.assemblyCode.AppendInstruction(ac.Push, labels, ac.NewImmediateOperand(ac.Bits8, uint8(0)))
		}
	}
}

// Load a variable from its activation record onto the top of the call stack.
func (e *emitter) variableLoad(dataType ic.DataType, offset, depthDifference int32, labels []string) {
	var basePointer ac.Register

	// determine the correct activation record from which to load the variable
	if depthDifference == 0 {
		// use the variables base pointer of the current activation record
		basePointer = ac.Rbp
	} else {
		// block nesting depth difference between variable use and variable declaration
		e.assemblyCode.AppendInstruction(ac.Mov, labels,
			ac.NewRegisterOperand(ac.Edi),
			ac.NewImmediateOperand(ac.Bits32, depthDifference))

		// follow the static link to determine the 'variables base' pointer of the correct lexical parent activation record
		e.assemblyCode.AppendInstruction(ac.Call, nil, ac.NewLabelOperand(ac.FollowStaticLinkLabel))

		// take the variables base pointer from the Rax register that is returned from the runtime function call
		labels = nil
		basePointer = ac.Rax
	}

	// depending on the data type, the variable is loaded from the activation record into the R10 register and then pushed onto the call stack
	switch dataType {
	case ic.Integer64, ic.Unsigned64, ic.Float64:
		// move the 64-bit integer/float bitwise from the activation record into the R10 register
		e.assemblyCode.AppendInstruction(ac.Mov, labels,
			ac.NewRegisterOperand(ac.R10),
			ac.NewMemoryOperand(basePointer, ac.Bits64, offset))

		// push the R10 register onto the call stack
		e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.R10))

	case ic.Integer32, ic.Unicode:
		// move the 32-bit signed integer/rune from the activation record into the R10 register and sign-extend it to 64 bits
		e.assemblyCode.AppendInstruction(ac.Movsxd, labels,
			ac.NewRegisterOperand(ac.R10),
			ac.NewMemoryOperand(basePointer, ac.Bits32, offset))

		// push the R10 register onto the call stack
		e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.R10))

	case ic.Unsigned32, ic.Float32:
		// move the 32-bit unsigned integer/float bitwise from the activation record into the R10d register and zero-extend it to 64 bits
		e.assemblyCode.AppendInstruction(ac.Mov, labels,
			ac.NewRegisterOperand(ac.R10d),
			ac.NewMemoryOperand(basePointer, ac.Bits32, offset))

		// push the R10 register onto the call stack
		// note: writing to R10d has already zeroed the upper 32 bits of R10, so pushing R10 pushes the correct zero-extended 64-bit value
		e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.R10))

	case ic.Integer16:
		// move the 16-bit signed integer from the activation record into the R10 register and sign-extend it to 64 bits
		e.assemblyCode.AppendInstruction(ac.Movsx, labels,
			ac.NewRegisterOperand(ac.R10),
			ac.NewMemoryOperand(basePointer, ac.Bits16, offset))

		// push the R10 register onto the call stack
		e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.R10))

	case ic.Unsigned16:
		// move the 16-bit unsigned integer from the activation record into the R10d register and zero-extend it to 32 bits
		e.assemblyCode.AppendInstruction(ac.Movzx, labels,
			ac.NewRegisterOperand(ac.R10d),
			ac.NewMemoryOperand(basePointer, ac.Bits16, offset))

		// push the R10 register onto the call stack
		// note: writing to R10d has already zeroed the upper 32 bits of R10, so pushing R10 pushes the correct zero-extended 64-bit value
		e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.R10))

	case ic.Integer8:
		// move the 8-bit signed integer from the activation record into the R10 register and sign-extend it to 64 bits
		e.assemblyCode.AppendInstruction(ac.Movsx, labels,
			ac.NewRegisterOperand(ac.R10),
			ac.NewMemoryOperand(basePointer, ac.Bits8, offset))

		// push the R10 register onto the call stack
		e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.R10))

	case ic.Unsigned8, ic.Boolean:
		// move the 8-bit unsigned integer from the activation record into the R10d register and zero-extend it to 32 bits
		e.assemblyCode.AppendInstruction(ac.Movzx, labels,
			ac.NewRegisterOperand(ac.R10d),
			ac.NewMemoryOperand(basePointer, ac.Bits8, offset))

		// push the R10 register onto the call stack
		// note: writing to R10d has already zeroed the upper 32 bits of R10, so pushing R10 pushes the correct zero-extended 64-bit value
		e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.R10))
	}
}

// Store the top of the call stack into a variable's activation record.
func (e *emitter) variableStore(dataType ic.DataType, offset, depthDifference int32, labels []string) {
	var basePointer ac.Register

	// determine the correct activation record to which to store the variable
	if depthDifference == 0 {
		// use the variables base pointer of the current activation record
		basePointer = ac.Rbp
	} else {
		// block nesting depth difference between variable use and variable declaration
		e.assemblyCode.AppendInstruction(ac.Mov, labels,
			ac.NewRegisterOperand(ac.Edi),
			ac.NewImmediateOperand(ac.Bits32, depthDifference))

		// follow the static link to determine the 'variables base' pointer of the correct lexical parent activation record
		e.assemblyCode.AppendInstruction(ac.Call, nil, ac.NewLabelOperand(ac.FollowStaticLinkLabel))

		// take the variables base pointer from the Rax register that is returned from the runtime function call
		labels = nil
		basePointer = ac.Rax
	}

	// pop the top of the call stack into the R10 register
	e.assemblyCode.AppendInstruction(ac.Pop, labels, ac.NewRegisterOperand(ac.R10))

	// depending on the data type, the R10 register is stored into the activation record of the variable
	switch dataType {
	case ic.Integer64, ic.Unsigned64, ic.Float64:
		// move the 64-bit integers/float bitwise from the R10 register into the activation record
		e.assemblyCode.AppendInstruction(ac.Mov, nil,
			ac.NewMemoryOperand(basePointer, ac.Bits64, offset),
			ac.NewRegisterOperand(ac.R10))

	case ic.Integer32, ic.Unsigned32, ic.Unicode, ic.Float32:
		// move the 32-bit signed integer and unsigned integer/rune/float bitwise from the R10d register into the activation record
		e.assemblyCode.AppendInstruction(ac.Mov, nil,
			ac.NewMemoryOperand(basePointer, ac.Bits32, offset),
			ac.NewRegisterOperand(ac.R10d))

	case ic.Integer16, ic.Unsigned16:
		// move the 16-bit integers bitwise from the R10w register into the activation record
		e.assemblyCode.AppendInstruction(ac.Mov, nil,
			ac.NewMemoryOperand(basePointer, ac.Bits16, offset),
			ac.NewRegisterOperand(ac.R10w))

	case ic.Integer8, ic.Unsigned8, ic.Boolean:
		// move the 8-bit integers/boolean bitwise from the R10b register into the activation record
		e.assemblyCode.AppendInstruction(ac.Mov, nil,
			ac.NewMemoryOperand(basePointer, ac.Bits8, offset),
			ac.NewRegisterOperand(ac.R10b))
	}
}

// Negate the top of the call stack and leave the result on top of the stack.
func (e *emitter) negate(dataType ic.DataType, labels []string) {
	// depending on the data type, the top of the call stack is popped into the correct register and negated
	switch dataType {
	case ic.Integer64, ic.Integer32, ic.Integer16, ic.Integer8:
		// all integer 64-bit, 32-bit, 16-bit and 8-bit values are negated the same way
		// note: all integer values must be correctly sign-extended to 64 bits before negation
		e.assemblyCode.AppendInstruction(ac.Pop, labels, ac.NewRegisterOperand(ac.R10))
		e.assemblyCode.AppendInstruction(ac.Neg, nil, ac.NewRegisterOperand(ac.R10))
		e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.R10))

	case ic.Float64:
		// move the 64-bit floating-point value (IEEE 754 double precision) to the XMM0 register
		e.assemblyCode.AppendInstruction(ac.Movsd, labels,
			ac.NewRegisterOperand(ac.Xmm0),
			ac.NewMemoryOperand(ac.Rsp, ac.Bits64, 0))

		// move the 64-bit floating-point sign bit mask into the R10 register
		e.assemblyCode.AppendInstruction(ac.Mov, nil,
			ac.NewRegisterOperand(ac.R10),
			ac.NewImmediateOperand(ac.Bits64, float64SignBitMask))

		// move the 64-bit floating-point sign bit mask into the XMM1 register
		e.assemblyCode.AppendInstruction(ac.Movq, nil,
			ac.NewRegisterOperand(ac.Xmm1),
			ac.NewRegisterOperand(ac.R10))

		// perform a bitwise XOR operation between the XMM0 register and the XMM1 register
		// note: this flips the sign bit of the floating-point value in the XMM0 register
		e.assemblyCode.AppendInstruction(ac.Xorpd, nil,
			ac.NewRegisterOperand(ac.Xmm0),
			ac.NewRegisterOperand(ac.Xmm1))

		// move the negated 64-bit floating-point value back onto the top of the call stack
		e.assemblyCode.AppendInstruction(ac.Movsd, nil,
			ac.NewMemoryOperand(ac.Rsp, ac.Bits64, 0),
			ac.NewRegisterOperand(ac.Xmm0))

	case ic.Float32:
		// move the 32-bit floating-point value (IEEE 754 single precision) to the XMM0 register
		// note: the Bits32 is redundant here because the 'Movss' instruction always expects a 32-bit value
		e.assemblyCode.AppendInstruction(ac.Movss, labels,
			ac.NewRegisterOperand(ac.Xmm0),
			ac.NewMemoryOperand(ac.Rsp, ac.Bits32, 0))

		// move the 32-bit floating-point sign bit mask into the R10d register
		e.assemblyCode.AppendInstruction(ac.Mov, nil,
			ac.NewRegisterOperand(ac.R10d),
			ac.NewImmediateOperand(ac.Bits32, float32SignBitMask))

		// move the 32-bit floating-point sign bit mask into the XMM1 register
		e.assemblyCode.AppendInstruction(ac.Movd, nil,
			ac.NewRegisterOperand(ac.Xmm1),
			ac.NewRegisterOperand(ac.R10d))

		// perform a bitwise XOR operation between the XMM0 register and the XMM1 register
		// note: this flips the sign bit of the single precision floating-point value in the XMM0 register
		e.assemblyCode.AppendInstruction(ac.Xorps, nil,
			ac.NewRegisterOperand(ac.Xmm0),
			ac.NewRegisterOperand(ac.Xmm1))

		// move the negated 32-bit floating-point value back onto the top of the call stack
		e.assemblyCode.AppendInstruction(ac.Movss, nil,
			ac.NewMemoryOperand(ac.Rsp, ac.Bits32, 0),
			ac.NewRegisterOperand(ac.Xmm0))

		// clear the upper 32 bits from [RSP] to maintain a clean 64-bit call stack slot
		e.assemblyCode.AppendInstruction(ac.Mov, nil,
			ac.NewMemoryOperand(ac.Rsp, ac.Bits32, ac.DoubleWordSize),
			ac.NewImmediateOperand(ac.Bits32, 0))
	}
}

// Check if the top of the call stack is odd depending on the data type.
func (e *emitter) odd(dataType ic.DataType, labels []string) {
	// depending on the data type, the top of the call stack is popped into the correct register and checked for oddness
	switch dataType {
	case ic.Integer64, ic.Integer32, ic.Integer16, ic.Integer8:
		// all integer 64-bit, 32-bit, 16-bit and 8-bit values are checked for oddness the same way
		// note: all integer values must be correctly sign-extended to 64 bits before checking if they are odd
		e.assemblyCode.AppendInstruction(ac.Pop, labels, ac.NewRegisterOperand(ac.R10))

		// test the least significant bit to determine oddness (64-bit 'Test' instruction is sufficient)
		e.assemblyCode.AppendInstruction(ac.Test, nil, ac.NewRegisterOperand(ac.R10), ac.NewImmediateOperand(ac.Bits64, uint64(1)))
	}
}

// Perform the arithmetic operations 'Plus', 'Minus', 'Times' on the top two elements of the call stack and replace them with one result on the stack.
func (e *emitter) arithmeticOperation(dataType ic.DataType, operation ic.Operation, labels []string) {
	// depending on the data type, the top two elements of the call stack are popped into the correct registers and the arithmetic operation is performed
	switch dataType {
	case ic.Integer64, ic.Integer32, ic.Integer16, ic.Integer8, ic.Unsigned64, ic.Unsigned32, ic.Unsigned16, ic.Unsigned8:
		// pop the right-hand integer value from the call stack into the R11 register
		// note: all integer values must be correctly sign-extended to 64 bits before arithmetic operations
		e.assemblyCode.AppendInstruction(ac.Pop, labels, ac.NewRegisterOperand(ac.R11))

		// pop the left-hand integer value from the call stack into the R10 register
		// note: all integer values must be correctly sign-extended to 64 bits before arithmetic operations
		e.assemblyCode.AppendInstruction(ac.Pop, nil, ac.NewRegisterOperand(ac.R10))

		// the R10 and R11 registers are used to perform the arithmetic operation which leaves the result in the R10 register
		switch operation {
		case ic.Plus:
			// add the right-hand integer value in the R11 register to the left-hand integer value in the R10 register
			e.assemblyCode.AppendInstruction(ac.Add, nil, ac.NewRegisterOperand(ac.R10), ac.NewRegisterOperand(ac.R11))

		case ic.Minus:
			// subtract the right-hand integer value in the R11 register from the left-hand integer value in the R10 register
			e.assemblyCode.AppendInstruction(ac.Sub, nil, ac.NewRegisterOperand(ac.R10), ac.NewRegisterOperand(ac.R11))

		case ic.Times:
			// multiply the left-hand integer value in the R10 register with the right-hand integer value in the R11 register
			e.assemblyCode.AppendInstruction(ac.Imul, nil, ac.NewRegisterOperand(ac.R10), ac.NewRegisterOperand(ac.R11))
		}

		// push the result of the arithmetic operation onto the call stack
		// note: flags from the 'Add', 'Sub', and 'Imul' instructions are intentionally ignored and will be overwritten by the 'Push' instruction
		e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.R10))

	case ic.Float64:
		// move the right-hand 64-bit floating-point value (IEEE 754 double precision) from the call stack into the XMM1 register
		e.assemblyCode.AppendInstruction(ac.Movsd, labels,
			ac.NewRegisterOperand(ac.Xmm1),
			ac.NewMemoryOperand(ac.Rsp, ac.Bits64, 0))

		// move the left-hand 64-bit floating-point value (IEEE 754 double precision) from the call stack into the XMM0 register
		e.assemblyCode.AppendInstruction(ac.Movsd, nil,
			ac.NewRegisterOperand(ac.Xmm0),
			ac.NewMemoryOperand(ac.Rsp, ac.Bits64, ac.PointerSize))

		// the XMM0 and XMM1 registers are used to perform the arithmetic operation which leaves the result in the XMM0 register
		switch operation {
		case ic.Plus:
			// add the right-hand floating-point value in the XMM1 register to the left-hand floating-point value in the XMM0 register
			e.assemblyCode.AppendInstruction(ac.Addsd, nil,
				ac.NewRegisterOperand(ac.Xmm0),
				ac.NewRegisterOperand(ac.Xmm1))

		case ic.Minus:
			// subtract the right-hand floating-point value in the XMM1 register from the left-hand floating-point value in the XMM0 register
			e.assemblyCode.AppendInstruction(ac.Subsd, nil,
				ac.NewRegisterOperand(ac.Xmm0),
				ac.NewRegisterOperand(ac.Xmm1))

		case ic.Times:
			// multiply the right-hand floating-point value in the XMM1 register with the left-hand floating-point value in the XMM0 register
			e.assemblyCode.AppendInstruction(ac.Mulsd, nil,
				ac.NewRegisterOperand(ac.Xmm0),
				ac.NewRegisterOperand(ac.Xmm1))

		case ic.Divide:
			// divide the left-hand floating-point value in the XMM0 register by the right-hand floating-point value in the XMM1 register
			e.assemblyCode.AppendInstruction(ac.Divsd, nil,
				ac.NewRegisterOperand(ac.Xmm0),
				ac.NewRegisterOperand(ac.Xmm1))
		}

		// remove the top element of the call stack (the right-hand value) by adjusting the stack pointer by 1 times the pointer size
		e.assemblyCode.AppendInstruction(ac.Add, nil,
			ac.NewRegisterOperand(ac.Rsp),
			ac.NewImmediateOperand(ac.Bits32, ac.PointerSize))

		// move the result of the arithmetic operation back onto the top of the call stack, overwriting the left-hand value
		e.assemblyCode.AppendInstruction(ac.Movsd, nil,
			ac.NewMemoryOperand(ac.Rsp, ac.Bits64, 0),
			ac.NewRegisterOperand(ac.Xmm0))

	case ic.Float32:
		// move the right-hand 32-bit floating-point value (IEEE 754 single precision) from the call stack into the XMM1 register
		e.assemblyCode.AppendInstruction(ac.Movss, labels,
			ac.NewRegisterOperand(ac.Xmm1),
			ac.NewMemoryOperand(ac.Rsp, ac.Bits32, 0))

		// move the left-hand 32-bit floating-point value (IEEE 754 single precision) from the call stack into the XMM0 register
		e.assemblyCode.AppendInstruction(ac.Movss, nil,
			ac.NewRegisterOperand(ac.Xmm0),
			ac.NewMemoryOperand(ac.Rsp, ac.Bits32, ac.PointerSize))

		// the XMM0 and XMM1 registers are used to perform the arithmetic operation which leaves the result in the XMM0 register
		switch operation {
		case ic.Plus:
			// add the right-hand floating-point value in the XMM1 register to the left-hand floating-point value in the XMM0 register
			e.assemblyCode.AppendInstruction(ac.Addss, nil,
				ac.NewRegisterOperand(ac.Xmm0),
				ac.NewRegisterOperand(ac.Xmm1))

		case ic.Minus:
			// subtract the right-hand floating-point value in the XMM1 register from the left-hand floating-point value in the XMM0 register
			e.assemblyCode.AppendInstruction(ac.Subss, nil,
				ac.NewRegisterOperand(ac.Xmm0),
				ac.NewRegisterOperand(ac.Xmm1))

		case ic.Times:
			// multiply the right-hand floating-point value in the XMM1 register with the left-hand floating-point value in the XMM0 register
			e.assemblyCode.AppendInstruction(ac.Mulss, nil,
				ac.NewRegisterOperand(ac.Xmm0),
				ac.NewRegisterOperand(ac.Xmm1))

		case ic.Divide:
			// divide the left-hand floating-point value in the XMM0 register by the right-hand floating-point value in the XMM1 register
			e.assemblyCode.AppendInstruction(ac.Divss, nil,
				ac.NewRegisterOperand(ac.Xmm0),
				ac.NewRegisterOperand(ac.Xmm1))
		}

		// remove the top element of the call stack (the right-hand value) by adjusting the stack pointer by 1 times the pointer size
		e.assemblyCode.AppendInstruction(ac.Add, nil,
			ac.NewRegisterOperand(ac.Rsp),
			ac.NewImmediateOperand(ac.Bits32, ac.PointerSize))

		// // move the result of the arithmetic operation back onto the top of the call stack and overwrite the left-hand value's lower 32 bits
		e.assemblyCode.AppendInstruction(ac.Movss, nil,
			ac.NewMemoryOperand(ac.Rsp, ac.Bits32, 0),
			ac.NewRegisterOperand(ac.Xmm0))

		// clear the upper 32 bits from [RSP] to maintain a clean 64-bit call stack slot
		e.assemblyCode.AppendInstruction(ac.Mov, nil,
			ac.NewMemoryOperand(ac.Rsp, ac.Bits32, ac.DoubleWordSize),
			ac.NewImmediateOperand(ac.Bits32, 0))
	}
}

// Perform the arithmetic operation 'Divide' on the top two elements of the call stack and replace them with one result on the stack.
func (e *emitter) integerDivide(dataType ic.DataType, labels []string) {
	// depending on the data type, the top two elements of the call stack are popped into the correct registers and the division operation is performed
	switch dataType {
	case ic.Integer64, ic.Integer32, ic.Integer16, ic.Integer8:
		// pop the right-hand integer value (divisor) from the call stack into the R11 register
		// note: all integer values must be correctly sign-extended to 64 bits before arithmetic operations
		e.assemblyCode.AppendInstruction(ac.Pop, labels, ac.NewRegisterOperand(ac.R11))

		// pop the left-hand integer value (dividend) from the call stack into the RAX register (required by IDIV)
		// note: all integer values must be correctly sign-extended to 64 bits before arithmetic operations
		e.assemblyCode.AppendInstruction(ac.Pop, nil, ac.NewRegisterOperand(ac.Rax))

		// sign-extend RAX to RDX:RAX by using the 'Cqo' instruction for a signed division (convert quadword to octword)
		// note: CQO operates on 64-bit operands but is safe to use with smaller types, as long as the value in RAX is properly sign-extended to 64 bits beforehand
		e.assemblyCode.AppendInstruction(ac.Cqo, nil)

		// divide the 128-bit dividend in RDX:RAX by the 64-bit divisor in R11 (signed division)
		// note: the quotient is stored in RAX and the remainder in RDX
		e.assemblyCode.AppendInstruction(ac.Idiv, nil, ac.NewRegisterOperand(ac.R11))

		// push the result onto the stack (quotient)
		e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.Rax))

	case ic.Unsigned64, ic.Unsigned32, ic.Unsigned16, ic.Unsigned8:
		// pop the right-hand integer value (divisor) from the call stack into the R11 register
		// note: all unsigned integer values must be correctly zero-extended to 64 bits before arithmetic operations
		e.assemblyCode.AppendInstruction(ac.Pop, labels, ac.NewRegisterOperand(ac.R11))

		// pop the left-hand integer value (dividend) from the call stack into the RAX register (required by DIV)
		// note: all unsigned integer values must be correctly zero-extended to 64 bits before arithmetic operations
		e.assemblyCode.AppendInstruction(ac.Pop, nil, ac.NewRegisterOperand(ac.Rax))

		// zero-extend RAX to RDX:RAX by using the 'Xor' instruction for an unsigned division (clear RDX)
		e.assemblyCode.AppendInstruction(ac.Xor, nil, ac.NewRegisterOperand(ac.Rdx), ac.NewRegisterOperand(ac.Rdx))

		// divide the 128-bit dividend in RDX:RAX by the 64-bit divisor in R11 (unsigned division)
		// note: the quotient is stored in RAX and the remainder in RDX
		e.assemblyCode.AppendInstruction(ac.Div, nil, ac.NewRegisterOperand(ac.R11))

		// push the result onto the stack (quotient)
		e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.Rax))
	}
}

// Compare the top two elements of the call stack and update the CPU flags.
func (e *emitter) compare(dataType ic.DataType, labels []string) {
	// depending on the data type, the top two elements of the call stack are popped into the correct registers and compared
	switch dataType {
	case ic.Integer64, ic.Integer32, ic.Integer16, ic.Integer8, ic.Unicode, ic.Unsigned64, ic.Unsigned32, ic.Unsigned16, ic.Unsigned8, ic.Boolean:
		// pop the right-hand integer value from the call stack into the R11 register
		// note: all integer values must be extended to 64 bits before arithmetic operations
		//       - sign-extended for signed integers (includes Unicode)
		//       - zero-extended for unsigned integers (includes Boolean)
		e.assemblyCode.AppendInstruction(ac.Pop, labels, ac.NewRegisterOperand(ac.R11))

		// pop the left-hand integer value from the call stack into the R10 register
		// note: all integer values must be extended to 64 bits before arithmetic operations
		//       - sign-extended for signed integers (includes Unicode)
		//       - zero-extended for unsigned integers (includes Boolean)
		e.assemblyCode.AppendInstruction(ac.Pop, nil, ac.NewRegisterOperand(ac.R10))

		// compare the left-hand integer value in R10 with the right-hand integer value in R11 (R10 - R11)
		// note: the 'Cmp' instruction itself does not distinguish between signed and unsigned values
		//       conditional jump instructions interpret the CPU flags according to signedness
		e.assemblyCode.AppendInstruction(ac.Cmp, nil, ac.NewRegisterOperand(ac.R10), ac.NewRegisterOperand(ac.R11))

	case ic.Float64:
		// move the right-hand 64-bit floating-point value (IEEE 754 double precision) from the call stack into the XMM1 register
		e.assemblyCode.AppendInstruction(ac.Movsd, labels,
			ac.NewRegisterOperand(ac.Xmm1),
			ac.NewMemoryOperand(ac.Rsp, ac.Bits64, 0))

		// move the left-hand 64-bit floating-point value (IEEE 754 double precision) from the call stack into the XMM0 register
		e.assemblyCode.AppendInstruction(ac.Movsd, nil,
			ac.NewRegisterOperand(ac.Xmm0),
			ac.NewMemoryOperand(ac.Rsp, ac.Bits64, ac.PointerSize))

		// remove both top elements of the call stack (right-hand and left-hand value) by adjusting the stack pointer by 2 times the pointer size
		e.assemblyCode.AppendInstruction(ac.Add, nil,
			ac.NewRegisterOperand(ac.Rsp),
			ac.NewImmediateOperand(ac.Bits32, 2*ac.PointerSize))

		// compare the left-hand floating-point value in the XMM0 register with the right-hand floating-point value in the XMM1 register
		// note: the 'Ucomisd' instruction compares the two values and sets the CPU flags accordingly
		//       it does not raise an exception for NaN values, but sets the CPU flags to indicate unordered comparisons
		e.assemblyCode.AppendInstruction(ac.Ucomisd, nil, ac.NewRegisterOperand(ac.Xmm0), ac.NewRegisterOperand(ac.Xmm1))

	case ic.Float32:
		// move the right-hand 32-bit floating-point value (IEEE 754 single precision) from the call stack into the XMM1 register
		// note: the value is stored in a 64-bit stack slot (lower 32 bits contain the float)
		e.assemblyCode.AppendInstruction(ac.Movss, labels,
			ac.NewRegisterOperand(ac.Xmm1),
			ac.NewMemoryOperand(ac.Rsp, ac.Bits32, 0))

		// move the left-hand 32-bit floating-point value (IEEE 754 single precision) from the call stack into the XMM0 register
		// note: the value is stored in a 64-bit stack slot (lower 32 bits contain the float)
		e.assemblyCode.AppendInstruction(ac.Movss, nil,
			ac.NewRegisterOperand(ac.Xmm0),
			ac.NewMemoryOperand(ac.Rsp, ac.Bits32, ac.PointerSize))

		// remove both top elements of the call stack (right-hand and left-hand value) by adjusting the stack pointer by 2 times the pointer size
		e.assemblyCode.AppendInstruction(ac.Add, nil,
			ac.NewRegisterOperand(ac.Rsp),
			ac.NewImmediateOperand(ac.Bits32, 2*ac.PointerSize))

		// compare the left-hand floating-point value in the XMM0 register with the right-hand floating-point value in the XMM1 register
		// note: the 'Ucomiss' instruction compares the two values and sets the CPU flags accordingly
		//       it does not raise an exception for NaN values, but sets the CPU flags to indicate unordered comparisons
		e.assemblyCode.AppendInstruction(ac.Ucomiss, nil, ac.NewRegisterOperand(ac.Xmm0), ac.NewRegisterOperand(ac.Xmm1))
	}
}
