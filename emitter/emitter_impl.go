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

// Runtime control stack must be aligned to 16 bytes before each function call.
const runtimeControlStackAlignment = 16

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
		ic.Rune32:     ac.Bits32,
		ic.Boolean8:   ac.Bits8,
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
			// save caller's base pointer because it will be changed
			// this creates a 'dynamic link' chain of base pointers so that each callee knows the base pointer of its caller
			// an alternative naming from literature is 'control link' that points to the activation record of the caller
			e.assemblyCode.AppendInstruction(ac.Push, l, ac.NewRegisterOperand(ac.Rbp))

			// new base pointer points to start of local variables in the activation record
			e.assemblyCode.AppendInstruction(ac.Mov, nil, ac.NewRegisterOperand(ac.Rbp), ac.NewRegisterOperand(ac.Rsp))

		case ic.Epilogue: // function exit sequence
			// clean allocated local variables from the activation record
			e.assemblyCode.AppendInstruction(ac.Mov, l, ac.NewRegisterOperand(ac.Rsp), ac.NewRegisterOperand(ac.Rbp))

			// restore caller's base pointer
			e.assemblyCode.AppendInstruction(ac.Pop, nil, ac.NewRegisterOperand(ac.Rbp))

		case ic.Setup:
			// panic if parsing of the metadata into its value fails (unsupported value or data type)
			depth := i.ThreeAddressCode.Arg1.Parse().(int32)

			// the main block has no parent procedure declaration
			if depth > 0 {
				// call runtime function to create static link which provides the compile-time block nesting hierarchy at runtime
				e.assemblyCode.AppendInstruction(ac.Call, nil, ac.NewLabelOperand(ac.CreateStaticLinkLabel))
			}

		case ic.ValueCopy: // copy an immediate value to an address
			// panic if parsing of the literal into its value fails (unsupported value or data type)
			value := i.ThreeAddressCode.Arg1.Parse()

			// emit assembly code to copy the value onto the top of the runtime control stack
			e.valueCopy(value, i.ThreeAddressCode.Arg1.DataType, l)

		case ic.VariableLoad: // load a variable from its runtime control stack address onto the top of the stack
			// panic if parsing of the variable into nil fails (unsupported data type)
			i.ThreeAddressCode.Arg1.Parse()

			// determinde offset of the local variable in its activation record
			offset := e.offsetTable[i.ThreeAddressCode.Arg1.Name]

			if i.DepthDifference == 0 {
				// push memory content at 'variables base - variable offset' onto runtime control stack
				e.assemblyCode.AppendInstruction(ac.Push, l, ac.NewMemoryOperand(ac.Rbp, ac.Bits64, offset))
			} else {
				// block nesting depth difference between variable use and variable declaration
				e.assemblyCode.AppendInstruction(ac.Mov, l,
					ac.NewRegisterOperand(ac.Rcx),
					ac.NewImmediateOperand(ac.Bits64, int64(i.DepthDifference)))

				// call runtime function to follow static link to determine the 'variables base' pointer
				e.assemblyCode.AppendInstruction(ac.Call, nil, ac.NewLabelOperand(ac.FollowStaticLinkLabel))

				// push memory content at 'variables base - variable offset' onto runtime control stack
				e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewMemoryOperand(ac.Rbx, ac.Bits64, offset))
			}

		case ic.VariableStore: // store the top of the runtime control stack into a variable's stack address
			// panic if parsing of the variable into nil fails (unsupported data type)
			i.ThreeAddressCode.Result.Parse()

			// determine offset of the local variable in its activation record
			offset := e.offsetTable[i.ThreeAddressCode.Result.Name]

			if i.DepthDifference == 0 {
				// pop content of the variable
				e.assemblyCode.AppendInstruction(ac.Pop, l, ac.NewRegisterOperand(ac.Rax))

				// copy content of the variable into memory at 'variables base - variable offset'
				e.assemblyCode.AppendInstruction(ac.Mov, nil,
					ac.NewMemoryOperand(ac.Rbp, ac.Bits64, offset),
					ac.NewRegisterOperand(ac.Rax))
			} else {
				// block nesting depth difference between variable use and variable declaration
				e.assemblyCode.AppendInstruction(ac.Mov, l,
					ac.NewRegisterOperand(ac.Rcx),
					ac.NewImmediateOperand(ac.Bits64, int64(i.DepthDifference)))

				// call runtime function to follow static link to determine the 'variables base' pointer
				e.assemblyCode.AppendInstruction(ac.Call, nil, ac.NewLabelOperand(ac.FollowStaticLinkLabel))

				// pop content of the variable
				e.assemblyCode.AppendInstruction(ac.Pop, nil, ac.NewRegisterOperand(ac.Rax))

				// copy content of the variable into memory at 'variables base - variable offset'
				e.assemblyCode.AppendInstruction(ac.Mov, nil,
					ac.NewMemoryOperand(ac.Rbx, ac.Bits64, offset),
					ac.NewRegisterOperand(ac.Rax))
			}

		case ic.Negate: // negate the top of the runtime control stack and leave the result on the stack
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.ThreeAddressCode.Arg1.Parse()

			e.assemblyCode.AppendInstruction(ac.Pop, l, ac.NewRegisterOperand(ac.Rax))
			e.assemblyCode.AppendInstruction(ac.Neg, nil, ac.NewRegisterOperand(ac.Rax))
			e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.Rax))

		case ic.Odd: // check if the top of the runtime control stack is an odd number and leave the result in the CPU flags register
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.ThreeAddressCode.Arg1.Parse()

			e.assemblyCode.AppendInstruction(ac.Pop, l, ac.NewRegisterOperand(ac.Rax))
			e.assemblyCode.AppendInstruction(ac.And, nil, ac.NewRegisterOperand(ac.Rax))

		case ic.Plus, ic.Minus, ic.Times, ic.Divide: // perform an arithmetic operation on the top two elements of the runtime control stack and replace them with one result on the stack
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.ThreeAddressCode.Arg1.Parse()
			_ = i.ThreeAddressCode.Arg2.Parse()

			e.assemblyCode.AppendInstruction(ac.Pop, l, ac.NewRegisterOperand(ac.Rbx))
			e.assemblyCode.AppendInstruction(ac.Pop, nil, ac.NewRegisterOperand(ac.Rax))

			switch i.ThreeAddressCode.Operation {
			case ic.Plus:
				e.assemblyCode.AppendInstruction(ac.Add, nil, ac.NewRegisterOperand(ac.Rax), ac.NewRegisterOperand(ac.Rbx))

			case ic.Minus:
				e.assemblyCode.AppendInstruction(ac.Sub, nil, ac.NewRegisterOperand(ac.Rax), ac.NewRegisterOperand(ac.Rbx))

			case ic.Times:
				e.assemblyCode.AppendInstruction(ac.Imul, nil, ac.NewRegisterOperand(ac.Rax), ac.NewRegisterOperand(ac.Rbx))

			case ic.Divide:
				e.assemblyCode.AppendInstruction(ac.Idiv, nil, ac.NewRegisterOperand(ac.Rax), ac.NewRegisterOperand(ac.Rbx))
			}

			e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.Rax))

		case ic.Equal, ic.NotEqual, ic.Less, ic.LessEqual, ic.Greater, ic.GreaterEqual: // compare the top two elements of the runtime control stack, remove them, and leave the result in the CPU flags register
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.ThreeAddressCode.Arg1.Parse()
			_ = i.ThreeAddressCode.Arg2.Parse()

			e.assemblyCode.AppendInstruction(ac.Pop, l, ac.NewRegisterOperand(ac.Rbx))
			e.assemblyCode.AppendInstruction(ac.Pop, nil, ac.NewRegisterOperand(ac.Rax))
			e.assemblyCode.AppendInstruction(ac.Cmp, nil, ac.NewRegisterOperand(ac.Rax), ac.NewRegisterOperand(ac.Rbx))

		case ic.Jump: // unconditionally jump to a label that is resolved by the linker
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.ThreeAddressCode.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(ac.Jmp, l, ac.NewLabelOperand(name))

		case ic.JumpEqual: // jump to a label if the CPU flags register indicates that the top two elements of the stack were equal
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.ThreeAddressCode.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(ac.Je, l, ac.NewLabelOperand(name))

		case ic.JumpNotEqual: // jump to a label if the CPU flags register indicates that the top two elements of the stack were not equal
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.ThreeAddressCode.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(ac.Jne, l, ac.NewLabelOperand(name))

		case ic.JumpLess: // jump to a label if the CPU flags register indicates that the first element of the stack was less than the second top element
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.ThreeAddressCode.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(ac.Jl, l, ac.NewLabelOperand(name))

		case ic.JumpLessEqual: // jump to a label if the CPU flags register indicates that the first element of the stack was less than or equal to the second top element
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.ThreeAddressCode.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(ac.Jle, l, ac.NewLabelOperand(name))

		case ic.JumpGreater: // jump to a label if the CPU flags register indicates that the first element of the stack was greater than the second top element
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.ThreeAddressCode.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(ac.Jg, l, ac.NewLabelOperand(name))

		case ic.JumpGreaterEqual: // jump to a label if the CPU flags register indicates that the first element of the stack was greater than or equal to the second top element
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
				// push difference between use depth and declaration depth on runtime control stack
				// e.assemblyCode.AppendInstruction(ac.Push, l, ac.NewImmediateOperand(ac.Bits64, int64(i.DepthDifference)))

				// move the difference between use depth and declaration depth as 32-bit signed integer into the R10d register
				e.assemblyCode.AppendInstruction(ac.Mov, l,
					ac.NewRegisterOperand(ac.R10d),
					ac.NewImmediateOperand(ac.Bits32, int32(i.DepthDifference)))

				// push return address on runtime control stack and jump to callee
				e.assemblyCode.AppendInstruction(ac.Call, nil, ac.NewLabelOperand(name))

				// remove difference from runtime control stack after return from callee
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
				// current standard library functions expect 1 parameter on the runtime control stack
				panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unexpectedNumberOfFunctionArguments, nil, nil))
			} else {
				// the top element of the runtime control stack is either consumed or updated by the standard library function
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
			offset = ac.Align(offset, runtimeControlStackAlignment)

			// grow the runtime control stack downwards to provide space for all local variables int the activiation record (2GB maximum)
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

// Copy an immediate value onto the top of the runtime control stack.
func (e *emitter) valueCopy(value any, dataType ic.DataType, labels []string) {
	// depending on the data type, the value is copied onto the runtime control stack as an immediate value or as a 64-bit value in the R10 register
	switch dataType {
	case ic.Integer64:
		// move the 64-bit signed integer into the R10 register without sign extension
		e.assemblyCode.AppendInstruction(ac.MovAbs, labels,
			ac.NewRegisterOperand(ac.R10),
			ac.NewImmediateOperand(ac.Bits64, value.(int64)))

		// push the R10 register onto the runtime control stack
		e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.R10))

	case ic.Integer32:
		// push the 32-bit signed integer onto the runtime control stack and sign-extend it to 64 bits
		e.assemblyCode.AppendInstruction(ac.Push, labels, ac.NewImmediateOperand(ac.Bits32, value.(int32)))

	case ic.Integer16:
		// convert the 16-bit signed integer to a 32-bit signed integer before pushing it onto the runtime control stack and sign-extend it to 64 bits
		e.assemblyCode.AppendInstruction(ac.Push, labels, ac.NewImmediateOperand(ac.Bits32, int32(value.(int16))))

	case ic.Integer8:
		// push the 8-bit signed integer onto the runtime control stack and sign-extend it to 64 bits
		e.assemblyCode.AppendInstruction(ac.Push, labels, ac.NewImmediateOperand(ac.Bits8, value.(int8)))

	case ic.Unsigned64:
		// move the 64-bit unsigned integer into the R10 register without sign extension
		e.assemblyCode.AppendInstruction(ac.MovAbs, labels,
			ac.NewRegisterOperand(ac.R10),
			ac.NewImmediateOperand(ac.Bits64, value.(uint64)))

		// push the R10 register onto the runtime control stack
		e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.R10))

	case ic.Unsigned32:
		// push the 32-bit unsigned integer onto the runtime control stack and zero-extend it to 64 bits
		e.assemblyCode.AppendInstruction(ac.Push, labels, ac.NewImmediateOperand(ac.Bits32, value.(uint32)))

	case ic.Unsigned16:
		// convert the 16-bit unsigned integer to a 32-bit unsigned integer before pushing it onto the runtime control stack and zero-extend it to 64 bits
		e.assemblyCode.AppendInstruction(ac.Push, labels, ac.NewImmediateOperand(ac.Bits32, uint32(value.(uint16))))

	case ic.Unsigned8:
		// push the 8-bit unsigned integer onto the runtime control stack and zero-extend it to 64 bits
		e.assemblyCode.AppendInstruction(ac.Push, labels, ac.NewImmediateOperand(ac.Bits8, value.(uint8)))

	case ic.Float64:
		// convert the 64-bit float value to its IEEE 754 binary representation
		binaryRepresentationIEEE754 := math.Float64bits(value.(float64))

		// move the 64-bit float value into the R10 register without any extension
		e.assemblyCode.AppendInstruction(ac.MovAbs, labels,
			ac.NewRegisterOperand(ac.R10),
			ac.NewImmediateOperand(ac.Bits64, binaryRepresentationIEEE754))

		// push the R10 register onto the runtime control stack
		e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.R10))

	case ic.Float32:
		// convert the 32-bit float value to its IEEE 754 binary representation
		binaryRepresentationIEEE754 := math.Float32bits(value.(float32))

		// move the 32-bit float value into the lower 32 bits of the R10 register (named R10d) and zero-extend the upper 32 bits
		e.assemblyCode.AppendInstruction(ac.Mov, labels,
			ac.NewRegisterOperand(ac.R10d),
			ac.NewImmediateOperand(ac.Bits32, binaryRepresentationIEEE754))

		// push the 64-bit R10 register onto the runtime control stack (32-bit float value was zero-extended to 64 bits)
		e.assemblyCode.AppendInstruction(ac.Push, nil, ac.NewRegisterOperand(ac.R10))

	case ic.Rune32:
		// convert the rune to a 32-bit signed integer before pushing it onto the runtime control stack
		e.assemblyCode.AppendInstruction(ac.Push, labels, ac.NewImmediateOperand(ac.Bits32, int32(value.(rune))))

	case ic.Boolean8:
		// convert the boolean value to an 8-bit signed integer before pushing it onto the runtime control stack
		if value.(bool) {
			e.assemblyCode.AppendInstruction(ac.Push, labels, ac.NewImmediateOperand(ac.Bits8, int8(1)))
		} else {
			e.assemblyCode.AppendInstruction(ac.Push, labels, ac.NewImmediateOperand(ac.Bits8, int8(0)))
		}
	}
}
