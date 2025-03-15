// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emitter

import (
	"container/list"

	cor "github.com/petersen65/PL0/v2/core"
	gen "github.com/petersen65/PL0/v2/generator"
)

// Implementation of the assembly code emitter.
type emitter struct {
	intermediateCode gen.IntermediateCodeUnit // intermediate code unit to generate assembly code for
	assemblyCode     *assemblyCodeUnit        // assembly code unit for the CPU target
	cpu              CentralProcessingUnit    // target CPU for the emitter
}

// Map CPU targets to their string representation.
var cpuNames = map[CentralProcessingUnit]string{
	Amd64: "amd64",
}

// Return the interface of the emitter implementation.
func newEmitter(cpu CentralProcessingUnit, intermediateCodeUnit gen.IntermediateCodeUnit) Emitter {
	if cpu != Amd64 {
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedCpuTarget, cpu, nil))
	}

	return &emitter{
		intermediateCode: intermediateCodeUnit,
		assemblyCode:     NewAssemblyCodeUnit().(*assemblyCodeUnit),
		cpu:              cpu,
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
		i.Code.ValidateAddressesContract()

		switch i.Code.Operation {
		case gen.Target: // append labels for the directly following non 'Target' instruction
			l = append(l, i.Label)

		case gen.Allocate: // allocate space in an activation record for all local variables
			// group consecutive intermediate code allocate operations into one alloc instruction
			for j := 0; ; j++ {
				if iterator.Peek(j).Code.Operation != gen.Allocate {
					e.assemblyCode.AppendInstruction(Sub, l,
						newOperand(RegisterOperand, Rsp),
						newOperand(ImmediateOperand, int64(j+1)))

					iterator.Skip(j)
					break
				}
			}

		case gen.Prelude: // function body prelude
			// save caller's base pointer because it will be changed
			// this creates a 'dynamic link' chain of base pointers so that each callee knows the base pointer of its caller
			// an alternative naming from literature is 'control link' that points to the activation record of the caller
			e.assemblyCode.AppendInstruction(Push, l, newOperand(RegisterOperand, Rbp))

			// new base pointer points to start of local variables in the activation record
			e.assemblyCode.AppendInstruction(Mov, nil, newOperand(RegisterOperand, Rbp), newOperand(RegisterOperand, Rsp))

			// call runtime library function to create static link which provides the compile-time block nesting hierarchy at runtime
			e.assemblyCode.AppendInstruction(Call, nil, newOperand(LabelOperand, CreateStaticLinkLabel))

		case gen.Epilog: // function body epilog
			// clean allocated local variables from the activation record
			e.assemblyCode.AppendInstruction(Mov, l, newOperand(RegisterOperand, Rsp), newOperand(RegisterOperand, Rbp))

			// restore caller's base pointer
			e.assemblyCode.AppendInstruction(Pop, nil, newOperand(RegisterOperand, Rbp))

		case gen.ValueCopy: // push an immediate value onto the runtime control stack
			// panic if parsing of the literal into its value fails (unsupported value or data type)
			value := i.Code.Arg1.Parse()
			e.assemblyCode.AppendInstruction(Push, l, newOperand(ImmediateOperand, value))

		case gen.VariableLoad: // load a variable from its runtime control stack address onto the top of the stack
			// panic if parsing of the variable into its location fails (unsupported data type)
			location := i.Code.Arg1.Parse().(uint64)
			detail := MemoryDetail{Size: Bits64, Displacement: -int64(location)}

			if i.DepthDifference == 0 {
				// push memory content at 'variables base - variable offset' onto runtime control stack
				e.assemblyCode.AppendInstruction(Push, l, newOperand(MemoryOperand, Rbp, detail))
			} else {
				// block nesting depth difference between variable use and variable declaration
				e.assemblyCode.AppendInstruction(Mov, l,
					newOperand(RegisterOperand, Rcx),
					newOperand(ImmediateOperand, int64(i.DepthDifference)))

				// call runtime library function to follow static link to determine the 'variables base' pointer
				e.assemblyCode.AppendInstruction(Call, nil, newOperand(LabelOperand, FollowStaticLinkLabel))

				// push memory content at 'variables base - variable offset' onto runtime control stack
				e.assemblyCode.AppendInstruction(Push, nil, newOperand(MemoryOperand, Rbx, detail))
			}

		case gen.VariableStore: // store the top of the runtime control stack into a variable's stack address
			// panic if parsing of the variable into its location fails (unsupported data type)
			location := i.Code.Result.Parse().(uint64)
			detail := MemoryDetail{Size: Bits64, Displacement: -int64(location)}

			if i.DepthDifference == 0 {
				// pop content of the variable
				e.assemblyCode.AppendInstruction(Pop, l, newOperand(RegisterOperand, Rax))

				// copy content of the variable into memory location 'variables base - variable offset'
				e.assemblyCode.AppendInstruction(Mov, nil,
					newOperand(MemoryOperand, Rbp, detail),
					newOperand(RegisterOperand, Rax))
			} else {
				// block nesting depth difference between variable use and variable declaration
				e.assemblyCode.AppendInstruction(Mov, l,
					newOperand(RegisterOperand, Rcx),
					newOperand(ImmediateOperand, int64(i.DepthDifference)))

				// call runtime library function to follow static link to determine the 'variables base' pointer
				e.assemblyCode.AppendInstruction(Call, nil, newOperand(LabelOperand, FollowStaticLinkLabel))

				// pop content of the variable
				e.assemblyCode.AppendInstruction(Pop, nil, newOperand(RegisterOperand, Rax))

				// copy content of the variable into memory location 'variables base - variable offset'
				e.assemblyCode.AppendInstruction(Mov, nil,
					newOperand(MemoryOperand, Rbx, detail),
					newOperand(RegisterOperand, Rax))
			}

		case gen.Negate: // negate the top of the runtime control stack and leave the result on the stack
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.Code.Arg1.Parse()

			e.assemblyCode.AppendInstruction(Pop, l, newOperand(RegisterOperand, Rax))
			e.assemblyCode.AppendInstruction(Neg, nil, newOperand(RegisterOperand, Rax))
			e.assemblyCode.AppendInstruction(Push, nil, newOperand(RegisterOperand, Rax))

		case gen.Odd: // check if the top of the runtime control stack is an odd number and leave the result in the CPU flags register
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.Code.Arg1.Parse()

			e.assemblyCode.AppendInstruction(Pop, l, newOperand(RegisterOperand, Rax))
			e.assemblyCode.AppendInstruction(And, nil, newOperand(RegisterOperand, Rax))

		case gen.Plus, gen.Minus, gen.Times, gen.Divide: // perform an arithmetic operation on the top two elements of the runtime control stack and replace them with one result on the stack
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.Code.Arg1.Parse()
			_ = i.Code.Arg2.Parse()

			e.assemblyCode.AppendInstruction(Pop, l, newOperand(RegisterOperand, Rbx))
			e.assemblyCode.AppendInstruction(Pop, nil, newOperand(RegisterOperand, Rax))

			switch i.Code.Operation {
			case gen.Plus:
				e.assemblyCode.AppendInstruction(Add, nil, newOperand(RegisterOperand, Rax), newOperand(RegisterOperand, Rbx))

			case gen.Minus:
				e.assemblyCode.AppendInstruction(Sub, nil, newOperand(RegisterOperand, Rax), newOperand(RegisterOperand, Rbx))

			case gen.Times:
				e.assemblyCode.AppendInstruction(Imul, nil, newOperand(RegisterOperand, Rax), newOperand(RegisterOperand, Rbx))

			case gen.Divide:
				e.assemblyCode.AppendInstruction(Idiv, nil, newOperand(RegisterOperand, Rax), newOperand(RegisterOperand, Rbx))
			}

			e.assemblyCode.AppendInstruction(Push, nil, newOperand(RegisterOperand, Rax))

		case gen.Equal, gen.NotEqual, gen.Less, gen.LessEqual, gen.Greater, gen.GreaterEqual: // compare the top two elements of the runtime control stack, remove them, and leave the result in the CPU flags register
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.Code.Arg1.Parse()
			_ = i.Code.Arg2.Parse()

			e.assemblyCode.AppendInstruction(Pop, l, newOperand(RegisterOperand, Rbx))
			e.assemblyCode.AppendInstruction(Pop, nil, newOperand(RegisterOperand, Rax))
			e.assemblyCode.AppendInstruction(Cmp, nil, newOperand(RegisterOperand, Rax), newOperand(RegisterOperand, Rbx))

		case gen.Jump: // unconditionally jump to a label that is resolved by the linker
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(Jmp, l, newOperand(LabelOperand, name))

		case gen.JumpEqual: // jump to a label if the CPU flags register indicates that the top two elements of the stack were equal
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(Je, l, newOperand(LabelOperand, name))

		case gen.JumpNotEqual: // jump to a label if the CPU flags register indicates that the top two elements of the stack were not equal
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(Jne, l, newOperand(LabelOperand, name))

		case gen.JumpLess: // jump to a label if the CPU flags register indicates that the first element of the stack was less than the second top element
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(Jl, l, newOperand(LabelOperand, name))

		case gen.JumpLessEqual: // jump to a label if the CPU flags register indicates that the first element of the stack was less than or equal to the second top element
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(Jle, l, newOperand(LabelOperand, name))

		case gen.JumpGreater: // jump to a label if the CPU flags register indicates that the first element of the stack was greater than the second top element
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(Jg, l, newOperand(LabelOperand, name))

		case gen.JumpGreaterEqual: // jump to a label if the CPU flags register indicates that the first element of the stack was greater than or equal to the second top element
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(Jge, l, newOperand(LabelOperand, name))

		case gen.Parameter: // push a parameter onto the compile-time parameters list for a standard library function call
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.Code.Arg1.Parse()
			parameters.PushBack(i)

		case gen.Call: // call a function with 0 arguments by jumping to the function's label
			// panic if parsing of the parameters count into an unsigned integer fails (unsupported value or data type)
			count := i.Code.Arg1.Parse().(uint64)

			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg2.Parse().(string)

			if count != 0 {
				// procedures do not support parameters yet
				panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unexpectedNumberOfFunctionArguments, nil, nil))
			} else {
				// push difference between use depth and declaration depth on runtime control stack
				e.assemblyCode.AppendInstruction(Push, l, newOperand(ImmediateOperand, int64(i.DepthDifference)))

				// push return address on runtime control stack and jump to callee
				e.assemblyCode.AppendInstruction(Call, nil, newOperand(LabelOperand, name))

				// remove difference from runtime control stack after return from callee
				e.assemblyCode.AppendInstruction(Add, nil, newOperand(RegisterOperand, Rsp), newOperand(ImmediateOperand, int64(1)))
			}

		case gen.Return: // return from a function to its caller
			e.assemblyCode.AppendInstruction(Ret, nil)

		case gen.Standard:
			// a standard function represents a function that is provided by the standard library of the programming language
			// the standard function has 2 direct parameters that are part of the intermediate code instruction at compile-time
			// the first parameter holds the number of parameters that the standard function expects
			// the second parameter holds the call code of the standard function in the programming language's standard library

			// panic if parsing of the parameters count into an unsigned integer fails (unsupported value or data type)
			count := i.Code.Arg1.Parse().(uint64)

			// panic if parsing of the call code into an integer fails (unsupported value or data type)
			code := i.Code.Arg2.Parse().(int64)

			// parameter instruction for the standard library function call
			pi := parameters.Back().Value.(*gen.Instruction)
			parameters.Remove(parameters.Back())

			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = pi.Code.Arg1.Parse()

			if count != 1 {
				// current standard library functions expect 1 parameter on the runtime control stack
				panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unexpectedNumberOfFunctionArguments, nil, nil))
			} else {
				// the top element of the runtime control stack is either consumed or updated by the standard library function
				e.assemblyCode.AppendInstruction(StdCall, l, newOperand(ImmediateOperand, code))
			}

		default:
			panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unknownIntermediateCodeOperation, i.Code.Operation, nil))
		}

		// collected labels must be used by the directly following instruction (one instruction consumes all collected labels)
		if i.Code.Operation != gen.Target {
			l = make([]string, 0)
		}
	}
}

// Get access to the generated assembly code.
func (e *emitter) GetAssemblyCodeUnit() AssemblyCodeUnit {
	return e.assemblyCode
}
