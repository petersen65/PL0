// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emulator

import (
	"container/list"
	"strconv"

	cod "github.com/petersen65/PL0/v2/code"
	cor "github.com/petersen65/PL0/v2/core"
)

// Number of bits of a signed integer.
const integerBitSize = 64

// JIT compile a module into the text section of a process and return an error if the module fails to compile.
func (p *process) jitCompile(module cod.Module) (err error) {
	p.text = make(textSection, 0)
	iterator := module.GetIterator()

	// compile-time stack of parameters for standard library function calls (procedures do not support parameters yet)
	parameters := list.New()

	// protect the JIT-compiler against non-valid structured intermediate code
	defer func() {
		if r := recover(); r != nil {
			err = cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, recoveredFromIllegalIntermediateCode, r, nil)
		}
	}()

	// iterate over the module's code and compile it into the text section of the process
	// the JIT compilation process translates intermediate code instructions into assembly instructions
	for i, l := iterator.First(), make([]string, 0); i != nil; i = iterator.Next() {
		switch i.Code.Operation {
		// append labels for the directly following instruction
		case cod.Branch:
			l = append(l, i.Label)

		case cod.Allocate: // allocate stack space for all local variables
			// group consecutive intermediate code allocate operations into one alloc instruction
			for j := 0; ; j++ {
				if iterator.Peek(j).Code.Operation != cod.Allocate {
					p.appendInstruction(sub, l, newOperand(registerOperand, rsp), newOperand(immediateOperand, int64(j+1)))
					iterator.Skip(j)
					break
				}
			}

		case cod.Prelude: // function body prelude
			// save caller's base pointer because it will be changed
			// this creates a dynamic link chain of base pointers so that each callee knows the base pointer of its caller
			p.appendInstruction(push, l, newOperand(registerOperand, rbp))

			// new base pointer points to start of local variables
			p.appendInstruction(mov, nil, newOperand(registerOperand, rbp), newOperand(registerOperand, rsp))

			// call runtime library function to create static link which provides the compile-time block nesting hierarchy at runtime
			p.appendInstruction(call, nil, newOperand(labelOperand, createStaticLinkLabel))

		case cod.Epilog: // function body epilog
			// clean allocated local variables
			p.appendInstruction(mov, l, newOperand(registerOperand, rsp), newOperand(registerOperand, rbp))

			// restore caller's base pointer
			p.appendInstruction(pop, nil, newOperand(registerOperand, rbp))

		case cod.ValueCopy: // push an immediate value onto the runtime CPU stack
			switch i.Code.Arg1.DataType {
			case cod.Integer64:
				if arg, err := strconv.ParseInt(i.Code.Arg1.Variable, 10, integerBitSize); err != nil {
					return newParsingError(i.Code.Arg1.Variable, err)
				} else {
					p.appendInstruction(push, l, newOperand(immediateOperand, arg))
				}

			default:
				return validateDataType(cod.Integer64, i.Code.Arg1.DataType)
			}

		case cod.VariableLoad: // load a variable from its runtime CPU stack address onto the top of the stack
			switch i.Code.Arg1.DataType {
			case cod.Integer64:
				if i.DepthDifference == 0 {
					// push memory content at 'variables base - variable offset' onto runtime CPU stack
					p.appendInstruction(push, l, newOperand(memoryOperand, rbp, -int64(i.Code.Arg1.Offset)))
				} else {
					// block nesting depth difference between variable use and variable declaration
					p.appendInstruction(mov, l,
						newOperand(registerOperand, rcx),
						newOperand(immediateOperand, int64(i.DepthDifference)))

					// call runtime library function to follow static link to determine the 'variables base' pointer
					p.appendInstruction(call, nil, newOperand(labelOperand, followStaticLinkLabel))

					// push memory content at 'variables base - variable offset' onto runtime CPU stack
					p.appendInstruction(push, nil, newOperand(memoryOperand, rbx, -int64(i.Code.Arg1.Offset)))
				}

			default:
				return validateDataType(cod.Integer64, i.Code.Arg1.DataType)
			}

		case cod.VariableStore: // store the top of the runtime CPU stack into a variable's stack address
			switch i.Code.Result.DataType {
			case cod.Integer64:
				if i.DepthDifference == 0 {
					// pop content of the variable
					p.appendInstruction(pop, l, newOperand(registerOperand, rax))

					// copy content of the variable into memory location 'variables base - variable offset'
					p.appendInstruction(mov, nil,
						newOperand(memoryOperand, rbp, -int64(i.Code.Result.Offset)),
						newOperand(registerOperand, rax))
				} else {
					// block nesting depth difference between variable use and variable declaration
					p.appendInstruction(mov, l,
						newOperand(registerOperand, rcx),
						newOperand(immediateOperand, int64(i.DepthDifference)))

					// call runtime library function to follow static link to determine the 'variables base' pointer
					p.appendInstruction(call, nil, newOperand(labelOperand, followStaticLinkLabel))

					// pop content of the variable
					p.appendInstruction(pop, nil, newOperand(registerOperand, rax))

					// copy content of the variable into memory location 'variables base - variable offset'
					p.appendInstruction(mov, nil,
						newOperand(memoryOperand, rbx, -int64(i.Code.Result.Offset)),
						newOperand(registerOperand, rax))
				}

			default:
				return validateDataType(cod.Integer64, i.Code.Result.DataType)
			}

		case cod.Negate: // negate the top of the runtime CPU stack and leave the result on the stack
			if err := validateDataType(cod.Integer64, i.Code.Arg1.DataType); err != nil {
				return err
			}

			p.appendInstruction(pop, l, newOperand(registerOperand, rax))
			p.appendInstruction(neg, nil, newOperand(registerOperand, rax))
			p.appendInstruction(push, nil, newOperand(registerOperand, rax))

		case cod.Odd: // check if the top of the runtime CPU stack is an odd number and leave the result in the CPU flags register
			if err := validateDataType(cod.Integer64, i.Code.Arg1.DataType); err != nil {
				return err
			}

			p.appendInstruction(pop, l, newOperand(registerOperand, rax))
			p.appendInstruction(and, nil, newOperand(registerOperand, rax))

		case cod.Plus, cod.Minus, cod.Times, cod.Divide:
			// perform an arithmetic operation on the top two elements of the runtime CPU stack and replace them with the result
			if err := validateDataType(cod.Integer64, i.Code.Arg1.DataType, i.Code.Arg2.DataType); err != nil {
				return err
			}

			p.appendInstruction(pop, l, newOperand(registerOperand, rbx))
			p.appendInstruction(pop, nil, newOperand(registerOperand, rax))

			switch i.Code.Operation {
			case cod.Plus:
				p.appendInstruction(add, nil, newOperand(registerOperand, rax), newOperand(registerOperand, rbx))

			case cod.Minus:
				p.appendInstruction(sub, nil, newOperand(registerOperand, rax), newOperand(registerOperand, rbx))

			case cod.Times:
				p.appendInstruction(imul, nil, newOperand(registerOperand, rax), newOperand(registerOperand, rbx))

			case cod.Divide:
				p.appendInstruction(idiv, nil, newOperand(registerOperand, rax), newOperand(registerOperand, rbx))
			}

			p.appendInstruction(push, nil, newOperand(registerOperand, rax))

		case cod.Equal, cod.NotEqual, cod.Less, cod.LessEqual, cod.Greater, cod.GreaterEqual:
			// compare the top two elements of the runtime CPU stack, remove them, and leave the result in the CPU flags register
			if err := validateDataType(cod.Integer64, i.Code.Arg1.DataType, i.Code.Arg2.DataType); err != nil {
				return err
			}

			p.appendInstruction(pop, l, newOperand(registerOperand, rbx))
			p.appendInstruction(pop, nil, newOperand(registerOperand, rax))
			p.appendInstruction(cmp, nil, newOperand(registerOperand, rax), newOperand(registerOperand, rbx))

		case cod.Jump: // unconditionally jump to a label that is resolved by the linker
			p.appendInstruction(jmp, l, newOperand(labelOperand, i.Code.Arg1.Variable))

		case cod.JumpEqual:
			// jump to a label if the CPU flags register indicates that the top two elements of the stack were equal
			p.appendInstruction(je, l, newOperand(labelOperand, i.Code.Arg1.Variable))

		case cod.JumpNotEqual:
			// jump to a label if the CPU flags register indicates that the top two elements of the stack were not equal
			p.appendInstruction(jne, l, newOperand(labelOperand, i.Code.Arg1.Variable))

		case cod.JumpLess:
			// jump to a label if the CPU flags register indicates that the first element of the stack was less than the second top element
			p.appendInstruction(jl, l, newOperand(labelOperand, i.Code.Arg1.Variable))

		case cod.JumpLessEqual:
			// jump to a label if the CPU flags register indicates that the first element of the stack was less than or equal to the second top element
			p.appendInstruction(jle, l, newOperand(labelOperand, i.Code.Arg1.Variable))

		case cod.JumpGreater:
			// jump to a label if the CPU flags register indicates that the first element of the stack was greater than the second top element
			p.appendInstruction(jg, l, newOperand(labelOperand, i.Code.Arg1.Variable))

		case cod.JumpGreaterEqual:
			// jump to a label if the CPU flags register indicates that the first element of the stack was greater than or equal to the second top element
			p.appendInstruction(jge, l, newOperand(labelOperand, i.Code.Arg1.Variable))

		case cod.Parameter: // push a parameter onto the compile-time stack for a standard library function call
			parameters.PushBack(i)

		case cod.Call: // call a function with 0 arguments by jumping to the function's label
			if i.Code.Arg1.DataType != cod.UnsignedInteger64 {
				return validateDataType(cod.UnsignedInteger64, i.Code.Arg1.DataType)
			}

			if arg, err := strconv.ParseUint(i.Code.Arg1.Variable, 10, integerBitSize); err != nil {
				return newParsingError(i.Code.Arg1.Variable, err)
			} else if arg != 0 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unexpectedNumberOfFunctionArguments, arg, nil)
			} else {
				// push difference between use depth and declaration depth on runtime CPU stack
				p.appendInstruction(push, l, newOperand(immediateOperand, int64(i.DepthDifference)))

				// push return address on runtime CPU stack and jump to callee
				p.appendInstruction(call, nil, newOperand(labelOperand, i.Code.Arg2.Variable))

				// remove difference from runtime CPU stack after return from callee
				p.appendInstruction(add, nil, newOperand(registerOperand, rsp), newOperand(immediateOperand, int64(1)))
			}

		case cod.Return: // return from a function to its caller
			p.appendInstruction(ret, nil)

		case cod.Standard:
			// a standard function represents a function that is provided by the standard library of the programming language
			// the standard function has 2 direct arguments that are part of the intermediate code instruction at compile-time
			// the first argument is the number of arguments that the standard function expects
			// the second argument is the call code of the standard function in the programming language's standard library
			// current standard library functions expect 1 existing parameter on the runtime CPU stack

			// pop a parameter from the compile-time stack that represents the data type of the expected parameter on the runtime CPU stack
			param := parameters.Back().Value.(*cod.Instruction)
			parameters.Remove(parameters.Back())

			// extract direct arguments 1 and 2 from the intermediate code instruction
			arg, err1 := strconv.ParseUint(i.Code.Arg1.Variable, 10, integerBitSize)
			stdc, err2 := strconv.ParseInt(i.Code.Arg2.Variable, 10, integerBitSize)

			switch {
			case param.Code.Arg1.DataType != cod.Integer64:
				return validateDataType(cod.Integer64, param.Code.Arg1.DataType)

			case i.Code.Arg1.DataType != cod.UnsignedInteger64 || i.Code.Arg2.DataType != cod.UnsignedInteger64:
				return validateDataType(cod.UnsignedInteger64, i.Code.Arg1.DataType, i.Code.Arg2.DataType)

			case err1 != nil:
				return newParsingError(i.Code.Arg1.Variable, err1)

			case err2 != nil:
				return newParsingError(i.Code.Arg2.Variable, err2)

			case arg != 1:
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unexpectedNumberOfFunctionArguments, arg, nil)

			default:
				p.appendInstruction(stdcall, l, newOperand(immediateOperand, stdc))
			}

		default:
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unknownIntermediateCodeOperation, i.Code.Operation, nil)
		}

		// collected labels must be used by the directly following instruction (one instruction consumes all collected labels)
		if i.Code.Operation != cod.Branch {
			l = make([]string, 0)
		}
	}

	return nil
}

// Validate the data type of the actuals and return an error if the data type is not as expected.
func validateDataType(expected cod.DataType, actuals ...cod.DataType) error {
	for _, actual := range actuals {
		if actual != expected {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperandDataType, actual, nil)
		}
	}

	return nil
}

// Create a new parsing error with the given value and inner error.
func newParsingError(value any, inner error) error {
	return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, operandParsingError, value, inner)
}
