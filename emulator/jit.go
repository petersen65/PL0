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

// UnusedDifference states that an instruction does not use a block nesting depth difference.
const unusedDifference = -1

// NoLabel is a constant for an empty label in an instruction.
const noLabel = ""

// JIT compile a module into the text section of a process and return an error if the module fails to compile.
func (p *process) jitCompile(module cod.Module) (err error) {
	p.text = make(textSection, 0)
	iterator := module.GetIterator()

	// compile-time stack of parameters for runtime calls (procedures do not support parameters yet)
	parameters := list.New()

	// protect the JIT-compiler against non-valid structured intermediate code
	defer func() {
		if r := recover(); r != nil {
			err = cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, recoveredFromIllegalIntermediateCode, r, nil)
		}
	}()

	// iterate over the module's code and compile it into the text section of the process
	// the JIT compilation process translates intermediate code instructions into pseudo-assembly instructions
	for i, l := iterator.First(), make([]string, 0); i != nil; i = iterator.Next() {
		switch i.Code.Operation {
		// append labels for the directly following instruction
		case cod.Branch:
			l = append(l, i.Label)

		case cod.Allocate:
			// group consecutive intermediate code allocate operations into one alloc instruction
			for j := 0; ; j++ {
				if iterator.Peek(j).Code.Operation != cod.Allocate {
					p.appendInstruction(
						newInstruction(sub, unusedDifference, l, newOperand(registerOperand, rsp), newOperand(immediateOperand, int64(j+1))))

					iterator.Skip(j)
					break
				}
			}

		case cod.ValueCopy:
			// push an immediate value onto the runtime CPU stack
			switch i.Code.Arg1.DataType {
			case cod.Integer64:
				if arg, err := strconv.ParseInt(i.Code.Arg1.Variable, 10, integerBitSize); err != nil {
					return newParsingError(i.Code.Arg1.Variable, err)
				} else {
					p.appendInstruction(newInstruction(push, unusedDifference, l, newOperand(immediateOperand, arg)))
				}

			default:
				return validateDataType(cod.Integer64, i.Code.Arg1.DataType)
			}

		case cod.VariableLoad:
			// load a variable from its runtime CPU stack address onto the top of the stack
			switch i.Code.Arg1.DataType {
			case cod.Integer64:
				p.appendInstruction(
					newInstruction(loadvar, i.DepthDifference, l, newOperand(immediateOperand, int64(i.Code.Arg1.Offset))))

			default:
				return validateDataType(cod.Integer64, i.Code.Arg1.DataType)
			}

		case cod.VariableStore:
			// store the top of the runtime CPU stack into a variable's stack address
			switch i.Code.Result.DataType {
			case cod.Integer64:
				p.appendInstruction(
					newInstruction(storevar, i.DepthDifference, l, newOperand(immediateOperand, int64(i.Code.Result.Offset))))

			default:
				return validateDataType(cod.Integer64, i.Code.Result.DataType)
			}

		case cod.Negate:
			// negate the top of the runtime CPU stack and leave the result on the stack
			if err := validateDataType(cod.Integer64, i.Code.Arg1.DataType); err != nil {
				return err
			}

			p.appendInstruction(newInstruction(pop, unusedDifference, l, newOperand(registerOperand, rax)))
			p.appendInstruction(newInstruction(neg, unusedDifference, l, newOperand(registerOperand, rax)))
			p.appendInstruction(newInstruction(push, unusedDifference, l, newOperand(registerOperand, rax)))

		case cod.Odd:
			// check if the top of the runtime CPU stack is an odd number and leave the result in the CPU flags register
			if err := validateDataType(cod.Integer64, i.Code.Arg1.DataType); err != nil {
				return err
			}

			p.appendInstruction(newInstruction(pop, unusedDifference, l, newOperand(registerOperand, rax)))
			p.appendInstruction(newInstruction(and, unusedDifference, l, newOperand(registerOperand, rax)))

		case cod.Plus, cod.Minus, cod.Times, cod.Divide:
			// perform an arithmetic operation on the top two elements of the runtime CPU stack and replace them with the result
			if err := validateDataType(cod.Integer64, i.Code.Arg1.DataType, i.Code.Arg2.DataType); err != nil {
				return err
			}

			p.appendInstruction(newInstruction(pop, unusedDifference, l, newOperand(registerOperand, rbx)))
			p.appendInstruction(newInstruction(pop, unusedDifference, l, newOperand(registerOperand, rax)))

			switch i.Code.Operation {
			case cod.Plus:
				p.appendInstruction(
					newInstruction(add, unusedDifference, l, newOperand(registerOperand, rax), newOperand(registerOperand, rbx)))

			case cod.Minus:
				p.appendInstruction(
					newInstruction(sub, unusedDifference, l, newOperand(registerOperand, rax), newOperand(registerOperand, rbx)))

			case cod.Times:
				p.appendInstruction(
					newInstruction(imul, unusedDifference, l, newOperand(registerOperand, rax), newOperand(registerOperand, rbx)))

			case cod.Divide:
				p.appendInstruction(
					newInstruction(idiv, unusedDifference, l, newOperand(registerOperand, rax), newOperand(registerOperand, rbx)))
			}

			p.appendInstruction(newInstruction(push, unusedDifference, l, newOperand(registerOperand, rax)))

		case cod.Equal, cod.NotEqual, cod.Less, cod.LessEqual, cod.Greater, cod.GreaterEqual:
			// compare the top two elements of the runtime CPU stack, remove them, and leave the result in the CPU flags register
			if err := validateDataType(cod.Integer64, i.Code.Arg1.DataType, i.Code.Arg2.DataType); err != nil {
				return err
			}

			p.appendInstruction(newInstruction(pop, unusedDifference, l, newOperand(registerOperand, rbx)))
			p.appendInstruction(newInstruction(pop, unusedDifference, l, newOperand(registerOperand, rax)))
			p.appendInstruction(newInstruction(cmp, unusedDifference, l, newOperand(registerOperand, rax), newOperand(registerOperand, rbx)))

		case cod.Jump:
			// unconditionally jump to a label that is resolved by the linker
			p.appendInstruction(newInstruction(jmp, unusedDifference, l, newOperand(labelOperand, i.Code.Arg1.Variable)))

		case cod.JumpEqual:
			// jump to a label if the CPU flags register indicates that the top two elements of the stack were equal
			p.appendInstruction(newInstruction(je, unusedDifference, l, newOperand(labelOperand, i.Code.Arg1.Variable)))

		case cod.JumpNotEqual:
			// jump to a label if the CPU flags register indicates that the top two elements of the stack were not equal
			p.appendInstruction(newInstruction(jne, unusedDifference, l, newOperand(labelOperand, i.Code.Arg1.Variable)))

		case cod.JumpLess:
			// jump to a label if the CPU flags register indicates that the first element of the stack was less than the second top element
			p.appendInstruction(newInstruction(jl, unusedDifference, l, newOperand(labelOperand, i.Code.Arg1.Variable)))

		case cod.JumpLessEqual:
			// jump to a label if the CPU flags register indicates that the first element of the stack was less than or equal to the second top element
			p.appendInstruction(newInstruction(jle, unusedDifference, l, newOperand(labelOperand, i.Code.Arg1.Variable)))

		case cod.JumpGreater:
			// jump to a label if the CPU flags register indicates that the first element of the stack was greater than the second top element
			p.appendInstruction(newInstruction(jg, unusedDifference, l, newOperand(labelOperand, i.Code.Arg1.Variable)))

		case cod.JumpGreaterEqual:
			// jump to a label if the CPU flags register indicates that the first element of the stack was greater than or equal to the second top element
			p.appendInstruction(newInstruction(jge, unusedDifference, l, newOperand(labelOperand, i.Code.Arg1.Variable)))

		case cod.Parameter:
			// push a parameter onto the compile-time stack for a runtime function call
			parameters.PushBack(i)

		case cod.Call:
			// call a function with 0 arguments by jumping to the function's label (intermediate code procedures do not support parameters yet)
			// intermediate code procedures are named emulator target functions in the pseudo-assembly code
			if i.Code.Arg1.DataType != cod.UnsignedInteger64 {
				return validateDataType(cod.UnsignedInteger64, i.Code.Arg1.DataType)
			}

			if arg, err := strconv.ParseUint(i.Code.Arg1.Variable, 10, integerBitSize); err != nil {
				return newParsingError(i.Code.Arg1.Variable, err)
			} else if arg != 0 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unexpectedNumberOfFunctionArguments, arg, nil)
			} else {
				p.appendInstruction(newInstruction(call, i.DepthDifference, l, newOperand(labelOperand, i.Code.Arg2.Variable)))
			}

		case cod.Runtime:
			// a runtime function represents a function that is provided by the runtime library of the programming language
			// the runtime function has 2 direct arguments that are part of the intermediate code instruction at compile-time
			// the first argument is the number of arguments that the runtime function expects
			// the second argument is the call code of the runtime function in the programming language's runtime library
			// current runtime functions expect 1 existing parameter on the runtime CPU stack
			
			// pop a parameter from the compile-time stack that represents the data type of the expected parameter on the runtime CPU stack
			param := parameters.Back().Value.(*cod.Instruction)
			parameters.Remove(parameters.Back())

			// extract direct arguments 1 and 2 from the intermediate code instruction
			arg, err1 := strconv.ParseUint(i.Code.Arg1.Variable, 10, integerBitSize)
			rtc, err2 := strconv.ParseInt(i.Code.Arg2.Variable, 10, integerBitSize)

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
				p.appendInstruction(newInstruction(rcall, unusedDifference, l, newOperand(immediateOperand, rtc)))
			}

		case cod.Return:
			// return from a function to its caller
			p.appendInstruction(newInstruction(ret, unusedDifference, l))

		default:
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unknownIntermediateCodeOperation, i.Code.Operation, nil)
		}

		// collected labels must be used by the directly following instruction 
		// one instruction consumes all collected labels before it comes
		if i.Code.Operation != cod.Branch {
			l = make([]string, 0)
		}
	}

	// link the pseudo-assembly instructions
	return p.linker()
}

// Append an instruction to the end of the text section of the process.
func (p *process) appendInstruction(instruction *instruction) {
	p.text = append(p.text, instruction)
}

// The linker resolves jump and call label references to absolut code addresses in emulator target pseudo-assembly code.
func (p *process) linker() error {
	labels := make(map[string]uint64)

	// create a map of labels and their absolute addresses
	for i := range p.text {
		for _, label := range p.text[i].Labels {
			if label != noLabel {
				labels[label] = uint64(i)
			}
		}

		// set the address of every pseudo-assembly instruction for display purposes
		p.text[i].Address = uint64(i)
	}

	// resolve jump and call label references to absolute code addresses
	for _, pasm := range p.text {
		switch pasm.Operation {
		case call, jmp, je, jne, jl, jle, jg, jge:
			if address, ok := labels[pasm.Operands[0].Label]; !ok {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unresolvedLabelReference, pasm.Operands[0].Label, nil)
			} else {
				pasm.Operands[0] = newOperand(jumpOperand, address)
			}
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
