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
	parameters := list.New()

	// protect the JIT compiler against non-valid structured intermediate code
	defer func() {
		if r := recover(); r != nil {
			err = cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, recoveredFromIllegalIntermediateCode, r, nil)
		}
	}()

	// iterate over the module's code and compile it into the text section of the process
	for i, l := iterator.First(), noLabel; i != nil; i = iterator.Next() {
		switch i.Code.Operation {
		case cod.Branch:
			// if next := iterator.Peek(1); l != noLabel || next != nil && next.Code.Operation == cod.Branch {
			// 	return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, consecutiveBranchOperationsNotSupported, nil, nil)
			// }

			l = i.Label // save label for directly following instruction

		case cod.Allocate:
			// group consecutive intermediate code allocate operations into one alloc instruction
			for j := 0; ; j++ {
				if iterator.Peek(j).Code.Operation != cod.Allocate {
					p.appendInstruction(newInstruction(alloc, unusedDifference, l, newOperand(addressOperand, uint64(j+1))))
					iterator.Skip(j)
					break
				}
			}

		case cod.ValueCopy:
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
			switch i.Code.Arg1.DataType {
			case cod.Integer64:
				p.appendInstruction(newInstruction(loadvar, i.DepthDifference, l, newOperand(addressOperand, i.Code.Arg1.Offset)))

			default:
				return validateDataType(cod.Integer64, i.Code.Arg1.DataType)
			}

		case cod.VariableStore:
			switch i.Code.Result.DataType {
			case cod.Integer64:
				p.appendInstruction(newInstruction(storevar, i.DepthDifference, l, newOperand(addressOperand, i.Code.Result.Offset)))

			default:
				return validateDataType(cod.Integer64, i.Code.Result.DataType)
			}

		case cod.Negate:
			if err := validateDataType(cod.Integer64, i.Code.Arg1.DataType); err != nil {
				return err
			}

			p.appendInstruction(newInstruction(neg, unusedDifference, l))

		case cod.Odd:
			if err := validateDataType(cod.Integer64, i.Code.Arg1.DataType); err != nil {
				return err
			}

			p.appendInstruction(newInstruction(and, unusedDifference, l))

		case cod.Plus, cod.Minus, cod.Times, cod.Divide:
			if err := validateDataType(cod.Integer64, i.Code.Arg1.DataType, i.Code.Arg2.DataType); err != nil {
				return err
			}

			switch i.Code.Operation {
			case cod.Plus:
				p.appendInstruction(newInstruction(add, unusedDifference, l))

			case cod.Minus:
				p.appendInstruction(newInstruction(sub, unusedDifference, l))

			case cod.Times:
				p.appendInstruction(newInstruction(imul, unusedDifference, l))

			case cod.Divide:
				p.appendInstruction(newInstruction(idiv, unusedDifference, l))
			}

		case cod.Equal, cod.NotEqual, cod.Less, cod.LessEqual, cod.Greater, cod.GreaterEqual:
			if err := validateDataType(cod.Integer64, i.Code.Arg1.DataType, i.Code.Arg2.DataType); err != nil {
				return err
			}

			p.appendInstruction(newInstruction(cmp, unusedDifference, l))

		case cod.Jump:
			p.appendInstruction(newInstruction(jmp, unusedDifference, l, newOperand(labelOperand, i.Code.Arg1.Variable)))

		case cod.JumpEqual:
			p.appendInstruction(newInstruction(je, unusedDifference, l, newOperand(labelOperand, i.Code.Arg1.Variable)))

		case cod.JumpNotEqual:
			p.appendInstruction(newInstruction(jne, unusedDifference, l, newOperand(labelOperand, i.Code.Arg1.Variable)))

		case cod.JumpLess:
			p.appendInstruction(newInstruction(jl, unusedDifference, l, newOperand(labelOperand, i.Code.Arg1.Variable)))

		case cod.JumpLessEqual:
			p.appendInstruction(newInstruction(jle, unusedDifference, l, newOperand(labelOperand, i.Code.Arg1.Variable)))

		case cod.JumpGreater:
			p.appendInstruction(newInstruction(jg, unusedDifference, l, newOperand(labelOperand, i.Code.Arg1.Variable)))

		case cod.JumpGreaterEqual:
			p.appendInstruction(newInstruction(jge, unusedDifference, l, newOperand(labelOperand, i.Code.Arg1.Variable)))

		case cod.Parameter:
			parameters.PushBack(i)

		case cod.Call:
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
			param := parameters.Back().Value.(*cod.Instruction)
			parameters.Remove(parameters.Back())

			arg, err1 := strconv.ParseUint(i.Code.Arg1.Variable, 10, integerBitSize)
			rtc, err2 := strconv.ParseUint(i.Code.Arg2.Variable, 10, integerBitSize)

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
				p.appendInstruction(newInstruction(rcall, unusedDifference, l, newOperand(addressOperand, rtc)))
			}

		case cod.Return:
			p.appendInstruction(newInstruction(ret, unusedDifference, l))

		default:
			// return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unknownIntermediateCodeOperation, i.Code.Operation, nil)
		}

		// a label must be used by the directly following instruction
		if i.Code.Operation != cod.Branch {
			l = noLabel
		}
	}

	linker := make(map[string]uint64)

	for i := range p.text {
		if p.text[i].Label != noLabel {
			linker[p.text[i].Label] = uint64(i)
		}
	}

	for i := range p.text {
		switch p.text[i].Operation {
		case call, jmp, je, jne, jl, jle, jg, jge:
			if address, ok := linker[p.text[i].Operands[0].Label]; ok {
				p.text[i].Operands[0] = newOperand(addressOperand, address)
			}
		}
	}

	return nil
}

// Append an instruction to the end of the text section
func (p *process) appendInstruction(instruction *instruction) {
	p.text = append(p.text, instruction)
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
