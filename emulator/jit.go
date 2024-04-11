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
			if next := iterator.Peek(1); l != noLabel || next != nil && next.Code.Operation == cod.Branch {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, consecutiveBranchOperationsNotSupported, nil, nil)
			}

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
				arg, err := strconv.ParseInt(i.Code.Arg1.Variable, 10, integerBitSize)

				if err != nil {
					return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, operandParsingError, i.Code.Arg1.Variable, err)
				}

				p.appendInstruction(newInstruction(push, unusedDifference, l, newOperand(immediateOperand, arg)))

			default:
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperandDataType, i.Code.Arg1.DataType, nil)
			}

		case cod.VariableLoad:
			switch i.Code.Arg1.DataType {
			case cod.Integer64:
				p.appendInstruction(newInstruction(loadvar, i.DepthDifference, l, newOperand(addressOperand, i.Code.Arg1.Offset)))

			default:
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperandDataType, i.Code.Arg1.DataType, nil)
			}

		case cod.VariableStore:
			switch i.Code.Result.DataType {
			case cod.Integer64:
				p.appendInstruction(newInstruction(storevar, i.DepthDifference, l, newOperand(addressOperand, i.Code.Result.Offset)))

			default:
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperandDataType, i.Code.Result.DataType, nil)
			}

		case cod.Negate:
			p.appendInstruction(newInstruction(neg, unusedDifference, l))

		case cod.Odd:
			p.appendInstruction(newInstruction(and, unusedDifference, l))

		case cod.Plus:
			p.appendInstruction(newInstruction(add, unusedDifference, l))

		case cod.Minus:
			p.appendInstruction(newInstruction(sub, unusedDifference, l))

		case cod.Times:
			p.appendInstruction(newInstruction(imul, unusedDifference, l))

		case cod.Divide:
			p.appendInstruction(newInstruction(idiv, unusedDifference, l))

		case cod.Parameter:
			parameters.PushBack(i)

		case cod.Runtime:
			param := parameters.Back().Value.(*cod.Instruction)
			parameters.Remove(parameters.Back())

			switch param.Code.Arg1.DataType {
			case cod.Integer64:
				if i.Code.Arg1.DataType != cod.UnsignedInteger64 {
					return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperandDataType, i.Code.Arg1.DataType, nil)
				}

				arg, err := strconv.ParseUint(i.Code.Arg1.Variable, 10, integerBitSize)

				if err != nil {
					return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, operandParsingError, i.Code.Arg1.Variable, err)
				}

				if arg != 1 {
					return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unexpectedNumberOfFunctionArguments, arg, nil)
				}

				if i.Code.Arg2.DataType != cod.UnsignedInteger64 {
					return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperandDataType, i.Code.Arg2.DataType, nil)
				}

				rtc, err := strconv.ParseUint(i.Code.Arg2.Variable, 10, integerBitSize)

				if err != nil {
					return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, operandParsingError, i.Code.Arg2.Variable, err)
				}

				p.appendInstruction(newInstruction(rcall, unusedDifference, l, newOperand(addressOperand, rtc)))

			default:
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperandDataType, param.Code.Arg1.DataType, nil)
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

	return nil
}

// Append an instruction to the end of the text section
func (p *process) appendInstruction(instruction *instruction) {
	p.text = append(p.text, instruction)
}
