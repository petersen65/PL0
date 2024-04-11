// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emulator

import (
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

	// protect the JIT compiler against non-valid structured intermediate code
	defer func() {
		if r := recover(); r != nil {
			err = cor.NewGeneralError(
				cor.Emulator, failureMap, cor.Error, recoveredFromIllegalIntermediateCode, r, nil)
		}
	}()

	// iterate over the module's code and compile it into the text section of the process
	for i, l := iterator.First(), noLabel; i != nil; i = iterator.Next() {
		switch i.Code.Operation {
		case cod.Branch:
			if next := iterator.Peek(1); l != noLabel || next != nil && next.Code.Operation == cod.Branch {
				return cor.NewGeneralError(
					cor.Emulator, failureMap, cor.Error, consecutiveBranchOperationsNotSupported, nil, nil)
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
					return cor.NewGeneralError(
						cor.Emulator, failureMap, cor.Error, immediateOperandParsingError, i.Code.Arg1.Variable, err)
				}

				p.appendInstruction(newInstruction(push, unusedDifference, l, newOperand(immediateOperand, arg)))

			default:
				return cor.NewGeneralError(
					cor.Emulator, failureMap, cor.Error, unsupportedOperandDataType, i.Code.Arg1.DataType, nil)
			}

		default:
			return cor.NewGeneralError(
				cor.Emulator, failureMap, cor.Error, unknownIntermediateCodeOperation, i.Code.Operation, nil)
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
