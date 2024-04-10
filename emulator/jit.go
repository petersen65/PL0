// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emulator

import (
	cod "github.com/petersen65/PL0/v2/code"
	cor "github.com/petersen65/PL0/v2/core"
)

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

	// Iterate over the module's code and compile it into the text section of the process.
	for i, l := iterator.First(), ""; i != nil; i = iterator.Next() {
		switch i.Code.Operation {
		case cod.Branch:
			if next := iterator.Peek(1); l != "" || next != nil && next.Code.Operation == cod.Branch {
				return cor.NewGeneralError(
					cor.Emulator, failureMap, cor.Error, consecutiveBranchOperationsNotSupported, nil, nil)
			}

			l = i.Label // save label for directly following assembly instruction

		case cod.Allocate:
			// group consecutive intermediate code allocate operations into one assembly alloc instruction
			for j := 0; ; j++ {
				if iterator.Peek(j).Code.Operation != cod.Allocate {
					p.appendInstruction(newInstruction(alloc, 0, l, newOperand(addressOperand, uint64(j+1))))
					iterator.Skip(j)
					break
				}
			}

		default:
			return cor.NewGeneralError(
				cor.Emulator, failureMap, cor.Error, unknownIntermediateCodeOperation, i.Code.Operation, nil)
		}

		// a label must be used by the directly following assembly instruction
		if i.Code.Operation != cod.Branch {
			l = ""
		}
	}

	return nil
}

// Append an assembler instruction to the end of the text section
func (p *process) appendInstruction(instruction *instruction) {
	p.text = append(p.text, instruction)
}
