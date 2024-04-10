// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emulator

import (
	cod "github.com/petersen65/PL0/v2/code"
	cor "github.com/petersen65/PL0/v2/core"
)

// JIT compile a module into the text section of a process and return an error if the module fails to compile.
func (p *process) jitCompile(module cod.Module) error {
	p.text = make(textSection, 0)

	// Iterate over the module's code and compile it into the text section of the process.
	for instruction := range module.IterateInstruction() {
		// Instruction selection for assembly code.
		switch instruction.Code.Operation {
		case cod.Branch:

		case cod.Allocate:

		default:
			return cor.NewGeneralError(
				cor.Emulator, failureMap, cor.Error, unknownIntermediateCodeOperation, instruction.Code.Operation, nil)
		}
	}

	return nil
}

func (p *process) appendInstruction(instruction *instruction) {
	p.text = append(p.text, instruction)
}
