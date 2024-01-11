// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package emitter

import (
	"bytes"
	"encoding/binary"
)

type emitter struct {
	textSection TextSection
}

func (e *emitter) emitInstruction(declarationDepth int32, operation Operation, argument Address) (Address, error) {
	if len(e.textSection) >= textSectionMax {
		return 0, e.error(reachedTextSectionMax, len(e.textSection))
	}

	e.textSection = append(e.textSection, Instruction{
		Depth:     declarationDepth,
		Operation: operation,
		Argument:  argument,
	})

	return Address(len(e.textSection) - 1), nil
}

func (e *emitter) updateInstructionArgument(instruction, argument Address) error {
	if uint64(instruction) >= uint64(len(e.textSection)) {
		return e.error(instructionOutOfRange, instruction)
	}

	e.textSection[instruction].Argument = argument
	return nil
}

func (e *emitter) exportSections() ([]byte, error) {
	var buffer bytes.Buffer

	if err := binary.Write(&buffer, binary.LittleEndian, e.textSection); err != nil {
		return nil, err
	}

	return buffer.Bytes(), nil
}
