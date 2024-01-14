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

func (e *emitter) emitInstruction(declarationDepth int32, operation Operation, argument any) (Address, error) {
	e.textSection = append(e.textSection, Instruction{Depth: declarationDepth, Operation: operation})
	address := Address(len(e.textSection) - 1)
	return address, e.updateInstructionArgument(address, argument)
}

func (e *emitter) updateInstructionArgument(address Address, argument any) error {
	if uint64(address) >= uint64(len(e.textSection)) {
		return e.error(instructionOutOfRange, address)
	}

	instruction := e.textSection[address]

	switch arg := argument.(type) {
	case Address:
		instruction.Address = arg

	case Offset:
		instruction.Address = Address(arg)

	case SystemCall:
		instruction.Address = Address(arg)

	case int64:
		var buffer bytes.Buffer

		if err := binary.Write(&buffer, binary.LittleEndian, arg); err != nil {
			return err
		}

		copy(instruction.Argument[:], buffer.Bytes())

	case float64:
		var buffer bytes.Buffer

		if err := binary.Write(&buffer, binary.LittleEndian, arg); err != nil {
			return err
		}

		copy(instruction.Argument[:], buffer.Bytes())

	case Ignore:
		// do nothing and ignore argument

	default:
		return e.error(invalidArgumentType, argument)
	}

	e.textSection[address] = instruction
	return nil
}

func (e *emitter) exportSections() ([]byte, error) {
	var buffer bytes.Buffer

	if err := binary.Write(&buffer, binary.LittleEndian, e.textSection); err != nil {
		return nil, err
	}

	return buffer.Bytes(), nil
}
