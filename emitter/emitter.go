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

func (e *emitter) updateAddress(instruction, target Address) error {
	if uint64(instruction) >= uint64(len(e.textSection)) {
		return newError(instructionOutOfRange, instruction)
	}

	e.textSection[instruction].Address = target
	return nil
}

func (e *emitter) exportSections() ([]byte, error) {
	var buffer bytes.Buffer

	if err := binary.Write(&buffer, binary.LittleEndian, e.textSection); err != nil {
		return nil, err
	}

	return buffer.Bytes(), nil
}

func (e *emitter) constant(side Side, value any) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Lit, Side: side, Arg1: value.(int64)})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) variable(side Side, offset Offset, depth int32, store bool) Address {
	if store {
		e.textSection = append(e.textSection, Instruction{Operation: Sto, Side: side, Address: Address(offset), Depth: depth})
	} else {
		e.textSection = append(e.textSection, Instruction{Operation: Lod, Side: side, Address: Address(offset), Depth: depth})
	}

	return Address(len(e.textSection) - 1)
}

func (e *emitter) multiply() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Mul})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) divide() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Div})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) add() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Add})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) subtract() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Sub})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) negate() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Neg})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) odd() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Odd})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) equal() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Eq})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) notEqual() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Neq})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) less() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Lss})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) lessEqual() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Leq})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) greater() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Gtr})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) greaterEqual() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Geq})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) jump(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jmp, Address: target})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) jumpEqual(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Je, Address: target})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) jumpNotEqual(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jne, Address: target})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) jumpLess(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jl, Address: target})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) jumpLessEqual(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jle, Address: target})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) jumpGreater(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jg, Address: target})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) jumpGreaterEqual(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jge, Address: target})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) allocateStackSpace(offset Offset) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Inc, Address: Address(offset)})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) call(target Address, depth int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Cal, Address: target, Depth: depth})
	return Address(len(e.textSection) - 1)
}	

func (e *emitter) returnFromCall() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Ret})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) systemCall(call SystemCall) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Sys, Address: Address(call)})
	return Address(len(e.textSection) - 1)
}
