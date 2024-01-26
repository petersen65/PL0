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

func newEmitter() Emitter {
	return &emitter{
		textSection: make(TextSection, 0),
	}
}

func (e *emitter) Update(instruction, target Address) error {
	if uint64(instruction) >= uint64(len(e.textSection)) {
		return newError(instructionOutOfRange, instruction)
	}

	e.textSection[instruction].Address = target
	return nil
}

func (e *emitter) GetNextAddress() Address {
	return Address(len(e.textSection))
}

func (e *emitter) Export() ([]byte, error) {
	var buffer bytes.Buffer

	if err := binary.Write(&buffer, binary.LittleEndian, e.textSection); err != nil {
		return nil, err
	}

	return buffer.Bytes(), nil
}

func (e *emitter) Constant(memloc int32, value any) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Lit, MemoryLocation: memloc, Arg1: value.(int64)})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) LoadVariable(offset Offset, depth, memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Lod, DeclarationDepth: depth, MemoryLocation: memloc, Address: Address(offset)})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) StoreVariable(offset Offset, depth, memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Sto, DeclarationDepth: depth, MemoryLocation: memloc, Address: Address(offset)})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) Add(memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Add, MemoryLocation: memloc})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) Subtract(memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Sub, MemoryLocation: memloc})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) Multiply(memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Mul, MemoryLocation: memloc})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) Divide(memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Div, MemoryLocation: memloc})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) Negate(memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Neg, MemoryLocation: memloc})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) Odd(memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Odd, MemoryLocation: memloc})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) Equal() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Eq})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) NotEqual() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Neq})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) Less() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Lss})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) LessEqual() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Leq})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) Greater() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Gtr})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) GreaterEqual() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Geq})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) Jump(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jmp, Address: target})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) JumpEqual(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Je, Address: target})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) JumpNotEqual(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jne, Address: target})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) JumpLess(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jl, Address: target})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) JumpLessEqual(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jle, Address: target})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) JumpGreater(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jg, Address: target})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) JumpGreaterEqual(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jge, Address: target})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) AllocateStackSpace(offset Offset) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Inc, Address: Address(offset)})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) Call(target Address, depth int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Cal, Address: target, DeclarationDepth: depth})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) Return() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Ret})
	return Address(len(e.textSection) - 1)
}

func (e *emitter) System(call SystemCall) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Sys, Address: Address(call)})
	return Address(len(e.textSection) - 1)
}
