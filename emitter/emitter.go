// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

// Package emitter implements the IL/0 emitter that generates intermediate language code targeted for the IL/0 emulator.
package emitter

import (
	"bytes"
	"encoding/binary"
)

// Private implementation of the IL/0 emitter.
type emitter struct {
	textSection TextSection
}

// Return the public interface of the private emitter implementation.
func newEmitter() Emitter {
	return &emitter{
		textSection: make(TextSection, 0),
	}
}

// Update the target address and optionally the argument of an instruction in the text section.
func (e *emitter) Update(instruction, target Address, value any) error {
	if uint64(instruction) >= uint64(len(e.textSection)) {
		return newError(instructionOutOfRange, instruction)
	}

	e.textSection[instruction].Address = target

	if value != nil {
		e.textSection[instruction].Arg1 = value.(int64)
	}
	return nil
}

// Get the next free address in the text section.
func (e *emitter) GetNextAddress() Address {
	return Address(len(e.textSection))
}

// Save text section as a byte slice.
func (e *emitter) Export() ([]byte, error) {
	var buffer bytes.Buffer

	if err := binary.Write(&buffer, binary.LittleEndian, e.textSection); err != nil {
		return nil, err
	}

	return buffer.Bytes(), nil
}

// Load a constant value into a memory location.
func (e *emitter) Constant(memloc int32, value any) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Mov, MemoryLocation: memloc, Arg1: value.(int64)})
	return Address(len(e.textSection) - 1)
}

// Load a variable value from the stack into a memory location.
func (e *emitter) LoadVariable(offset Offset, difference, memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Mlv, DeclarationDepthDifference: difference, MemoryLocation: memloc, Address: Address(offset)})
	return Address(len(e.textSection) - 1)
}

// Store a variable value from a memory location into the stack.
func (e *emitter) StoreVariable(offset Offset, difference, memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Msv, DeclarationDepthDifference: difference, MemoryLocation: memloc, Address: Address(offset)})
	return Address(len(e.textSection) - 1)
}

// Add two values from memory location and memory location plus one and store the result in the memory location.
func (e *emitter) Add(memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Add, MemoryLocation: memloc})
	return Address(len(e.textSection) - 1)
}

// Subtract two values from memory location and memory location plus one and store the result in the memory location.
func (e *emitter) Subtract(memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Sub, MemoryLocation: memloc})
	return Address(len(e.textSection) - 1)
}

// Multiply two values from memory location and memory location plus one and store the result in the memory location.
func (e *emitter) Multiply(memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Mul, MemoryLocation: memloc})
	return Address(len(e.textSection) - 1)
}

// Divide two values from memory location and memory location plus one and store the result in the memory location.
func (e *emitter) Divide(memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Div, MemoryLocation: memloc})
	return Address(len(e.textSection) - 1)
}

// Negate the value in the memory location.
func (e *emitter) Negate(memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Neg, MemoryLocation: memloc})
	return Address(len(e.textSection) - 1)
}

// Test if the value in the memory location is odd.
func (e *emitter) Odd(memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: And, MemoryLocation: memloc})
	return Address(len(e.textSection) - 1)
}

// Test if two values from memory location and memory location plus one are equal.
func (e *emitter) Equal(memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Eq, MemoryLocation: memloc})
	return Address(len(e.textSection) - 1)
}

// Test if two values from memory location and memory location plus one are not equal.
func (e *emitter) NotEqual(memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Neq, MemoryLocation: memloc})
	return Address(len(e.textSection) - 1)
}

// Test if the value in memory location is less than the value in memory location plus one.
func (e *emitter) Less(memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Lss, MemoryLocation: memloc})
	return Address(len(e.textSection) - 1)
}

// Test if the value in memory location is less than or equal to the value in memory location plus one.
func (e *emitter) LessEqual(memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Leq, MemoryLocation: memloc})
	return Address(len(e.textSection) - 1)
}

// Test if the value in memory location is greater than the value in memory location plus one.
func (e *emitter) Greater(memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Gtr, MemoryLocation: memloc})
	return Address(len(e.textSection) - 1)
}

// Test if the value in memory location is greater than or equal to the value in memory location plus one.
func (e *emitter) GreaterEqual(memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Geq, MemoryLocation: memloc})
	return Address(len(e.textSection) - 1)
}

// Unconditionally jump to the target address.
func (e *emitter) Jump(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jmp, Address: target})
	return Address(len(e.textSection) - 1)
}

// Jump to the target address if the last comparison was equal.
func (e *emitter) JumpEqual(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Je, Address: target})
	return Address(len(e.textSection) - 1)
}

// Jump to the target address if the last comparison was not equal.
func (e *emitter) JumpNotEqual(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jne, Address: target})
	return Address(len(e.textSection) - 1)
}

// Jump to the target address if the last comparison was less.
func (e *emitter) JumpLess(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jl, Address: target})
	return Address(len(e.textSection) - 1)
}

// Jump to the target address if the last comparison was less or equal.
func (e *emitter) JumpLessEqual(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jle, Address: target})
	return Address(len(e.textSection) - 1)
}

// Jump to the target address if the last comparison was greater.
func (e *emitter) JumpGreater(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jg, Address: target})
	return Address(len(e.textSection) - 1)
}

// Jump to the target address if the last comparison was greater or equal.
func (e *emitter) JumpGreaterEqual(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jge, Address: target})
	return Address(len(e.textSection) - 1)
}

// Allocate block stack space for variables and memory locations required by expressions.
func (e *emitter) AllocateStackSpace(varOffset Offset, mlocOffset int64) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Alc, Address: Address(varOffset), Arg1: mlocOffset})
	return Address(len(e.textSection) - 1)
}

// Call a procdure at the target address from a caller.
func (e *emitter) Call(target Address, difference int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Cal, Address: target, DeclarationDepthDifference: difference})
	return Address(len(e.textSection) - 1)
}

// Return from a procedure to the caller.
func (e *emitter) Return() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Ret})
	return Address(len(e.textSection) - 1)
}

// System call for operating system calls.
func (e *emitter) System(call SystemCall, memloc int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Sys, MemoryLocation: memloc, Address: Address(call)})
	return Address(len(e.textSection) - 1)
}
