// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package emitter

import (
	"bytes"
	"encoding/binary"

	tok "github.com/petersen65/PL0/token"
)

// Private implementation of the IL/0 emitter.
type emitter struct {
	textSection TextSection
}

// Return the public interface of the private IL/0 emitter implementation.
func newEmitter() Emitter {
	return &emitter{
		textSection: make(TextSection, 0),
	}
}

// Update the target address and optionally the argument of an instruction in the text section.
func (e *emitter) Update(instruction, target Address, value any) error {
	if uint64(instruction) >= uint64(len(e.textSection)) {
		return tok.NewGeneralError(tok.Emitter, failureMap, tok.Error, instructionOutOfRange, instruction)
	}

	e.textSection[instruction].Address = target

	if value != nil {
		e.textSection[instruction].ArgInt = value.(int64)
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

// Load a constant value onto the stack.
func (e *emitter) Constant(value any) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Ldc, ArgInt: value.(int64)})
	return Address(len(e.textSection) - 1)
}

// Load a variable value onto the stack.
func (e *emitter) LoadVariable(offset Offset, difference int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Ldv, DeclarationDepthDifference: difference, Address: Address(offset)})
	return Address(len(e.textSection) - 1)
}

// Store a variable value into the stack.
func (e *emitter) StoreVariable(offset Offset, difference int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Stv, DeclarationDepthDifference: difference, Address: Address(offset)})
	return Address(len(e.textSection) - 1)
}

// Add two values from top of stack and store the result onto the stack.
func (e *emitter) Add() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Add})
	return Address(len(e.textSection) - 1)
}

// Subtract two values from top of stack and store the result onto the stack.
func (e *emitter) Subtract() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Sub})
	return Address(len(e.textSection) - 1)
}

// Multiply two values from top of stack and store the result onto the stack.
func (e *emitter) Multiply() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Mul})
	return Address(len(e.textSection) - 1)
}

// Divide two values from top of stack and store the result onto the stack.
func (e *emitter) Divide() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Div})
	return Address(len(e.textSection) - 1)
}

// Negate the value onto the stack.
func (e *emitter) Negate() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Neg})
	return Address(len(e.textSection) - 1)
}

// Test if the value onto the stack is odd.
func (e *emitter) Odd() Address {
	e.textSection = append(e.textSection, Instruction{Operation: And})
	return Address(len(e.textSection) - 1)
}

// Test if two values from top of stack are equal.
func (e *emitter) Equal() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Eq})
	return Address(len(e.textSection) - 1)
}

// Test if two values from top of stack are not equal.
func (e *emitter) NotEqual() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Neq})
	return Address(len(e.textSection) - 1)
}

// Test if the value from top of stack is less than the value from top of stack minus one.
func (e *emitter) Less() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Lss})
	return Address(len(e.textSection) - 1)
}

// Test if the value from top of stack is less than or equal to the value from top of stack minus one.
func (e *emitter) LessEqual() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Leq})
	return Address(len(e.textSection) - 1)
}

// Test if the value from top of stack is greater than the value from top of stack minus one.
func (e *emitter) Greater() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Gtr})
	return Address(len(e.textSection) - 1)
}

// Test if the value from top of stack is greater than or equal to the value from top of stack minus one.
func (e *emitter) GreaterEqual() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Geq})
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

// Allocate stack space for variables.
func (e *emitter) AllocateStackSpace(varOffset Offset) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Alc, Address: Address(varOffset)})
	return Address(len(e.textSection) - 1)
}

// Call a procedure at the target address from a caller.
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
func (e *emitter) System(call SystemCall) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Sys, Address: Address(call)})
	return Address(len(e.textSection) - 1)
}
