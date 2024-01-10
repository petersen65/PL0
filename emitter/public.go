// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package emitter

const (
	Lit = Operation(iota)
	Opr
	Lod
	Sto
	Cal
	Ret
	Inc
	Jmp
	Jpc
)

const (
	Neg = Address(iota)
	Add
	Sub
	Mul
	Div
	Odd
	Eq
	Neq
	Lss
	Leq
	Gtr
	Geq
)

type (
	Operation int
	Address   uint64

	Emitter interface {
		EmitInstruction(declarationDepth int, operation Operation, argument Address) (Address, error)
		UpdateInstructionArgument(instructionAddress, argument Address) error
		GetNextInstructionAddress() Address
	}
)

var NullAddress Address = 0

func NewEmitter() Emitter {
	return &emitter{
		codeSegment: make([]instruction, 0),
	}
}

func (e *emitter) EmitInstruction(declarationDepth int, operation Operation, argument Address) (Address, error) {
	return e.emitInstruction(declarationDepth, operation, argument)
}

func (e *emitter) UpdateInstructionArgument(instructionAddress, argument Address) error {
	return e.updateInstructionArgument(instructionAddress, argument)
}

func (e *emitter) GetNextInstructionAddress() Address {
	return Address(len(e.codeSegment))
}
