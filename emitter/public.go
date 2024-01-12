// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package emitter

const (
	Lit = Operation(iota)
	Lod
	Sto
	Cal
	Ret
	Inc
	Jmp
	Jpc
	Neg
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
	Operation   int32
	Address     uint64
	TextSection []Instruction

	Instruction struct {
		Depth     int32
		Operation Operation
		Argument  Address
	}

	Emitter interface {
		Emit(declarationDepth int32, operation Operation, argument Address) (Address, error)
		UpdateArgument(instructionAddress, argument Address) error
		GetNextAddress() Address
		Export() ([]byte, error)
	}
)

var (
	NullAddress Address = 0

	OperationNames = map[Operation]string{
		Lit: "lit",
		Lod: "lod",
		Sto: "sto",
		Cal: "cal",
		Ret: "ret",
		Inc: "inc",
		Jmp: "jmp",
		Jpc: "jpc",
		Neg: "neg",
		Add: "add",
		Sub: "sub",
		Mul: "mul",
		Div: "div",
		Odd: "odd",
		Eq:  "eq",
		Neq: "neq",
		Lss: "lss",
		Leq: "leq",
		Gtr: "gtr",
		Geq: "geq",
	}
)

func NewEmitter() Emitter {
	return &emitter{
		textSection: make(TextSection, 0),
	}
}

func (e *emitter) Emit(declarationDepth int32, operation Operation, argument Address) (Address, error) {
	return e.emitInstruction(declarationDepth, operation, argument)
}

func (e *emitter) UpdateArgument(instructionAddress, argument Address) error {
	return e.updateInstructionArgument(instructionAddress, argument)
}

func (e *emitter) GetNextAddress() Address {
	return Address(len(e.textSection))
}

func (e *emitter) Export() ([]byte, error) {
	return e.exportSections()
}
