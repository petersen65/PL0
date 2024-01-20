// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package emitter

const (
	ArgumentSize        = 8        // size of an instruction argument in bytes
	VariableOffsetStart = 0        // start offset for variables on top of stack frame descriptor
	EntryPointName      = "_start" // name of the entry point procedure of a program
)

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
	Sys
)

const (
	Read = SystemCall(iota)
	Write
)

const (
	None = Side(iota)
	Left
	Right
)

type (
	Operation   int32
	Side        int32
	SystemCall  int32
	Ignore      int32
	Address     uint64
	Offset      uint64
	TextSection []Instruction

	Instruction struct {
		Depth     int32
		Operation Operation
		Side      Side
		Address   Address
		Arg1      int64
	}

	Emitter interface {
		Emit(depth int32, operation Operation, side Side, any any) (Address, error)
		Update(instructionAddress Address, any any) error
		GetNextAddress() Address
		Export() ([]byte, error)
	}
)

var (
	NullAddress Address = 0
	IgnoreValue Ignore  = 0

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
		Sys: "sys",
	}
)

func NewEmitter() Emitter {
	return &emitter{
		textSection: make(TextSection, 0),
	}
}

func (e *emitter) Emit(depth int32, operation Operation, side Side, any any) (Address, error) {
	return e.emitInstruction(depth, operation, side, any)
}

func (e *emitter) Update(instructionAddress Address, any any) error {
	return e.updateInstruction(instructionAddress, any)
}

func (e *emitter) GetNextAddress() Address {
	return Address(len(e.textSection))
}

func (e *emitter) Export() ([]byte, error) {
	return e.exportSections()
}
