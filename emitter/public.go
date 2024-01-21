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
	Jne
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
	Address     uint64
	Offset      uint64
	TextSection []Instruction

	Instruction struct {
		Depth     int32
		Operation Operation
		Side	  Side
		Address   Address
		Arg1      int64
	}

	Emitter interface {
		Update(instruction, target Address) error
		GetNextAddress() Address
		Export() ([]byte, error)
		Constant(side Side, value any) Address
		Variable(side Side, offset Offset, depth int32, store bool) Address
		Multiply() Address
		Divide() Address
		Add() Address
		Subtract() Address
		Negate() Address
		Odd() Address
		Equal() Address
		NotEqual() Address
		Less() Address
		LessEqual() Address
		Greater() Address
		GreaterEqual() Address
		Jump(target Address) Address
		JumpConditional(target Address) Address
		AllocateStackSpace(offset Offset) Address
		Call(target Address, depth int32) Address
		Return() Address
		System(call SystemCall) Address
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
		Jne: "jne",
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

func (e *emitter) Update(instruction, target Address) error {
	return e.updateAddress(instruction, target)
}

func (e *emitter) GetNextAddress() Address {
	return Address(len(e.textSection))
}

func (e *emitter) Export() ([]byte, error) {
	return e.exportSections()
}

func (e *emitter) Constant(side Side, value any) Address {
	return e.constant(side, value)
}

func (e *emitter) Variable(side Side, offset Offset, depth int32, store bool) Address {
	return e.variable(side, offset, depth, store)
}

func (e *emitter) Multiply() Address {
	return e.multiply()
}

func (e *emitter) Divide() Address {
	return e.divide()
}

func (e *emitter) Add() Address {
	return e.add()
}

func (e *emitter) Subtract() Address {
	return e.subtract()
}

func (e *emitter) Negate() Address {
	return e.negate()
}

func (e *emitter) Odd() Address {
	return e.odd()
}

func (e *emitter) Equal() Address {
	return e.equal()
}

func (e *emitter) NotEqual() Address {
	return e.notEqual()
}

func (e *emitter) Less() Address {
	return e.less()
}

func (e *emitter) LessEqual() Address {
	return e.lessEqual()
}

func (e *emitter) Greater() Address {
	return e.greater()
}

func (e *emitter) GreaterEqual() Address {
	return e.greaterEqual()
}

func (e *emitter) Jump(target Address) Address {
	return e.jump(target)
}

func (e *emitter) JumpConditional(target Address) Address {
	return e.jumpConditional(target)
}

func (e *emitter) AllocateStackSpace(offset Offset) Address {
	return e.allocateStackSpace(offset)
}

func (e *emitter) Call(target Address, depth int32) Address {
	return e.call(target, depth)
}

func (e *emitter) Return() Address {
	return e.returnFromCall()
}

func (e *emitter) System(call SystemCall) Address {
	return e.systemCall(call)
}
