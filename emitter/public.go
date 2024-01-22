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
	Je
	Jne
	Jl
	Jle
	Jg
	Jge
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
		Side      Side
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
		JumpEqual(target Address) Address
		JumpNotEqual(target Address) Address
		JumpLess(target Address) Address
		JumpLessEqual(target Address) Address
		JumpGreater(target Address) Address
		JumpGreaterEqual(target Address) Address
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
		Je:  "je",
		Jne: "jne",
		Jl:  "jl",
		Jle: "jle",
		Jg:  "jg",
		Jge: "jge",
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
	return newEmitter()
}
