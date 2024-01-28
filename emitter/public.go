// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package emitter

const (
	VariableOffsetStart = 0        // start offset for variables on top of stack frame descriptor
	EntryPointName      = "_start" // name of the entry point procedure of a program
)

const (
	_ = Operation(iota)
	Ldc
	Ldv
	Stv
	Cal
	Ret
	Asp
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

type (
	Operation   int32
	SystemCall  int32
	Address     uint64
	Offset      uint64
	TextSection []Instruction

	Instruction struct {
		Operation        Operation
		DeclarationDepth int32
		MemoryLocation   int32
		Address          Address
		Arg1             int64
	}

	Emitter interface {
		Update(instruction, target Address, value any) error
		GetNextAddress() Address
		Export() ([]byte, error)
		Constant(memloc int32, value any) Address
		LoadVariable(offset Offset, depth int32, memloc int32) Address
		StoreVariable(offset Offset, depth int32, memloc int32) Address
		Add(memloc int32) Address
		Subtract(memloc int32) Address
		Multiply(memloc int32) Address
		Divide(memloc int32) Address
		Negate(memloc int32) Address
		Odd(memloc int32) Address
		Equal(memloc int32) Address
		NotEqual(memloc int32) Address
		Less(memloc int32) Address
		LessEqual(memloc int32) Address
		Greater(memloc int32) Address
		GreaterEqual(memloc int32) Address
		Jump(target Address) Address
		JumpEqual(target Address) Address
		JumpNotEqual(target Address) Address
		JumpLess(target Address) Address
		JumpLessEqual(target Address) Address
		JumpGreater(target Address) Address
		JumpGreaterEqual(target Address) Address
		AllocateStackSpace(varOffset Offset, mlocOffset int64) Address
		Call(target Address, depth int32) Address
		Return() Address
		System(call SystemCall, memloc int32) Address
	}
)

var (
	NullAddress Address = 0

	OperationNames = map[Operation]string{
		Ldc: "ldc",
		Ldv: "ldv",
		Stv: "stv",
		Cal: "call",
		Ret: "ret",
		Asp: "asp",
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
