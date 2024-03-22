// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package emitter implements the IL/0 emitter that generates intermediate language code targeted for the IL/0 emulator.
package emitter

// Start offset for variables on top of stack frame descriptor.
const VariableOffsetStart = 0

// Operation codes for IL/0.
const (
	_ = Operation(iota)
	Ldc
	Ldv
	Stv
	Cal
	Ret
	Alc
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
	And
	Eq
	Neq
	Lss
	Leq
	Gtr
	Geq
	Sys
)

// System call codes for operating system calls for IL/0.
const (
	Read = SystemCall(iota)
	Write
)

type (
	// Type for operation codes.
	Operation int32

	// Type for system call codes.
	SystemCall int32

	// Type for 64 bit addresses.
	Address uint64

	// Type for 64 bit offsets.
	Offset uint64

	// Text section of the emitted IL/0 program.
	TextSection []Instruction

	// Instruction is the internal representation of an IL/0 instruction.
	Instruction struct {
		Operation                   Operation // operation code of the instruction
		BlockNestingDepthDifference int32     // block nesting depth difference between procedure block and to be accessed variables
		Address                     Address   // target address or offset of a variable of the operation
		ArgInt                      int64     // int64 argument of the operation
	}

	// The emitter interface provides an abstract API for emitting IL/0 instructions.
	Emitter interface {
		Update(instruction, target Address, value any) error
		GetNextAddress() Address
		Export() ([]byte, error)
		Constant(value any) Address
		LoadVariable(offset Offset, difference int32) Address
		StoreVariable(offset Offset, difference int32) Address
		Add() Address
		Subtract() Address
		Multiply() Address
		Divide() Address
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
		AllocateStackSpace(varOffset Offset) Address
		Call(target Address, difference int32) Address
		Return() Address
		System(call SystemCall) Address
	}
)

var (
	// NullAddress is the null address value for IL/0 instructions.
	NullAddress Address = 0

	// OperationNames maps IL/0 operation codes to their string representation.
	OperationNames = map[Operation]string{
		Ldc: "ldc",
		Ldv: "ldv",
		Stv: "stv",
		Cal: "cal",
		Ret: "ret",
		Alc: "alc",
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
		And: "and",
		Eq:  "eq",
		Neq: "neq",
		Lss: "lss",
		Leq: "leq",
		Gtr: "gtr",
		Geq: "geq",
		Sys: "sys",
	}
)

// Return the public interface of the private IL/0 emitter implementation.
func NewEmitter() Emitter {
	return newEmitter()
}
