// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

// Package emitter implements the IL/0 emitter that generates intermediate language code targeted for the IL/0 emulator.
package emitter

const (
	EntryPointName      = "_start" // name of the entry point procedure of a program
	VariableOffsetStart = 0        // start offset for variables on top of stack frame descriptor
	FreeCpuRegisters    = 4        // number of 64-bit unsigned integer free cpu registers
)

// Operation codes for IL/0.
const (
	_ = Operation(iota)
	Mov
	Mlv
	Msv
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
	_
	_
	Movr
	Movs
)

// Destination stores either a cpu register or a memory location offset from the stack pointer.
const (
	Ax    = Destination(iota) // accumulator is used for intermediate results of arithmetic operations
	Bx                        // base register is used for addressing variables in the current stack frame (descriptor)
	Cx                        // counter register is used for counting iterations of loops
	Dx                        // data register is used for addressing variables in the previous stack frame (descriptor)
	Ip                        // instruction pointer is pointing to the next instruction to be executed
	Sp                        // stack pointer is pointing to the top of the stack
	Bp                        // base pointer is pointing to the base of the current stack frame (descriptor)
	Flags                     // flags register contains the current state of the cpu and reflects the result of arithmetic operations
)

// System call codes for operating system calls for IL/0.
const (
	Read = SystemCall(iota)
	Write
)

// Core API for the IL/0 emitter.
type (
	Operation   int32
	Destination uint64
	SystemCall  int32
	Address     uint64
	Offset      uint64
	TextSection []Instruction

	Instruction struct {
		Operation                  Operation   // operation code of the instruction
		DeclarationDepthDifference int32       // declaration depth difference between procedure block and to be accessed variables
		Destination                Destination // destination holds a register or a stack pointer offset
		MemoryLocation             int32       // memory location used by operators to store temporary results while calculating expressions
		Address                    Address     // target address or offset of a variable of the operation
		Arg1                       int64       // int64 argument of the operation
	}

	Emitter interface {
		Update(instruction, target Address, value any) error
		GetNextAddress() Address
		Export() ([]byte, error)
		Constant(destination Destination, value any) Address
		LoadVariable(offset Offset, difference int32, memloc int32) Address
		StoreVariable(offset Offset, difference int32, memloc int32) Address
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
		Call(target Address, difference int32) Address
		Return() Address
		System(call SystemCall, memloc int32) Address
	}
)

var (
	// NullAddress is the null address value for IL/0 instructions.
	NullAddress Address = 0

	// OperationNames maps IL/0 operation codes to their string representation.
	OperationNames = map[Operation]string{
		Mov:  "mov",
		Mlv:  "mlv",
		Msv:  "msv",
		Cal:  "cal",
		Ret:  "ret",
		Alc:  "alc",
		Jmp:  "jmp",
		Je:   "je",
		Jne:  "jne",
		Jl:   "jl",
		Jle:  "jle",
		Jg:   "jg",
		Jge:  "jge",
		Neg:  "neg",
		Add:  "add",
		Sub:  "sub",
		Mul:  "mul",
		Div:  "div",
		And:  "and",
		Eq:   "eq",
		Neq:  "neq",
		Lss:  "lss",
		Leq:  "leq",
		Gtr:  "gtr",
		Geq:  "geq",
		Sys:  "sys",
		Movr: "movr",
		Movs: "movs",
	}
)

// Return the public interface of the private IL/0 emitter implementation.
func NewEmitter() Emitter {
	return newEmitter()
}
