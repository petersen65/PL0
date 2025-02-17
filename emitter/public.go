// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package emitter implements the assembly code generation compiler phase by iterating over the intermediate code unit.
package emitter

import (
	"io"

	cod "github.com/petersen65/PL0/v2/code"
	cor "github.com/petersen65/PL0/v2/core"
)

// CPU target for the assembly code emitter.
const (
	Amd64 = CentralProcessingUnit(iota) // AMD64 CPU target
)

// Operation codes for assembly instructions.
const (
	_ = OperationCode(iota)

	// assembly instructions for data copy operations
	Push
	Pop
	Mov

	// comparison assembly instruction for all relational operators and conditional jumps
	Cmp

	// unconditional and conditional jump assembly instructions
	Jmp
	Je
	Jne
	Jl
	Jle
	Jg
	Jge

	// arithmetic assembly instructions with one operand
	Neg
	And

	// arithmetic assembly instructions with two operands
	Add
	Sub
	Imul
	Idiv

	// subroutine assembly instructions for procedure calls
	Call
	Ret
	StdCall
)

// Registers of the CPU.
const (
	_     = Register(iota)
	Rax   // accumulator is used for intermediate results of arithmetic operations
	Rbx   // base register can be used for addressing variables
	Rcx   // counter register can be used for counting iterations of loops
	Rdx   // data register can be used for addressing variables
	Rsi   // source index register used in string and array operations as a pointer to source data
	Rdi   // destination index register is used used in string and array operations as a pointer to destination data
	R8    // 64-bit general purpose register
	R9    // 64-bit general purpose register
	R10   // 64-bit general purpose register
	R11   // 64-bit general purpose register
	R12   // 64-bit general purpose register
	R13   // 64-bit general purpose register
	R14   // 64-bit general purpose register
	R15   // 64-bit general purpose register
	Flags // flags register contains the current state of the CPU and reflects the result of arithmetic operations
	Rip   // instruction pointer is pointing to the next instruction to be executed
	Rsp   // stack pointer is pointing to the top of the control stack
	Rbp   // base pointer is pointing to the base of an activation record
)

// Operand kinds for instructions.
const (
	_                = OperandKind(iota)
	RegisterOperand  // 64-bit registers: rax, rbx, rcx, rdx, rsi, rdi, rsp, rbp, r8 to r15
	ImmediateOperand // int64 constant values like 'mov eax, 1'
	MemoryOperand    // memory addresses are specified indirectly through registers
	LabelOperand     // labels are used to specify jump targets and must be replaced by absolute addresses before execution
	JumpOperand      // destinations for jump instructions that are specified as absolute addresses
)

// Call codes for the programming language standard library.
const (
	_ = StandardCall(iota)
	Readln
	Writeln
)

type (
	// Type for CPU targets.
	CentralProcessingUnit int

	// Type for CPU operation codes.
	OperationCode int32

	// Type for operand kinds of CPU operations.
	OperandKind int32

	// Enumeration of registers of the CPU.
	Register int32

	// Type for standard library call codes.
	StandardCall int64

	// The operand of a CPU operation holds the kind of the operand and its value.
	Operand struct {
		OperandKind  OperandKind `json:"operand"`      // kind of the operand
		Register     Register    `json:"register"`     // register operand for the operation
		ArgInt       int64       `json:"arg_int"`      // int64 immediate value argument
		Memory       Register    `json:"memory"`       // memory address specified indirectly through register
		Label        string      `json:"label"`        // labels for jump instructions will be replaced by an address
		Jump         uint64      `json:"jump"`         // destinations for jump instructions are specified as absolute addresses
		Displacement int64       `json:"displacement"` // used by the memory operand for "base plus displacement" addressing
	}

	// The assembly instruction is the representation of a single CPU operation with all its operands and labels.
	Instruction struct {
		Operation OperationCode `json:"operation"` // operation code of the instruction
		Operands  []*Operand    `json:"operands"`  // operands for the operation
		Labels    []string      `json:"labels"`    // labels to whom jump instructions will jump
	}

	// The Emitter interface provides methods for emitting assembly code for CPU targets.
	Emitter interface {
		Emit()
		GetAssemblyCodeUnit() AssemblyCodeUnit
		Link() error
	}

	// AssemblyCodeUnit represents a logical unit of instructions created from one intermediate code unit.
	AssemblyCodeUnit interface {
		AppendInstruction(op OperationCode, labels []string, operands ...*Operand)
		AppendRuntimeLibrary()
		Length() int
		GetInstruction(index int) *Instruction
		Print(print io.Writer, args ...any) error
		Export(format cor.ExportFormat, print io.Writer) error
		Import(format cor.ExportFormat, scan io.Reader) error
	}
)

// Return the public interface of the private emitter implementation.
func NewEmitter(cpu CentralProcessingUnit, intermediateCode cod.IntermediateCodeUnit) Emitter {
	return newEmitter(cpu, intermediateCode)
}

// Return the public interface of the private assembly code unit implementation.
func NewAssemblyCodeUnit() AssemblyCodeUnit {
	return newAssemblyCodeUnit()
}

// Create a new assembly instruction with an operation code, some labels, and operands.
func NewInstruction(op OperationCode, labels []string, operands ...*Operand) *Instruction {
	return newInstruction(op, labels, operands...)
}

// Create a new operand for an assembly instruction.
func NewOperand(kind OperandKind, value any, displacement ...int64) *Operand {
	return newOperand(kind, value, displacement...)
}
