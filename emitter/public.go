// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package emitter implements the assembly code generation compiler phase by iterating over the intermediate code unit.
package emitter

import (
	"io"

	cor "github.com/petersen65/PL0/v2/core"
	gen "github.com/petersen65/PL0/v2/generator"
)

// CPU target for the assembly code emitter.
const (
	Amd64 = CentralProcessingUnit(iota) // AMD64 CPU target
)

// Operation codes for assembly instructions of the AMD64 CPU.
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

// Register enumeration for the 256 bit, 128-bit, 64-bit, 32-bit, 16-bit, and 8-bit registers of the AMD64 CPU.
const (
	_ = Register(iota)

	// 64-bit flags and pointer registers of the AMD64 CPU (used for control flow).
	Rflags // flags register contains the current state of the CPU and reflects the result of arithmetic operations
	Rip    // instruction pointer is pointing to the next instruction to be executed
	Rsp    // stack pointer is pointing to the top of the control stack
	Rbp    // base pointer is pointing to the base of an activation record

	// 64-bit general purpose registers of the AMD64 CPU (used for arithmetic and logical operations).
	Rax    // accumulator is used for intermediate results of arithmetic operations
	Rbx    // base register can be used for addressing variables
	Rcx    // counter register can be used for counting iterations of loops
	Rdx    // data register can be used for addressing variables
	Rsi    // source index register used in string and array operations as a pointer to source data
	Rdi    // destination index register is used used in string and array operations as a pointer to destination data
	R8     // 64-bit general purpose register
	R9     // 64-bit general purpose register
	R10    // 64-bit general purpose register
	R11    // 64-bit general purpose register
	R12    // 64-bit general purpose register
	R13    // 64-bit general purpose register
	R14    // 64-bit general purpose register
	R15    // 64-bit general purpose register

	// 32-bit general purpose registers of the AMD64 CPU (used for arithmetic and logical operations).
	Eax  // accumulator is used for intermediate results of arithmetic operations (bits 0-31 of Rax)
	Ebx  // base register can be used for addressing variables (bits 0-31 of Rbx)
	Ecx  // counter register can be used for counting iterations of loops (bits 0-31 of Rcx)
	Edx  // data register can be used for addressing variables (bits 0-31 of Rdx)
	Esi  // source index register used in string and array operations as a pointer to source data (bits 0-31 of Rsi)
	Edi  // destination index register is used used in string and array operations as a pointer to destination data (bits 0-31 of Rdi)
	R8d  // 32-bit general purpose register (bits 0-31 of R8)
	R9d  // 32-bit general purpose register (bits 0-31 of R9)
	R10d // 32-bit general purpose register (bits 0-31 of R10)
	R11d // 32-bit general purpose register (bits 0-31 of R11)
	R12d // 32-bit general purpose register (bits 0-31 of R12)
	R13d // 32-bit general purpose register (bits 0-31 of R13)
	R14d // 32-bit general purpose register (bits 0-31 of R14)
	R15d // 32-bit general purpose register (bits 0-31 of R15)

	// 16-bit general purpose registers of the AMD64 CPU (used for arithmetic and logical operations).
	Ax   // accumulator is used for intermediate results of arithmetic operations (bits 0-15 of Rax)
	Bx   // base register can be used for addressing variables (bits 0-15 of Rbx)
	Cx   // counter register can be used for counting iterations of loops (bits 0-15 of Rcx)
	Dx   // data register can be used for addressing variables (bits 0-15 of Rdx)
	Si   // source index register used in string and array operations as a pointer to source data (bits 0-15 of Rsi)
	Di   // destination index register is used used in string and array operations as a pointer to destination data (bits 0-15 of Rdi)
	R8w  // 16-bit general purpose register (bits 0-15 of R8)
	R9w  // 16-bit general purpose register (bits 0-15 of R9)
	R10w // 16-bit general purpose register (bits 0-15 of R10)
	R11w // 16-bit general purpose register (bits 0-15 of R11)
	R12w // 16-bit general purpose register (bits 0-15 of R12)
	R13w // 16-bit general purpose register (bits 0-15 of R13)
	R14w // 16-bit general purpose register (bits 0-15 of R14)
	R15w // 16-bit general purpose register (bits 0-15 of R15)

	// 8-bit general purpose registers of the AMD64 CPU (used for arithmetic and logical operations).
	Al   // accumulator is used for intermediate results of arithmetic operations (bits 0-7 of Rax)
	Bl   // base register can be used for addressing variables (bits 0-7 of Rbx)
	Cl   // counter register can be used for counting iterations of loops (bits 0-7 of Rcx)
	Dl   // data register can be used for addressing variables (bits 0-7 of Rdx)
	Ah   // accumulator is used for intermediate results of arithmetic operations (bits 8-15 of Rax)
	Bh   // base register can be used for addressing variables (bits 8-15 of Rbx)
	Ch   // counter register can be used for counting iterations of loops (bits 8-15 of Rcx)
	Dh   // data register can be used for addressing variables (bits 8-15 of Rdx)
	R8b  // 8-bit general purpose register (bits 0-7 of R8)
	R9b  // 8-bit general purpose register (bits 0-7 of R9)
	R10b // 8-bit general purpose register (bits 0-7 of R10)
	R11b // 8-bit general purpose register (bits 0-7 of R11)
	R12b // 8-bit general purpose register (bits 0-7 of R12)
	R13b // 8-bit general purpose register (bits 0-7 of R13)
	R14b // 8-bit general purpose register (bits 0-7 of R14)
	R15b // 8-bit general purpose register (bits 0-7 of R15)

	// 256 bit AVX registers of the AMD64 CPU (advanced vector extensions, AVX).
	Ymm0  // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm1  // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm2  // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm3  // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm4  // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm5  // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm6  // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm7  // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm8  // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm9  // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm10 // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm11 // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm12 // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm13 // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm14 // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm15 // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)

	// 128-bit SSE registers of the AMD64 CPU (streaming single instructions multiple data extensions, SSE).
	Xmm0  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm0)
	Xmm1  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm1)
	Xmm2  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm2)
	Xmm3  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm3)
	Xmm4  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm4)
	Xmm5  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm5)
	Xmm6  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm6)
	Xmm7  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm7)
	Xmm8  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm8)
	Xmm9  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm9)
	Xmm10 // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm10)
	Xmm11 // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm11)
	Xmm12 // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm12)
	Xmm13 // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm13)
	Xmm14 // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm14)
	Xmm15 // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm15)
)

// Operand kinds for instructions.
const (
	_                = OperandKind(iota)
	RegisterOperand  // register operands are used for arithmetic and logical operations
	ImmediateOperand // constant values like 'mov rax, 1'
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

	// Enumeration of 64-bit registers of the CPU.
	Register int32

	// Type for standard library call codes.
	StandardCall int64

	// The operand of a CPU operation holds the kind of the operand and its value.
	Operand struct {
		OperandKind  OperandKind `json:"operand"`      // kind of the operand
		Register     Register    `json:"register"`     // register operand for the operation
		Value        any         `json:"value"`        // immediate value argument
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
	}

	// AssemblyCodeUnit represents a logical unit of instructions created from one intermediate code unit.
	AssemblyCodeUnit interface {
		AppendInstruction(op OperationCode, labels []string, operands ...*Operand)
		AppendRuntimeLibrary()
		Link() error
		Length() int
		GetInstruction(index int) *Instruction
		Print(print io.Writer, args ...any) error
		Export(format cor.ExportFormat, print io.Writer) error
		Import(format cor.ExportFormat, scan io.Reader) error
	}
)

// Return the public interface of the private emitter implementation.
func NewEmitter(cpu CentralProcessingUnit, intermediateCode gen.IntermediateCodeUnit) Emitter {
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
