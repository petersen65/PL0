// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emitter

import (
	"io"

	cor "github.com/petersen65/PL0/v2/core"
)

// Size of a pointer to a memory address in bytes (64 bits).
const PointerSize = 8

// Operand kinds for instructions.
const (
	_                = OperandKind(iota)
	RegisterOperand  // register operands are used for arithmetic and logical operations
	ImmediateOperand // constant values are literals in various bit sizes
	MemoryOperand    // memory addresses are specified indirectly through registers
	LabelOperand     // labels are used to specify jump targets and must be replaced by absolute addresses before execution
	JumpOperand      // destinations for jump instructions that are specified as absolute addresses
)

// Operand sizes in bits.
const (
	Bits8  = OperandSize(8)
	Bits16 = OperandSize(16)
	Bits32 = OperandSize(32)
	Bits64 = OperandSize(64)
)

type (
	// Type for CPU operation codes.
	OperationCode int32

	// Type for operand kinds of CPU operations.
	OperandKind int32

	// Type for sizes of an operand in bits.
	OperandSize int32

	// Enumeration of registers of the CPU.
	Register int32

	// The operand of a CPU operation holds the kind of the operand and its value.
	Operand struct {
		Kind      OperandKind     `json:"kind"`          // kind of the operand
		Register  Register        `json:"register"`      // register with various bit sizes
		Immediate ImmediateDetail `json:"value"`         // additional details about the immediate operand
		Memory    MemoryDetail    `json:"memory_detail"` // additional details about the memory operand
		Label     string          `json:"label"`         // labels for jump instructions will be replaced by an address
		Jump      uint64          `json:"jump"`          // destinations for jump instructions are specified as absolute addresses
	}

	// The assembly instruction is the representation of a single CPU operation with all its operands and labels.
	Instruction struct {
		Operation OperationCode `json:"operation"` // operation code of the instruction
		Operands  []*Operand    `json:"operands"`  // operands for the operation
		Labels    []string      `json:"labels"`    // labels to whom jump instructions will jump
	}

	// Additional details about the bit size and value of immediate operands.
	ImmediateDetail struct {
		Size  OperandSize `json:"size"`  // bit size of the value
		Value any         `json:"value"` // value of the immediate operand
	}

	// Additional details about the bit size and displacement for memory operands.
	MemoryDetail struct {
		Size         OperandSize `json:"size"`         // bit size of the value in the memory space
		Displacement int64       `json:"displacement"` // used by the memory operand for "base plus displacement" addressing
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

// Return the public interface of the private assembly code unit implementation.
func NewAssemblyCodeUnit() AssemblyCodeUnit {
	return newAssemblyCodeUnit()
}

// Create a new assembly instruction with an operation code, some labels, and operands.
func NewInstruction(op OperationCode, labels []string, operands ...*Operand) *Instruction {
	return newInstruction(op, labels, operands...)
}

// Create a new register operand for an assembly instruction.
func NewRegisterOperand(register Register) *Operand {
	return newOperand(RegisterOperand, register)
}

// Create a new immediate operand for an assembly instruction.
func NewImmediateOperand(size OperandSize, value any) *Operand {
	return newOperand(ImmediateOperand, nil, ImmediateDetail{Size: size, Value: value})
}

// Ceate a new memory operand for an assembly instruction.
func NewMemoryOperand(register Register, size OperandSize, displacement int64) *Operand {
	return newOperand(MemoryOperand, register, MemoryDetail{Size: size, Displacement: displacement})
}

// Create a new label operand for an assembly instruction.
func NewLabelOperand(label string) *Operand {
	return newOperand(LabelOperand, label)
}

// Create a new jump operand for an assembly instruction.
func NewJumpOperand(address uint64) *Operand {
	return newOperand(JumpOperand, address)
}
