// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package assembly implements the assembly code language which is based on the AMD64 assembly language.
package assembly

import (
	"bytes"
	"fmt"
	"io"
	"strings"

	cor "github.com/petersen65/PL0/v2/core"
)

const (
	PointerSize           = 8                       // size of a pointer to a memory address in bytes (64 bits)
	QuadWordSize          = 8                       // size of a quad word in bytes (64 bits)
	DoubleWordSize        = 4                       // size of a double word in bytes (32 bits)
	EntryPointLabel       = "main"                  // label for the entry point of the program
	CreateStaticLinkLabel = "rt.create_static_link" // label for runtime function "create_static_link"
	FollowStaticLinkLabel = "rt.follow_static_link" // label for runtime function "follow_static_link"
)

// Operand kinds for instructions.
const (
	RegisterOperand  OperandKind = iota // register operands are used for arithmetic and logical operations
	ImmediateOperand                    // constant values are literals in various bit sizes
	MemoryOperand                       // memory addresses are specified indirectly through registers
	LabelOperand                        // labels are used to specify jump targets and must be replaced by absolute addresses before execution
)

// Operand sizes in bits.
const (
	Bits8  OperandSize = 8
	Bits16 OperandSize = 16
	Bits32 OperandSize = 32
	Bits64 OperandSize = 64
)

// Comparison types used to interpret the CPU flags in conditional jumps.
const (
	ComparisonNone            ComparisonType = iota // no comparison instruction, used for unconditional jumps
	ComparisonIntegerSigned                         // signed integer comparison (Cmp instruction)
	ComparisonIntegerUnsigned                       // unsigned integer comparison (Cmp instruction)
	ComparisonFloat                                 // floating-point comparison (Ucomisd/Ucomiss instructions)
)

// Kind of read-only data to be stored in a read-only section.
const (
	ReadOnlyUtf32 ReadOnlyDataKind = iota // UTF-32 encoded strings
	ReadOnlyInt64                         // 64-bit integer literals (signed and unsigned)
	ReadOnlyBytes                         // static raw byte arrays
)

// Kind of output that is produced by the assembly code.
const (
	Application OutputKind = iota
	Runtime
)

type (
	// Type for CPU operation codes.
	OperationCode int

	// Type for operand kinds of CPU operations.
	OperandKind int

	// Type for sizes of an operand in bits.
	OperandSize int

	// Comparison types for conditional jumps.
	ComparisonType int

	// Enumeration of registers of the CPU.
	Register int

	// Kind of read-only static data.
	ReadOnlyDataKind int

	// Output kind of the assembly code.
	OutputKind int

	// The operand of a CPU operation holds the kind of the operand and its value.
	Operand struct {
		Kind      OperandKind  `json:"kind"`          // kind of the operand
		Register  Register     `json:"register"`      // register with various bit sizes
		Immediate any          `json:"immediate"`     // value of the immediate operand
		Memory    MemoryDetail `json:"memory_detail"` // additional details about the memory operand
		Label     string       `json:"label"`         // labels for jump instructions will be replaced by an address
	}

	// The assembly instruction is the representation of a single CPU operation with all its operands and labels.
	Instruction struct {
		Prefix    OperationCode `json:"prefix"`    // prefix for the instruction
		Operation OperationCode `json:"operation"` // operation code of the instruction
		Operands  []*Operand    `json:"operands"`  // operands for the operation
		Labels    []string      `json:"labels"`    // labels to whom jump instructions will jump
	}

	// Additional details about the bit size and displacement for memory operands.
	MemoryDetail struct {
		Size         OperandSize `json:"size"`         // bit size of the value in the memory space
		Displacement int32       `json:"displacement"` // used by the memory operand for "base plus displacement" addressing
	}

	// A read-only data item is a constant binary value that is not modified during program execution.
	ReadOnlyDataItem struct {
		Kind   ReadOnlyDataKind `json:"kind"`   // kind of the read-only data item
		Labels []string         `json:"labels"` // labels to access the read-only data item
		Value  any              `json:"value"`  // the value will be stored in a read-only section and encoded based on its kind
	}

	// AssemblyCodeUnit represents a logical unit of instructions created from one intermediate code unit.
	AssemblyCodeUnit interface {
		AppendInstruction(operation OperationCode, labels []string, operands ...*Operand)
		AppendPrefixedInstruction(prefix, operation OperationCode, labels []string, operands ...*Operand)
		AppendReadOnlyDataItem(kind ReadOnlyDataKind, labels []string, value any)
		AppendRuntime()
		Length() int
		GetInstruction(index int) *Instruction
		Print(print io.Writer, args ...any) error
		Export(format cor.ExportFormat, print io.Writer) error
	}
)

// Return the interface of the assembly code unit implementation.
func NewAssemblyCodeUnit(outputKind OutputKind) AssemblyCodeUnit {
	return newAssemblyCodeUnit(outputKind)
}

// Create a new assembly instruction with an operation code, some labels, and operands.
func NewInstruction(operation OperationCode, labels []string, operands ...*Operand) *Instruction {
	return &Instruction{Prefix: None, Operation: operation, Operands: operands, Labels: labels}
}

// Create a new assembly instruction with a prefix operation code, an operation code, some labels, and operands.
func NewPrefixedInstruction(prefix, operation OperationCode, labels []string, operands ...*Operand) *Instruction {
	return &Instruction{Prefix: prefix, Operation: operation, Operands: operands, Labels: labels}
}

// Create a new register operand for an assembly instruction.
func NewRegisterOperand(register Register) *Operand {
	return &Operand{Kind: RegisterOperand, Register: register}
}

// Create a new immediate operand for an assembly instruction.
func NewImmediateOperand(value any) *Operand {
	return &Operand{Kind: ImmediateOperand, Immediate: value}
}

// Ceate a new memory operand for an assembly instruction.
func NewMemoryOperand(register Register, size OperandSize, displacement int32) *Operand {
	return &Operand{Kind: MemoryOperand, Register: register, Memory: MemoryDetail{Size: size, Displacement: displacement}}
}

// Create a new label operand for an assembly instruction.
func NewLabelOperand(label string) *Operand {
	return &Operand{Kind: LabelOperand, Label: label}
}

// Create a new read-only data item for a read-only section.
func NewReadOnlyDataItem(kind ReadOnlyDataKind, labels []string, value any) *ReadOnlyDataItem {
	return &ReadOnlyDataItem{Kind: kind, Labels: labels, Value: value}
}

// String representation of a CPU operation code.
func (oc OperationCode) String() string {
	return operationNames[oc]
}

// String representation of a CPU register.
func (r Register) String() string {
	return registerNames[r]
}

// String representation of a CPU operand size.
func (s OperandSize) String() string {
	return operandSizeNames[s]
}

// String representation of an operand kind of CPU operations.
func (o *Operand) String() string {
	switch o.Kind {
	case RegisterOperand:
		return o.Register.String()

	case ImmediateOperand:
		return fmt.Sprintf("%v", o.Immediate)

	case MemoryOperand:
		if o.Memory.Displacement != 0 {
			return fmt.Sprintf("%v ptr [%v%+d]", o.Memory.Size, o.Register, o.Memory.Displacement)
		}

		return fmt.Sprintf("%v ptr [%v]", o.Memory.Size, o.Register)

	case LabelOperand:
		return o.Label

	default:
		panic(cor.NewGeneralError(cor.Assembly, failureMap, cor.Fatal, unknownKindOfOperandInCpuOperation, o.Kind, nil))
	}
}

// String representation of an assembly instruction.
func (i *Instruction) String() string {
	var buffer bytes.Buffer

	for _, label := range i.Labels {
		buffer.WriteString(label)
		buffer.WriteString(":\n")
	}

	buffer.WriteString(fmt.Sprintf("  %-12v", strings.TrimSpace(fmt.Sprintf("%v %v", i.Prefix, i.Operation))))

	for _, op := range i.Operands {
		buffer.WriteString(op.String())
		buffer.WriteString(", ")
	}

	return strings.TrimSuffix(buffer.String(), ", ")
}

// Round up or down an offset to its next alignment boundary. If something is wrong with the offset or alignment, return 0 (offset has a +/- 2GB limit).
func Align(offset, alignment int32) int32 {
	// validate and make sure the alignment is a power of 2
	if alignment <= 0 || alignment&(alignment-1) != 0 {
		return 0
	}

	// calculate the alignment of the offset depending on its sign (round up or down)
	if offset >= 0 {
		return (offset + alignment - 1) & ^(alignment - 1)
	} else {
		return -(((-offset) + alignment - 1) & ^(alignment - 1))
	}
}
