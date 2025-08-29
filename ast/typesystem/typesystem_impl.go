// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package typesystem

import (
	"fmt"

	cor "github.com/petersen65/pl0/v3/core"
)

// Prefixes for pointer and reference modifier string representations.
const (
	pointerPrefix   = "^"
	referencePrefix = "var "
)

// Packing rules define how structure based data types are packed.
const (
	naturalPacking   packingRule = iota // align each field to its natural alignment
	microsoftPacking                    // Microsoft-specific packing rules
)

type (
	// A packing rule defines how structure based data types are packed.
	packingRule int

	// Primitive data types that cannot be further refined.
	simpleTypeDescriptor struct {
		PrimitiveType PrimitiveDataType              `json:"primitive_type"` // underlying primitive data type
		Abi           cor.ApplicationBinaryInterface `json:"abi"`            // ABI governing size and alignment calculations
	}

	// Pointer type that represents a memory address pointing to a value with a specific data type.
	pointerTypeDescriptor struct {
		ValueType   TypeDescriptor                 `json:"value_type"`   // type descriptor of the value that this pointer references
		IsReference bool                           `json:"is_reference"` // true if this is a reference rather than a raw pointer
		Abi         cor.ApplicationBinaryInterface `json:"abi"`          // ABI governing size and alignment calculations
	}

	// The application binary interface specification contains size, alignment, and packing rules.
	applicationBinaryInterfaceSpecification struct {
		Name              string                    `json:"name"`                // name of the ABI specification
		PointerSize       int                       `json:"pointer_size"`        // size of pointers in bytes
		PointerAlignment  int                       `json:"pointer_alignment"`   // alignment requirement for pointers
		TypeSizes         map[PrimitiveDataType]int `json:"type_sizes"`          // size mapping for each primitive type
		TypeAlignments    map[PrimitiveDataType]int `json:"type_alignments"`     // alignment mapping for each primitive type
		StructPackingRule packingRule               `json:"struct_packing_rule"` // rule for packing struct fields
		MaxAlignment      int                       `json:"max_alignment"`       // maximum alignment allowed
	}
)

var (
	// Application binary interface specifications for modern 64-bit platforms.
	applicationBinaryInterfaceSpecifications = map[cor.ApplicationBinaryInterface]*applicationBinaryInterfaceSpecification{
		cor.ABI_SystemV_AMD64: {
			Name:             cor.ABI_SystemV_AMD64.String(),
			PointerSize:      8,
			PointerAlignment: 8,
			TypeSizes: map[PrimitiveDataType]int{
				Bit:        0,
				Integer8:   1,
				Integer16:  2,
				Integer32:  4,
				Integer64:  8,
				Float32:    4,
				Float64:    8,
				Unsigned8:  1,
				Unsigned16: 2,
				Unsigned32: 4,
				Unsigned64: 8,
				Boolean:    1,
				Character:  4,
				String:     16,
			},
			TypeAlignments: map[PrimitiveDataType]int{
				Bit:        1,
				Integer8:   1,
				Integer16:  2,
				Integer32:  4,
				Integer64:  8,
				Float32:    4,
				Float64:    8,
				Unsigned8:  1,
				Unsigned16: 2,
				Unsigned32: 4,
				Unsigned64: 8,
				Boolean:    1,
				Character:  4,
				String:     8,
			},
			StructPackingRule: naturalPacking,
			MaxAlignment:      16,
		},
		cor.ABI_Microsoft_x64: {
			Name:             cor.ABI_Microsoft_x64.String(),
			PointerSize:      8,
			PointerAlignment: 8,
			TypeSizes: map[PrimitiveDataType]int{
				Bit:        0,
				Integer8:   1,
				Integer16:  2,
				Integer32:  4,
				Integer64:  8,
				Float32:    4,
				Float64:    8,
				Unsigned8:  1,
				Unsigned16: 2,
				Unsigned32: 4,
				Unsigned64: 8,
				Boolean:    1,
				Character:  4,
				String:     16,
			},
			TypeAlignments: map[PrimitiveDataType]int{
				Bit:        1,
				Integer8:   1,
				Integer16:  2,
				Integer32:  4,
				Integer64:  8,
				Float32:    4,
				Float64:    8,
				Unsigned8:  1,
				Unsigned16: 2,
				Unsigned32: 4,
				Unsigned64: 8,
				Boolean:    1,
				Character:  4,
				String:     8,
			},
			StructPackingRule: microsoftPacking,
			MaxAlignment:      8,
		},
		cor.ABI_AAPCS64: {
			Name:             cor.ABI_AAPCS64.String(),
			PointerSize:      8,
			PointerAlignment: 8,
			TypeSizes: map[PrimitiveDataType]int{
				Bit:        0,
				Integer8:   1,
				Integer16:  2,
				Integer32:  4,
				Integer64:  8,
				Float32:    4,
				Float64:    8,
				Unsigned8:  1,
				Unsigned16: 2,
				Unsigned32: 4,
				Unsigned64: 8,
				Boolean:    1,
				Character:  4,
				String:     16,
			},
			TypeAlignments: map[PrimitiveDataType]int{
				Bit:        1,
				Integer8:   1,
				Integer16:  2,
				Integer32:  4,
				Integer64:  8,
				Float32:    4,
				Float64:    8,
				Unsigned8:  1,
				Unsigned16: 2,
				Unsigned32: 4,
				Unsigned64: 8,
				Boolean:    1,
				Character:  4,
				String:     8,
			},
			StructPackingRule: naturalPacking,
			MaxAlignment:      16,
		},
		cor.ABI_Windows_ARM64: {
			Name:             cor.ABI_Windows_ARM64.String(),
			PointerSize:      8,
			PointerAlignment: 8,
			TypeSizes: map[PrimitiveDataType]int{
				Bit:        0,
				Integer8:   1,
				Integer16:  2,
				Integer32:  4,
				Integer64:  8,
				Float32:    4,
				Float64:    8,
				Unsigned8:  1,
				Unsigned16: 2,
				Unsigned32: 4,
				Unsigned64: 8,
				Boolean:    1,
				Character:  4,
				String:     16,
			},
			TypeAlignments: map[PrimitiveDataType]int{
				Bit:        1,
				Integer8:   1,
				Integer16:  2,
				Integer32:  4,
				Integer64:  8,
				Float32:    4,
				Float64:    8,
				Unsigned8:  1,
				Unsigned16: 2,
				Unsigned32: 4,
				Unsigned64: 8,
				Boolean:    1,
				Character:  4,
				String:     8,
			},
			StructPackingRule: microsoftPacking,
			MaxAlignment:      8,
		},
	}

	// Map a primitive data type to its string representation.
	dataTypeNames = map[PrimitiveDataType]string{
		Integer64:  "int64",
		Integer32:  "int32",
		Integer16:  "int16",
		Integer8:   "int8",
		Float64:    "float64",
		Float32:    "float32",
		Unsigned64: "uint64",
		Unsigned32: "uint32",
		Unsigned16: "uint16",
		Unsigned8:  "uint8",
		Boolean:    "bool",
		Character:  "char",
		String:     "string",
	}

	// Map a data type kind to its string representation.
	dataTypeKindNames = map[DataTypeKind]string{
		DataTypeSimple:    "simple",
		DataTypePointer:   "pointer",
		DataTypeStructure: "structure",
	}
)

// String representation of a primitive data type.
func (t PrimitiveDataType) String() string {
	var prefix string

	if t.IsPointer() {
		prefix = pointerPrefix
	} else if t.IsReference() {
		prefix = referencePrefix
	}

	return fmt.Sprintf("%v%v", prefix, dataTypeNames[t.AsPlain()])
}

// Data type classification for simple type descriptors.
func (d *simpleTypeDescriptor) Kind() DataTypeKind {
	return DataTypeSimple
}

// String representation of the primitive type.
func (d *simpleTypeDescriptor) String() string {
	return d.PrimitiveType.String()
}

// Depending on the ABI, calculate the memory size in bytes for the simple type descriptor.
func (d *simpleTypeDescriptor) Size() int {
	return applicationBinaryInterfaceSpecifications[d.Abi].TypeSizes[d.PrimitiveType]
}

// Depending on the ABI, determine the memory alignment requirement for the simple type descriptor.
func (d *simpleTypeDescriptor) Alignment() int {
	return applicationBinaryInterfaceSpecifications[d.Abi].TypeAlignments[d.PrimitiveType]
}

// Data type classification for pointer type descriptors.
func (d *pointerTypeDescriptor) Kind() DataTypeKind {
	return DataTypePointer
}

// String representation of the pointer type.
func (d *pointerTypeDescriptor) String() string {
	if d.IsReference {
		return referencePrefix + d.ValueType.String()
	}

	return pointerPrefix + d.ValueType.String()
}

// Depending on the ABI, calculate the memory size in bytes for the pointer type descriptor.
func (d *pointerTypeDescriptor) Size() int {
	return applicationBinaryInterfaceSpecifications[d.Abi].PointerSize
}

// Depending on the ABI, determine the memory alignment requirement for the pointer type descriptor.
func (d *pointerTypeDescriptor) Alignment() int {
	return applicationBinaryInterfaceSpecifications[d.Abi].PointerAlignment
}
