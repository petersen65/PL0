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

var (
	// dataTypeNames maps a data type to its string representation.
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

	// Application binary interface specifications for modern 64-bit platforms.
	applicationBinaryInterfaceSpecifications = map[cor.ApplicationBinaryInterface]*ApplicationBinaryInterface{
		cor.ABI_SystemV_AMD64: {
			Name:             cor.ABI_SystemV_AMD64.String(),
			PointerSize:      8,
			PointerAlignment: 8,
			TypeSizes: map[PrimitiveDataType]int64{
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
			TypeAlignments: map[PrimitiveDataType]int64{
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
			StructPackingRule: NaturalPacking,
			MaxAlignment:      16,
		},
		cor.ABI_Microsoft_x64: {
			Name:             cor.ABI_Microsoft_x64.String(),
			PointerSize:      8,
			PointerAlignment: 8,
			TypeSizes: map[PrimitiveDataType]int64{
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
			TypeAlignments: map[PrimitiveDataType]int64{
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
			StructPackingRule: MicrosoftPacking,
			MaxAlignment:      8,
		},
		cor.ABI_AAPCS64: {
			Name:             cor.ABI_AAPCS64.String(),
			PointerSize:      8,
			PointerAlignment: 8,
			TypeSizes: map[PrimitiveDataType]int64{
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
			TypeAlignments: map[PrimitiveDataType]int64{
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
			StructPackingRule: NaturalPacking,
			MaxAlignment:      16,
		},
		cor.ABI_Windows_ARM64: {
			Name:             cor.ABI_Windows_ARM64.String(),
			PointerSize:      8,
			PointerAlignment: 8,
			TypeSizes: map[PrimitiveDataType]int64{
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
			TypeAlignments: map[PrimitiveDataType]int64{
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
			StructPackingRule: MicrosoftPacking,
			MaxAlignment:      8,
		},
	}
)

// String representation of a data type.
func (t PrimitiveDataType) String() string {
	var prefix string

	if t.IsPointer() {
		prefix = pointerPrefix
	} else if t.IsReference() {
		prefix = referencePrefix
	}

	return fmt.Sprintf("%v%v", prefix, dataTypeNames[t.AsPlain()])
}
