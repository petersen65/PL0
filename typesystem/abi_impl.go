// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package typesystem

import (
	eh "github.com/petersen65/pl0/v3/errors"
	plt "github.com/petersen65/pl0/v3/platform"
)

// Packing rules define how structure based data types are packed.
const (
	naturalPacking   packingRule = iota // align each field to its natural alignment, max struct alignment 16
	microsoftPacking                    // like natural packing, but max struct alignment 8, nested structs capped at 8
)

type (
	// A packing rule defines how structure based data types are packed.
	packingRule int

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
	// Current application binary interface specification.
	currentABI plt.ApplicationBinaryInterface = plt.ABI_NotSpecified

	// Application binary interface specifications for modern 64-bit platforms.
	applicationBinaryInterfaceSpecifications = map[plt.ApplicationBinaryInterface]*applicationBinaryInterfaceSpecification{
		plt.ABI_SystemV_AMD64: {
			Name:             plt.ABI_SystemV_AMD64.String(),
			PointerSize:      8,
			PointerAlignment: 8,
			TypeSizes: map[PrimitiveDataType]int{
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
		plt.ABI_Microsoft_x64: {
			Name:             plt.ABI_Microsoft_x64.String(),
			PointerSize:      8,
			PointerAlignment: 8,
			TypeSizes: map[PrimitiveDataType]int{
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
		plt.ABI_AAPCS64: {
			Name:             plt.ABI_AAPCS64.String(),
			PointerSize:      8,
			PointerAlignment: 8,
			TypeSizes: map[PrimitiveDataType]int{
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
		plt.ABI_Windows_ARM64: {
			Name:             plt.ABI_Windows_ARM64.String(),
			PointerSize:      8,
			PointerAlignment: 8,
			TypeSizes: map[PrimitiveDataType]int{
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
)

// Enforce that the current application binary interface has been specified and panic if not.
func enforceSpecifiedApplicationBinaryInterface() {
	if currentABI == plt.ABI_NotSpecified {
		panic(eh.NewGeneralError(eh.TypeSystem, failureMap, eh.Fatal, applicationBinaryInterfaceNotSpecified, nil))
	}
}
