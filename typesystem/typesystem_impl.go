// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package typesystem

import (
	plt "github.com/petersen65/pl0/v3/platform"
)

// Common fields shared by all type descriptors.
type commonTypeDescriptor struct {
	Abi     plt.ApplicationBinaryInterface `json:"application_binary_interface"` // ABI governing size and alignment calculations
	Kind_   DataTypeKind                   `json:"data_type_kind"`               // kind of data type (e.g., simple, structure, pointer)
	BuiltIn bool                           `json:"built_in"`                     // indicates if this type is a built-in data type
}

var (
	// Map data type kinds to their string representation.
	dataTypeKindNames = map[DataTypeKind]string{
		DataTypeSimple:    "simple",
		DataTypePointer:   "pointer",
		DataTypeStructure: "structure",
		DataTypeFunction:  "function",
		DataTypeProcedure: "procedure",
	}
)

// Kind of data type represented by the common type descriptor.
func (d *commonTypeDescriptor) Kind() DataTypeKind {
	return d.Kind_
}

// The common type descriptor represents a built-in data type.
func (d *commonTypeDescriptor) IsBuiltIn() bool {
	return d.BuiltIn
}

// Check if the common type descriptor represents a pointer data type.
func (d *commonTypeDescriptor) IsPointer() bool {
	return d.HasCapability(Dereferenceable)
}

// Check if the common type descriptor represents an integral data type.
func (d *commonTypeDescriptor) IsIntegral() bool {
	return d.HasCapability(Integral)
}

// Check if the common type descriptor represents a fractional data type.
func (d *commonTypeDescriptor) IsFractional() bool {
	return d.HasCapability(Fractional)
}

// Check if the common type descriptor represents a data type that has a sign.
func (d *commonTypeDescriptor) IsSigned() bool {
	return d.HasCapability(Negatable)
}

// Check if the common type descriptor represents a data type that does not have a sign.
func (d *commonTypeDescriptor) IsUnsigned() bool {
	return !d.HasCapability(Negatable)
}

// Check if the common type descriptor has the specified capability.
func (d *commonTypeDescriptor) HasCapability(capability DataTypeCapability) bool {
	return d.Capabilities()&capability != 0
}

// Default data type capabilities of the common type descriptor.
func (d *commonTypeDescriptor) Capabilities() DataTypeCapability {
	return 0
}
