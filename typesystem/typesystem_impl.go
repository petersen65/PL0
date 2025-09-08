// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package typesystem

import (
	plt "github.com/petersen65/pl0/v3/platform"
)

// Common fields shared by all type descriptors.
type commonTypeDescriptor struct {
	Abi   plt.ApplicationBinaryInterface `json:"application_binary_interface"` // ABI governing size and alignment calculations
	Kind_ DataTypeKind                   `json:"data_type_kind"`               // kind of data type (e.g., simple, structure, pointer)
}

// Map a data type kind to its string representation.
var dataTypeKindNames = map[DataTypeKind]string{
	DataTypeSimple:    "simple",
	DataTypePointer:   "pointer",
	DataTypeReference: "reference",
	DataTypeStructure: "structure",
	DataTypeFunction:  "function",
	DataTypeProcedure: "procedure",
}

// Kind of data type represented by the common type descriptor.
func (d *commonTypeDescriptor) Kind() DataTypeKind {
	return d.Kind_
}

// Check if the common type descriptor represents a pointer data type.
func (d *commonTypeDescriptor) IsPointer() bool {
	return d.Kind_ == DataTypePointer || d.Kind_ == DataTypeFunction || d.Kind_ == DataTypeProcedure
}

// Check if the common type descriptor represents a reference data type.
func (d *commonTypeDescriptor) IsReference() bool {
	return d.Kind_ == DataTypeReference
}

// Check if the common type descriptor represents an integer data type.
func (d *commonTypeDescriptor) IsInteger() bool {
	return false
}

// Check if the common type descriptor represents a floating-point data type.
func (d *commonTypeDescriptor) IsFloatingPoint() bool {
	return false
}

// Check if the common type descriptor represents a data type that has a sign.
func (d *commonTypeDescriptor) IsSigned() bool {
	return false
}

// Check if the common type descriptor represents a data type that does not have a sign.
func (d *commonTypeDescriptor) IsUnsigned() bool {
	return false
}
