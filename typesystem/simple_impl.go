// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package typesystem

// Primitive data types that cannot be further refined.
type simpleTypeDescriptor struct {
	commonTypeDescriptor                   // embedded common type descriptor
	PrimitiveType        PrimitiveDataType `json:"primitive_type"` // underlying primitive data type
}

// Map a primitive datas type to their string representation.
var primitiveDataTypeNames = map[PrimitiveDataType]string{
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

// String representation of the simple type.
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

// Check if the simple type descriptor is equal to another type descriptor.
func (d *simpleTypeDescriptor) Equal(other TypeDescriptor) bool {
	// check if the other type descriptor is also a simple type descriptor
	if o, ok := other.(*simpleTypeDescriptor); ok {
		return d.PrimitiveType == o.PrimitiveType
	}

	// data types are not equal if they are of different type descriptors
	return false
}

// Check if the simple type descriptor represents an integer data type.
func (d *simpleTypeDescriptor) IsInteger() bool {
	switch d.PrimitiveType {
	case Integer8, Integer16, Integer32, Integer64, Unsigned8, Unsigned16, Unsigned32, Unsigned64:
		return true

	default:
		return false
	}
}

// Check if the simple type descriptor represents a floating-point data type.
func (d *simpleTypeDescriptor) IsFloatingPoint() bool {
	switch d.PrimitiveType {
	case Float32, Float64:
		return true

	default:
		return false
	}
}

// Check if the simple type descriptor represents a data type that has a sign.
func (d *simpleTypeDescriptor) IsSigned() bool {
	switch d.PrimitiveType {
	case Integer8, Integer16, Integer32, Integer64, Float32, Float64:
		return true

	default:
		return false
	}
}

// Check if the simple type descriptor represents a data type that does not have a sign.
func (d *simpleTypeDescriptor) IsUnsigned() bool {
	switch d.PrimitiveType {
	case Unsigned8, Unsigned16, Unsigned32, Unsigned64:
		return true

	default:
		return false
	}
}
