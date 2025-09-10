// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package typesystem

// Primitive data types that cannot be further refined.
type simpleTypeDescriptor struct {
	commonTypeDescriptor                   // embedded common type descriptor
	PrimitiveDataType_   PrimitiveDataType `json:"primitive_type"` // underlying primitive data type
}

var (
	// Map primitive data types to their data type capabilities.
	primitiveDataTypeCapabilities = map[PrimitiveDataType]DataTypeCapability{
		Integer64:  Integral | Numeric | Ordered | Equality | Bitwise | Negatable,
		Integer32:  Integral | Numeric | Ordered | Equality | Bitwise | Negatable,
		Integer16:  Integral | Numeric | Ordered | Equality | Bitwise | Negatable,
		Integer8:   Integral | Numeric | Ordered | Equality | Bitwise | Negatable,
		Float64:    Fractional | Numeric | Ordered | Equality | Bitwise | Negatable,
		Float32:    Fractional | Numeric | Ordered | Equality | Bitwise | Negatable,
		Unsigned64: Integral | Numeric | Ordered | Equality | Bitwise,
		Unsigned32: Integral | Numeric | Ordered | Equality | Bitwise,
		Unsigned16: Integral | Numeric | Ordered | Equality | Bitwise,
		Unsigned8:  Integral | Numeric | Ordered | Equality | Bitwise,
		Boolean:    Equality | Logical,
		Character:  Equality | Ordered,
		String:     Equality | Ordered,
	}

	// Map primitive data types to their string representation.
	primitiveDataTypeNames = map[PrimitiveDataType]string{
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
)

// String representation of the simple type descriptor.
func (d *simpleTypeDescriptor) String() string {
	return d.PrimitiveDataType_.String()
}

// Depending on the ABI, calculate the memory size in bytes for the simple type descriptor.
func (d *simpleTypeDescriptor) Size() int {
	return applicationBinaryInterfaceSpecifications[d.Abi].TypeSizes[d.PrimitiveDataType_]
}

// Depending on the ABI, determine the memory alignment requirement for the simple type descriptor.
func (d *simpleTypeDescriptor) Alignment() int {
	return applicationBinaryInterfaceSpecifications[d.Abi].TypeAlignments[d.PrimitiveDataType_]
}

// Check if the simple type descriptor is equal to another type descriptor.
func (d *simpleTypeDescriptor) Equal(other TypeDescriptor) bool {
	// if either type descriptor is nil, they are not equal
	if d == nil || other == nil {
		return false
	}

	// detect self-comparison early and indicate equality to avoid unnecessary work
	if d == other {
		return true
	}

	// check if the other type descriptor is also a simple type descriptor
	if o, ok := other.(*simpleTypeDescriptor); ok {
		return d.PrimitiveDataType_ == o.PrimitiveDataType_
	}

	// data types are not equal if they are of different type descriptors
	return false
}

// Data type capabilities of the simple type descriptor.
func (d *simpleTypeDescriptor) Capabilities() DataTypeCapability {
	return primitiveDataTypeCapabilities[d.PrimitiveDataType_]
}
