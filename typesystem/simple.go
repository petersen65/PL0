// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package typesystem

// Primitive data types that are built-in and cannot be further refined.
const (
	Integer64  PrimitiveDataType = iota // signed 64-bit integer
	Integer32                           // signed 32-bit integer
	Integer16                           // signed 16-bit integer
	Integer8                            // signed 8-bit integer
	Float64                             // IEEE 754 64-bit floating-point number
	Float32                             // IEEE 754 32-bit floating-point number
	Unsigned64                          // unsigned 64-bit integer
	Unsigned32                          // unsigned 32-bit integer
	Unsigned16                          // unsigned 16-bit integer
	Unsigned8                           // unsigned 8-bit integer
	Boolean                             // unsigned 8-bit boolean (0 or 1, false or true)
	Character                           // Unicode code point (signed 32-bit integer, U+0000 ... U+10FFFF)
	String                              // encoded string (sequence of UTF encoded characters)
)

// Built-in primitive data types.
type PrimitiveDataType int

// Create a new simple type descriptor with an underlying primitive type.
func NewSimpleTypeDescriptor(primitiveType PrimitiveDataType) TypeDescriptor {
	enforceSpecifiedApplicationBinaryInterface()

	return &simpleTypeDescriptor{
		commonTypeDescriptor: commonTypeDescriptor{Abi: currentABI, Kind_: DataTypeSimple},
		PrimitiveType:        primitiveType,
	}
}

// String representation of a primitive data type.
func (t PrimitiveDataType) String() string {
	return primitiveDataTypeNames[t]
}

// String representation of a primitive data type as a pointer type.
func (t PrimitiveDataType) PointerString() string {
	return pointerPrefix + primitiveDataTypeNames[t]
}

// String representation of a primitive data type as a reference type.
func (t PrimitiveDataType) ReferenceString() string {
	return referencePrefix + primitiveDataTypeNames[t]
}
