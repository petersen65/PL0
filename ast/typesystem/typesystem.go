// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package typesystem provides a type system that is used for data type representation in the abstract syntax tree.
package typesystem

import cor "github.com/petersen65/pl0/v3/core"

// Primitive data types that are built-in and cannot be further refined.
// The first 8 bits (0-7) are used for the plain data type, the next bits (8+) are used for modifiers.
const (
	Bit        PrimitiveDataType = iota // single bit that the compiler packs into bytes, words, longs, or quad words
	Integer64                           // signed 64-bit integer
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

// Primitive data type bit flags for pointer and reference modifiers (bits 8+).
const (
	Pointer   PrimitiveDataType = 1 << 8 // bit 8: pointer type (^T)
	Reference PrimitiveDataType = 1 << 9 // bit 9: reference type (var T)
)

// All supported kinds of data types provided by the type system.
const (
	// simple types
	DataTypeSimple DataTypeKind = iota

	// pointer types
	DataTypePointer // complex pointer hierarchies

	// composite types
	DataTypeStructure // structures with named fields
)

type (
	// Primitive data types.
	PrimitiveDataType int

	// Kind of data type (e.g., simple, composite, pointer).
	DataTypeKind int

	// The type descriptor is the common interface for all specific type descriptions.
	TypeDescriptor interface {
		Kind() DataTypeKind
		String() string
		Size() int
		Alignment() int
	}
)

// Create a new simple type descriptor with an underlying primitive type and a governing ABI.
func NewSimpleTypeDescriptor(primitiveType PrimitiveDataType, abi cor.ApplicationBinaryInterface) TypeDescriptor {
	return &simpleTypeDescriptor{PrimitiveType: primitiveType, Abi: abi}
}

// Create a new pointer type descriptor with a value type and a governing ABI.
func NewPointerTypeDescriptor(valueType TypeDescriptor, isReference bool, abi cor.ApplicationBinaryInterface) TypeDescriptor {
	return &pointerTypeDescriptor{ValueType: valueType, IsReference: isReference, Abi: abi}
}

// Return the string representation of a data type kind.
func (k DataTypeKind) String() string {
	return dataTypeKindNames[k]
}

// Return the plain data type without modifiers.
func (t PrimitiveDataType) AsPlain() PrimitiveDataType {
	return t & 0xFF
}

// Return the plain data type with a pointer modifier.
func (t PrimitiveDataType) AsPointer() PrimitiveDataType {
	return t.AsPlain() | Pointer
}

// Check whether the data type is a pointer type.
func (t PrimitiveDataType) IsPointer() bool {
	return t&Pointer != 0
}

// Return the plain data type with a reference modifier.
func (t PrimitiveDataType) AsReference() PrimitiveDataType {
	return t.AsPlain() | Reference
}

// Check whether the data type is a reference type.
func (t PrimitiveDataType) IsReference() bool {
	return t&Reference != 0
}
