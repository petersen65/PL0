// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// Data types of literals, constants, and variables.
// The first 8 bits (0-7) are used for the plain data type, the next bits (8+) are used for modifiers.
const (
	Integer64  DataType = iota // signed 64-bit integer
	Integer32                  // signed 32-bit integer
	Integer16                  // signed 16-bit integer
	Integer8                   // signed 8-bit integer
	Float64                    // IEEE 754 64-bit floating-point number
	Float32                    // IEEE 754 32-bit floating-point number
	Unsigned64                 // unsigned 64-bit integer
	Unsigned32                 // unsigned 32-bit integer
	Unsigned16                 // unsigned 16-bit integer
	Unsigned8                  // unsigned 8-bit integer
	Boolean                    // unsigned 8-bit boolean (0 or 1, false or true)
	Character                  // Unicode code point (signed 32-bit integer, U+0000 ... U+10FFFF)
	String                     // Encoded string (sequence of UTF encoded characters)
)

// Data type bit flags for pointer and reference modifiers (bits 8+).
const (
	Pointer   DataType = 1 << 8 // bit 8: pointer type (^T)
	Reference DataType = 1 << 9 // bit 9: reference type (var T)
)

type (
	// The datatype of a symbol.
	DataType int
)

// Return the plain data type without modifiers.
func (dt DataType) AsPlain() DataType {
	return dt & 0xFF
}

// Return the plain data type with a pointer modifier.
func (dt DataType) AsPointer() DataType {
	return dt.AsPlain() | Pointer
}

// Check whether the data type is a pointer type.
func (dt DataType) IsPointer() bool {
	return dt&Pointer != 0
}

// Return the plain data type with a reference modifier.
func (dt DataType) AsReference() DataType {
	return dt.AsPlain() | Reference
}

// Check whether the data type is a reference type.
func (dt DataType) IsReference() bool {
	return dt&Reference != 0
}
