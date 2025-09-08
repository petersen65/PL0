// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package typesystem provides a type system that is used for all data type representations in the compiler.
package typesystem

// All supported kinds of data types provided by the type system.
const (
	// simple types
	DataTypeSimple DataTypeKind = iota // simple types with an underlying primitive type

	// pointer types
	DataTypePointer   // a pointer type holds the memory address of another value
	DataTypeReference // the reference type is an alias for a pointer type

	// composite types
	DataTypeStructure // a structure type is a composite type with named fields

	// function or procedure types
	DataTypeFunction  // the function type describes a function with parameters and a return type
	DataTypeProcedure // the procedure type describes a function with parameters but no return value
)

type (
	// Kind of data type (e.g., simple, composite, pointer).
	DataTypeKind int

	// The type checker interface provides checks for the characteristics of a type descriptor.
	TypeChecker interface {
		IsPointer() bool
		IsReference() bool
		IsInteger() bool
		IsFloatingPoint() bool
		IsSigned() bool
		IsUnsigned() bool
	}

	// The type descriptor is the interface for all specific type descriptions.
	TypeDescriptor interface {
		TypeChecker
		String() string
		Kind() DataTypeKind
		Size() int
		Alignment() int
		Equal(other TypeDescriptor) bool
	}
)

// String representation of a data type kind.
func (k DataTypeKind) String() string {
	return dataTypeKindNames[k]
}
