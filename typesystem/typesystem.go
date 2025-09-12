// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package typesystem provides a type system that is used for all data type representations in the compiler.
package typesystem

// All supported kinds of data types provided by the type system.
const (
	// simple types
	DataTypeSimple DataTypeKind = iota // simple types with an underlying primitive type

	// pointer types
	DataTypePointer // a pointer type holds the memory address of another value

	// composite types
	DataTypeStructure // a structure type is a composite type with named fields

	// function or procedure types
	DataTypeFunction  // the function type describes a function with parameters and a return type
	DataTypeProcedure // the procedure type describes a function with parameters but no return value
)

// Each data type has a specific set of capabilities that define what operations are supported on that type.
const (
	Numeric         DataTypeCapability = 1 << iota // supports numeric operations +, -, *, /
	Ordered                                        // supports ordering operations <, <=, >, >=
	Equality                                       // supports equality operations ==, !=
	Logical                                        // supports logical operations &&, ||, !
	Bitwise                                        // supports bitwise operations &, |, ^, <<, >>
	Integral                                       // operations on whole numbers (signed and unsigned integers)
	Fractional                                     // operations on decimal numbers with fractional parts (floating-point)
	Dereferenceable                                // supports dereferencing operation *
	Negatable                                      // supports negation operation only for signed numeric types
	Callable                                       // supports procedure call statement
	Valued                                         // supports return value in function call expression
)

type (
	// Kind of data type (e.g., simple, composite, pointer).
	DataTypeKind int

	// DataTypeCapability is a bit-mask that represents what operations a type supports.
	DataTypeCapability uint64

	// The type checker interface provides checks for the characteristics of a type descriptor.
	TypeChecker interface {
		IsBuiltIn() bool
		IsPointer() bool
		IsIntegral() bool
		IsFractional() bool
		IsSigned() bool
		IsUnsigned() bool
		HasCapability(cap DataTypeCapability) bool
		HasAllCapabilities(capabilities DataTypeCapability) bool
		HasAnyCapability(capabilities DataTypeCapability) bool
	}

	// The type descriptor is the interface for all specific type descriptions.
	TypeDescriptor interface {
		TypeChecker
		String() string
		Kind() DataTypeKind
		Size() int
		Alignment() int
		Equal(other TypeDescriptor) bool
		Capabilities() DataTypeCapability
	}
)

// String representation of a data type kind.
func (k DataTypeKind) String() string {
	return dataTypeKindNames[k]
}
