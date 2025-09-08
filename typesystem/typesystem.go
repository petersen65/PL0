// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package typesystem provides a type system that is used for all data type representations in the compiler.
package typesystem

import plt "github.com/petersen65/pl0/v3/platform"

// Primitive data types that are built-in and cannot be further refined.
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

// Parameter passing modes for functions and procedures.
const (
	CallByValue          ParameterPassingMode = iota // call by value
	CallByReference                                  // call by reference
	CallByConstReference                             // call by const reference
	OutputParameter                                  // output parameter
)

type (
	// Built-in primitive data types.
	PrimitiveDataType int

	// Kind of data type (e.g., simple, composite, pointer).
	DataTypeKind int

	// Mode for parameter passing in functions and procedures.
	ParameterPassingMode int

	// Structure field describes a field in a structure type descriptor.
	StructureField struct {
		Name       string         `json:"name"`      // name identifier used to access this field within the structure
		TypeName   string         `json:"type_name"` // type string defining the data type name of this field
		Type       TypeDescriptor `json:"type"`      // type descriptor defining the data type of this field
		ByteOffset int            `json:"offset"`    // byte offset is automatically calculated by the type descriptor
	}

	// Parameter and its passing mode for a function or procedure call.
	FunctionParameter struct {
		Name     string               `json:"name"`      // identifier name used to reference this parameter within the function
		TypeName string               `json:"type_name"` // type string defining the data type name of this parameter
		Type     TypeDescriptor       `json:"type"`      // type descriptor defining the data type of this parameter
		Mode     ParameterPassingMode `json:"mode"`      // determines how the parameter is passed (e.g., by value, by reference)
	}

	// The type descriptor is the interface for all specific type descriptions.
	TypeDescriptor interface {
		String() string
		Kind() DataTypeKind
		Size() int
		Alignment() int
	}
)

// Create a new simple type descriptor with an underlying primitive type.
func NewSimpleTypeDescriptor(primitiveType PrimitiveDataType) TypeDescriptor {
	enforceSpecifiedApplicationBinaryInterface()

	return &simpleTypeDescriptor{
		commonTypeDescriptor: commonTypeDescriptor{Abi: currentABI},
		PrimitiveType:        primitiveType,
	}
}

// Create a new pointer type descriptor with a value type.
func NewPointerTypeDescriptor(valueType TypeDescriptor, isReference bool) TypeDescriptor {
	enforceSpecifiedApplicationBinaryInterface()

	return &pointerTypeDescriptor{
		commonTypeDescriptor: commonTypeDescriptor{Abi: currentABI},
		ValueType:            valueType,
		IsReference:          isReference,
	}
}

// Create a new structure field with a name and field type information.
// A structure field can either be used with a type name or a type descriptor or both. The type name can be empty or the type descriptor can be nil.
func NewStructureField(fieldName string, fieldTypeName string, fieldType TypeDescriptor) *StructureField {
	return &StructureField{
		Name:     fieldName,
		TypeName: fieldTypeName,
		Type:     fieldType,
	}
}

// Create a new structure type descriptor with fields.
func NewStructureTypeDescriptor(structureTypeName string, fields []*StructureField, isPacked bool) TypeDescriptor {
	enforceSpecifiedApplicationBinaryInterface()

	if fields == nil {
		fields = make([]*StructureField, 0)
	}

	return &structureTypeDescriptor{
		commonTypeDescriptor: commonTypeDescriptor{Abi: currentABI},
		TypeName:             structureTypeName,
		Fields:               fields,
		IsPacked:             isPacked,
		ByteSize:             byteSizeNotCalculated,
		ByteAlignment:        byteAlignmentNotCalculated,
	}
}

// Create a new function parameter with a name, parameter type information, and passing mode.
// A function parameter can either be used with a type name or a type descriptor or both. The type name can be empty or the type descriptor can be nil.
func NewFunctionParameter(parameterName, parameterTypeName string, parameterType TypeDescriptor, mode ParameterPassingMode) *FunctionParameter {
	return &FunctionParameter{
		Name:     parameterName,
		TypeName: parameterTypeName,
		Type:     parameterType,
		Mode:     mode,
	}
}

// Create a new function type descriptor with parameters and a return type. The return type may be nil for procedures.
func NewFunctionTypeDescriptor(parameters []*FunctionParameter, returnType TypeDescriptor) TypeDescriptor {
	enforceSpecifiedApplicationBinaryInterface()

	if parameters == nil {
		parameters = make([]*FunctionParameter, 0)
	}

	typeDescriptor := &functionTypeDescriptor{
		commonTypeDescriptor: commonTypeDescriptor{Abi: currentABI},
		Parameters:           parameters,
		ReturnType:           returnType,
	}

	return typeDescriptor
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

// String representation of a data type kind.
func (k DataTypeKind) String() string {
	return dataTypeKindNames[k]
}

// String representation of a parameter passing mode.
func (m ParameterPassingMode) String() string {
	return parameterModeNames[m]
}

// Get the current application binary interface used by the type system.
func GetCurrentApplicationBinaryInterface() plt.ApplicationBinaryInterface {
	return currentABI
}

// The type system requires a current application binary interface to be set.
func SetCurrentApplicationBinaryInterface(abi plt.ApplicationBinaryInterface) {
	currentABI = abi
	enforceSpecifiedApplicationBinaryInterface()
}
