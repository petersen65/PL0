// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package typesystem provides a type system that is used for data type representation in the abstract syntax tree.
package typesystem

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

	// composite types
	DataTypeArray       // static and dynamic arrays
	DataTypeRecord      // structures with named fields
	DataTypeSet         // set of types
	DataTypeBitSequence // packed bit sequences

	// procedure or function types
	DataTypeFunction // function pointers with signature

	// enumeration and subrange types
	DataTypeEnumeration // user-defined enumerations
	DataTypeSpan        // subrange of existing data types

	// custom types
	DataTypeCustom // type alias or named type

	// template types that can be parameterized
	DataTypeTemplate // types with parameters

	// pointer types
	DataTypePointer // complex pointer hierarchies
)

// Packing rules define how structure based data types are packed.
const (
	NaturalPacking   PackingRule = iota // align each field to its natural alignment
	MicrosoftPacking                    // Microsoft-specific packing rules
)

// Parameter passing modes for procedures and functions.
const (
	CallByValue          ParameterMode = iota // call by value
	CallByReference                           // call by reference
	CallByConstReference                      // call by const reference
	OutputParameter                           // output parameter
)

type (
	// Primitive data types.
	PrimitiveDataType int

	// Kind of data type (e.g., simple, composite, pointer).
	DataTypeKind int

	// A packing rule defines how structure based data types are packed.
	PackingRule int

	// Mode for parameter passing in procedures and functions.
	ParameterMode int

	// Parameter used in template types.
	TemplateParameter struct {
		Name        string           `json:"name"`        // name of the parameter (e.g., T, K, V)
		Constraints []TypeDescriptor `json:"constraints"` // constraints that the parameter must satisfy
	}

	// Array dimension describes one array dimension.
	ArrayDimension struct {
		Lower  int  `json:"lower"`   // lower index bound of the array dimension
		Upper  int  `json:"upper"`   // upper index bound of the array dimension
		IsOpen bool `json:"is_open"` // bounds determined by actual array used or passed at runtime (no storage ownership)
	}

	// Record field describes a field in a record.
	RecordField struct {
		Name       string         `json:"name"`   // name identifier used to access this field within the record
		Type       TypeDescriptor `json:"type"`   // type descriptor defining the data type of this field
		ByteOffset int            `json:"offset"` // byte offset from the beginning of the record structure
	}

	// Parameter and its passing mode for a procedure or function call.
	FunctionParameter struct {
		Name string         `json:"name"` // identifier name used to reference this parameter within the function
		Type TypeDescriptor `json:"type"` // type descriptor defining the data type of this parameter
		Mode ParameterMode  `json:"mode"` // determines how the parameter is passed (e.g., by value, by reference)
	}

	// A single value with a name within an enumeration.
	EnumerationValue struct {
		Name  string `json:"name"`  // identifier name of the enumeration constant
		Value any    `json:"value"` // actual value assigned to this enumeration constant
	}

	// Primitive data types that cannot be further refined.
	SimpleTypeDescriptor struct {
		TypeName      string            `json:"name"`           // human-readable name of the type
		PrimitiveType PrimitiveDataType `json:"primitive_type"` // underlying primitive data type
		ByteSize      int               `json:"size"`           // memory size in bytes required to store this type
		ByteAlignment int               `json:"alignment"`      // memory alignment requirement in bytes for optimal access
	}

	// Array based data types that have bounds and dimensions.
	ArrayTypeDescriptor struct {
		TypeName    string           `json:"name"`         // human-readable name of the array type
		ElementType TypeDescriptor   `json:"element_type"` // type descriptor of the elements stored in the array
		Dimensions  []ArrayDimension `json:"dimensions"`   // supports multidimensional arrays
		IsDynamic   bool             `json:"is_dynamic"`   // can grow or shrink during execution (storage ownership)
	}

	// Structure based data types that group multiple fields.
	StructureTypeDescriptor struct {
		TypeName string        `json:"name"`      // human-readable name of the structure type
		Fields   []RecordField `json:"fields"`    // ordered list of fields that comprise this structure
		IsPacked bool          `json:"is_packed"` // memory layout optimized to minimize padding between fields
	}

	// Set based data types that represent a collection of elements.
	SetTypeDescriptor struct {
		TypeName    string         `json:"name"`         // human-readable name of the set type
		ElementType TypeDescriptor `json:"element_type"` // type descriptor of elements that can be stored in this set
	}

	// Bit based data types that represent a sequence of bits.
	BitSequenceTypeDescriptor struct {
		TypeName string `json:"name"`      // human-readable name of the bit sequence type
		BitCount int    `json:"bit_count"` // number of bits in the sequence
		IsPacked bool   `json:"is_packed"` // true for packed bit sequences
	}

	// Data type of a procedure or function that defines its signature.
	FunctionTypeDescriptor struct {
		TypeName   string              `json:"name"`        // human-readable name of the function type
		Parameters []FunctionParameter `json:"parameters"`  // ordered list of parameters that the function accepts
		ReturnType TypeDescriptor      `json:"return_type"` // type descriptor of the value returned by the function (nil for procedures)
	}

	// Enumeration type for user-defined enumerations.
	EnumerationTypeDescriptor struct {
		TypeName       string             `json:"name"`            // human-readable name of the enumeration type
		Values         []EnumerationValue `json:"values"`          // ordered list of named constants that comprise this enumeration
		UnderlyingType TypeDescriptor     `json:"underlying_type"` // underlying type used to store enumeration values
	}

	// Span type that represents a contiguous range of values as a view on existing data.
	SpanTypeDescriptor struct {
		TypeName       string         `json:"name"`            // human-readable name of the span type
		UnderlyingType TypeDescriptor `json:"underlying_type"` // underlying type that this span references
		Start          int            `json:"start"`           // starting index within the underlying type's value range
		Length         int            `json:"length"`          // number of consecutive values included in this span
	}

	// User-defined data type with a custom name and a custom underlying type.
	CustomTypeDescriptor struct {
		TypeName       string         `json:"name"`            // human-readable name of the custom type
		UnderlyingType TypeDescriptor `json:"underlying_type"` // type descriptor of the underlying implementation
		IsAlias        bool           `json:"is_alias"`        // true for type alias, false for distinct type
	}

	// Template type that can be parameterized with specific arguments.
	TemplateTypeDescriptor struct {
		TypeName       string                    `json:"name"`            // human-readable name of the template type
		Parameters     []TemplateParameter       `json:"type_parameters"` // list of type parameters that can be substituted
		TemplateType   TypeDescriptor            `json:"template_type"`   // template definition
		Instantiations map[string]TypeDescriptor `json:"instantiations"`  // cache for instantiations
	}

	// Pointer type that represents a memory address pointing to a value with a specific data type.
	PointerTypeDescriptor struct {
		TypeName    string         `json:"name"`         // human-readable name of the pointer type
		ValueType   TypeDescriptor `json:"value_type"`   // type descriptor of the value that this pointer references
		IsReference bool           `json:"is_reference"` // true if this is a reference rather than a raw pointer
	}

	// The application binary interface contains size, alignment, and packing rules.
	ApplicationBinaryInterface struct {
		Name              string
		PointerSize       int64
		PointerAlignment  int64
		TypeSizes         map[PrimitiveDataType]int64
		TypeAlignments    map[PrimitiveDataType]int64
		StructPackingRule PackingRule
		MaxAlignment      int64
	}

	// The type descriptor is the common interface for all specific type descriptions.
	TypeDescriptor interface {
		Kind() DataTypeKind
		Name() string
		String() string
		Size() int
		Alignment() int
		IsTemplate() bool
		Parameters() []TemplateParameter
		Instantiate(args []TypeDescriptor) (TypeDescriptor, error)
	}
)

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
