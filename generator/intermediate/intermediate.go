// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package intermediate implements the intermediate code language which is based on the three-address code concept.
package intermediate

import (
	"container/list"
	"fmt"
	"io"

	cor "github.com/petersen65/PL0/v2/core"
)

// Three-address code operations of the intermediate code.
const (
	_ Operation = iota

	// unary arithmetic or logical operation
	Odd    // unary logical operation 'odd'
	Negate // unary arithmetic operation 'negation'

	// binary arithmetic operation
	Plus   // binary arithmetic operation '+'
	Minus  // binary arithmetic operation '-'
	Times  // binary arithmetic operation '*'
	Divide // binary arithmetic operation '/'

	// binary comparison operation
	Equal        // binary comparison operation '=='
	NotEqual     // binary comparison operation '!='
	Less         // binary comparison operation '<'
	LessEqual    // binary comparison operation '<='
	Greater      // binary comparison operation '>'
	GreaterEqual // binary comparison operation '>='

	// unconditional or conditional jump
	Jump             // unconditional jump
	JumpEqual        // conditional jump '=='
	JumpNotEqual     // conditional jump '!='
	JumpLess         // conditional jump '<'
	JumpLessEqual    // conditional jump '<='
	JumpGreater      // conditional jump '>'
	JumpGreaterEqual // conditional jump '>='
	BranchTarget     // target for any code branch operation: conditional, unconditional, or call

	// function call and parameter passing
	Parameter // pass parameter to function
	Call      // call function
	Return    // return from function

	// function entry and exit sequences
	Prologue // function entry sequence inside the body of a function
	Epilogue // function exit sequence inside the body of a function
	Setup    // setup function call by initializing logical memory space and internal data structures

	// memory management and handling of variables or literals
	AllocateVariable // allocate memory for a variable in its logical memory space
	CopyLiteral      // copy the value of a literal into an address
	LoadVariable     // load variable value from the logical memory space into an address
	StoreVariable    // store variable value from an address into the logical memory space
)

// Three-address code address variants of the intermediate code.
const (
	Empty     Variant = iota // the address does not have a variant and does not hold any value
	Temporary                // temporary address holds the result from an expression and has a name and a datatype
	Literal                  // literal address holds a literal value and its datatype
	Variable                 // variable address holds a variable name and its datatype
)

// Datatype of an address in the three-address code concept.
// The first 8 bits (0-7) are used for the plain datatype, the next bits (8+) are used for modifiers.
const (
	// note: the order of the datatypes is important, do not change it without updating the code of the '(dataType DataType) Is*' methods
	Untyped    DataType = iota // the address does not have a datatype
	Integer64                  // signed 64-bit integer
	Integer32                  // signed 32-bit integer
	Integer16                  // signed 16-bit integer
	Integer8                   // signed 8-bit integer
	Float64                    // signed IEEE 754 64-bit floating-point number
	Float32                    // signed IEEE 754 32-bit floating-point number
	Unsigned64                 // unsigned 64-bit integer
	Unsigned32                 // unsigned 32-bit integer
	Unsigned16                 // unsigned 16-bit integer
	Unsigned8                  // unsigned 8-bit integer
	Boolean                    // unsigned 8-bit boolean (0 or 1, false or true)
	Character                  // Unicode code point (signed 32-bit integer, U+0000 ... U+10FFFF)
	String                     // Encoded string (sequence of UTF encoded characters)
)

// Datatype bit flags for pointer and reference modifiers (bits 8+).
const (
	Pointer   DataType = 1 << 8 // bit 8: pointer type (^T)
	Reference DataType = 1 << 9 // bit 9: reference type (&T)
)

// Kind of supported symbol entry in the intermediate code.
const (
	ConstantEntry Entry = iota
	VariableEntry
	FunctionEntry
)

type (
	// Operation represents a three-address code operation in the quadruple.
	Operation int

	// The variant type is used to distinguish between different kinds of addresses in the three-address code concept.
	Variant int

	// The datatype of an address in the three-address code concept.
	DataType int

	// Kind of symbol entries with flattened names in the intermediate code.
	Entry int

	// Address is the data structure for an argument or a result in the three-address code concept.
	Address struct {
		Variant  Variant  `json:"variant"`   // variant of what the address represents
		DataType DataType `json:"data_type"` // datatype of the address
		Name     string   `json:"name"`      // name of an address
		Value    any      `json:"value"`     // value of the address
	}

	// Quadruple represents a single three-address code operation with its three addresses (arg1, arg2, result).
	Quadruple struct {
		Operation Operation `json:"operation"` // three-address code operation
		Arg1      *Address  `json:"arg_1"`     // first address (argument 1)
		Arg2      *Address  `json:"arg_2"`     // second address (argument 2)
		Result    *Address  `json:"result"`    // third address (result)
	}

	// A contract for addresses describes a valid set of address variants for a three-address code operation.
	AddressesContract struct {
		Arg1   Variant // first address variant
		Arg2   Variant // second address variant
		Result Variant // third address variant
	}

	// Instruction represents a single three-address code operation with its required metadata (e.g. token stream index).
	Instruction struct {
		Quadruple        *Quadruple `json:"quadruple"`          // three-address code operation with its three addresses
		TokenStreamIndex int        `json:"token_stream_index"` // index of the token in the token stream
	}

	// A symbol is a data structure that stores all necessary information related to a declared identifier in the intermediate code.
	// A declared identifier in the intermediate code has a flattened name and was derived from an identifier in the abstract syntax tree.
	Symbol struct {
		Name       string        // flattened name in the intermediate code
		Kind       Entry         // kind of symbol entry
		DataType   DataType      // datatype of the symbol
		Definition *list.Element // instruction where the symbol is defined
	}

	// IntermediateCodeUnit represents a logical unit of instructions created from one source file.
	// It manages a list of instructions and a symbol table with all declared identifiers in this unit of the intermediate code.
	IntermediateCodeUnit interface {
		AppendInstruction(operation Operation, arg1, arg2, result *Address, tokenStreamIndex int) *list.Element
		AppendExistingInstruction(instruction *Instruction) *list.Element
		GetIterator() Iterator
		Insert(symbol *Symbol)
		Lookup(name string) *Symbol
		Print(print io.Writer, args ...any) error
		Export(format cor.ExportFormat, print io.Writer) error
	}

	// The iterator interface provides navigation methods for the units's intermediate code instructions.
	Iterator interface {
		Current() *Instruction
		First() *Instruction
		Last() *Instruction
		Next() *Instruction
		Previous() *Instruction
		Skip(offset int) *Instruction
		Peek(offset int) *Instruction
	}
)

// Return the interface of the intermediate code unit implementation.
func NewIntermediateCodeUnit() IntermediateCodeUnit {
	return newIntermediateCodeUnit()
}

// Create a new three-address code instruction with an operation, two arguments, a result, and its token stream index.
func NewInstruction(operation Operation, arg1, arg2, result *Address, tokenStreamIndex int) *Instruction {
	return &Instruction{Quadruple: &Quadruple{Operation: operation, Arg1: arg1, Arg2: arg2, Result: result}, TokenStreamIndex: tokenStreamIndex}
}

// Create a new three-address code temporary address.
func NewTemporaryAddress(dataType DataType, name string) *Address {
	return &Address{Variant: Temporary, DataType: dataType, Name: name}
}

// Create a new three-address code literal address.
func NewLiteralAddress(dataType DataType, value any) *Address {
	return &Address{Variant: Literal, DataType: dataType, Value: value}
}

// Create a new three-address code variable address.
func NewVariableAddress(dataType DataType, name string) *Address {
	return &Address{Variant: Variable, DataType: dataType, Name: name}
}

// Create new symbol for the intermediate code.
func NewSymbol(name string, kind Entry, dataType DataType) *Symbol {
	return &Symbol{Name: name, Kind: kind, DataType: dataType}
}

// String representation of a variant.
func (v Variant) String() string {
	return variantNames[v]
}

// String representation of a datatype with its modifiers.
func (dt DataType) String() string {
	var prefix string

	if dt.IsPointer() {
		prefix = "ptr "
	} else if dt.IsReference() {
		prefix = "ref "
	}

	return fmt.Sprintf("%v%v", prefix, dataTypeNames[dt.AsPlain()])
}

// String representation of the three-address code address.
func (a *Address) String() string {
	const maxWidth = 30
	var representation string

	switch {
	case a.Variant == Empty:
		representation = fmt.Sprintf("%v", a.Variant)

	case len(a.Name) > 0 && a.Value != nil:
		representation = fmt.Sprintf("%v %v %v %v", a.Variant, a.DataType, a.Name, a.Value)

	case len(a.Name) > 0:
		representation = fmt.Sprintf("%v %v %v", a.Variant, a.DataType, a.Name)

	case a.Value != nil:
		representation = fmt.Sprintf("%v %v %v", a.Variant, a.DataType, a.Value)

	default:
		representation = fmt.Sprintf("%v %v", a.Variant, a.DataType)
	}

	if len(representation) > maxWidth {
		representation = representation[:maxWidth]
	}

	return representation
}

// String representation of an three-address code operation.
func (o Operation) String() string {
	return operationNames[o]
}

// String representation of a three-address code quadruple.
func (q *Quadruple) String() string {
	const operationWidth = 20
	const argWidth = 30

	return fmt.Sprintf(
		"%-*v %-*v %-*v %-*v",
		operationWidth, q.Operation,
		argWidth, q.Arg1,
		argWidth, q.Arg2,
		argWidth, q.Result)
}

// String representation of an intermediate code instruction.
func (i *Instruction) String() string {
	const tokenStreamIndexWidth = 6
	return fmt.Sprintf("%*v    %v", tokenStreamIndexWidth, i.TokenStreamIndex, i.Quadruple)
}

// Return the plain datatype without modifiers.
func (dataType DataType) AsPlain() DataType {
	return dataType & 0xFF
}

// Check whether the datatype is a plain datatype without modifiers.
func (dataType DataType) IsPlain() bool {
	return dataType == dataType.AsPlain()
}

// Return the plain datatype with a pointer modifier.
func (dataType DataType) AsPointer() DataType {
	return dataType.AsPlain() | Pointer
}

// Check whether the datatype is a pointer type.
func (dataType DataType) IsPointer() bool {
	return dataType&Pointer != 0
}

// Return the plain datatype with a reference modifier.
func (dataType DataType) AsReference() DataType {
	return dataType.AsPlain() | Reference
}

// Check whether the datatype is a reference type.
func (dataType DataType) IsReference() bool {
	return dataType&Reference != 0
}

// Check whether the datatype is untyped.
func (dataType DataType) IsUntyped() bool {
	return dataType.AsPlain() == Untyped
}

// Supported data types for symbol entries, temporaries, literals, and variables.
func (dataType DataType) IsSupported() bool {
	return dataType.AsPlain() >= Integer64 && dataType.AsPlain() <= String
}

// Check whether the datatype has a signed representation.
func (dataType DataType) IsSigned() bool {
	return dataType.AsPlain() >= Integer64 && dataType.AsPlain() <= Float32
}

// Check whether the datatype has an unsigned representation.
func (dataType DataType) IsUnsigned() bool {
	return dataType.AsPlain() >= Unsigned64 && dataType.AsPlain() <= Boolean
}

// Check whether the datatype is a signed integer.
func (dataType DataType) IsSignedInteger() bool {
	return dataType.AsPlain() >= Integer64 && dataType.AsPlain() <= Integer8
}

// Check whether the datatype is an unsigned integer.
func (dataType DataType) IsUnsignedInteger() bool {
	return dataType.AsPlain() >= Unsigned64 && dataType.AsPlain() <= Unsigned8
}

// Check whether the datatype is an integer.
func (dataType DataType) IsInteger() bool {
	return dataType.IsSignedInteger() || dataType.IsUnsignedInteger()
}

// Check whether the datatype is a floating point number.
func (dataType DataType) IsFloatingPoint() bool {
	return dataType.AsPlain() == Float64 || dataType == Float32
}

// Check whether the datatype is a boolean.
func (dataType DataType) IsBoolean() bool {
	return dataType.AsPlain() == Boolean
}

// Check whether the datatype is a character.
func (dataType DataType) IsCharacter() bool {
	return dataType.AsPlain() == Character
}

// Check whether the datatype is a string.
func (dataType DataType) IsString() bool {
	return dataType.AsPlain() == String
}
