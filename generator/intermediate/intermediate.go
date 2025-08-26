// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package intermediate provides the intermediate code language which is based on the three-address code concept.
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
	Setup    // setup function call by initializing the memory space and internal data structures

	// memory management and handling of variables or literals
	AllocateVariable // allocate memory for a variable in its memory space
	CopyLiteral      // copy the value of a literal into an address
	LoadVariable     // load variable value from its memory space into an address
	StoreVariable    // store variable value from an address into its memory space
)

// Three-address code address variants of the intermediate code.
const (
	Empty     Variant = iota // the address does not have a variant and does not hold any value
	Temporary                // temporary address holds the result from an expression and has a name and a data type
	Literal                  // literal address holds a literal value and its data type
	Variable                 // variable address holds a variable name and its data type
)

// Data type of an address in the three-address code concept.
// The first 8 bits (0-7) are used for the plain data type, the next bits (8+) are used for modifiers.
const (
	// note: the order of the data types is important, do not change it without updating the code of the '(dataType DataType) Is*' methods
	Untyped    DataType = iota // the address does not have a data type
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

// Data type bit flags for pointer and reference modifiers (bits 8+).
const (
	Pointer   DataType = 1 << 8 // bit 8: pointer type (ptr T)
	Reference DataType = 1 << 9 // bit 9: reference type (ref T)
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

	// The data type of an address in the three-address code concept.
	DataType int

	// Kind of symbol entries with flattened names in the intermediate code.
	Entry int

	// Address is the data structure for an argument or a result in the three-address code concept.
	Address struct {
		Variant  Variant  `json:"variant"`   // variant of what the address represents
		DataType DataType `json:"data_type"` // data type of the address
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
		Arg1   Variant `json:"arg_1"`  // first address variant
		Arg2   Variant `json:"arg_2"`  // second address variant
		Result Variant `json:"result"` // third address variant
	}

	// Instruction represents a single three-address code operation with its required metadata (e.g. token stream index).
	Instruction struct {
		Quadruple        *Quadruple `json:"quadruple"`          // three-address code operation with its three addresses
		TokenStreamIndex int        `json:"token_stream_index"` // index of the token in the token stream
	}

	// A symbol is a data structure that stores all necessary information related to a declared identifier in the intermediate code.
	// A declared identifier in the intermediate code has a flattened name and was derived from an identifier in the abstract syntax tree.
	Symbol struct {
		Name       string        `json:"name"`      // flattened name in the intermediate code
		Kind       Entry         `json:"kind"`      // kind of symbol entry
		DataType   DataType      `json:"data_type"` // data type of the symbol
		Definition *list.Element `json:"-"`         // instruction where the symbol is defined
	}

	// A symbol table is a collection of symbols that can be used to look up names and their associated information.
	SymbolTable interface {
		Insert(symbol *Symbol)
		Lookup(name string) *Symbol
	}

	// IntermediateCodeUnit represents a logical unit of instructions created from one source file.
	// It manages a list of instructions and a symbol table with all declared identifiers in this unit of the intermediate code.
	IntermediateCodeUnit interface {
		SymbolTable
		AppendInstruction(operation Operation, arg1, arg2, result *Address, tokenStreamIndex int) *list.Element
		AppendExistingInstruction(instruction *Instruction) *list.Element
		GetIterator() Iterator
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

// String representation of an three-address code operation.
func (o Operation) String() string {
	return operationNames[o]
}

// String representation of an intermediate code instruction.
func (i *Instruction) String() string {
	const tokenStreamIndexWidth = 6
	return fmt.Sprintf("%*v    %v", tokenStreamIndexWidth, i.TokenStreamIndex, i.Quadruple)
}

// Return the plain data type without modifiers.
func (dt DataType) AsPlain() DataType {
	return dt & 0xFF
}

// Check whether the data type is a plain data type without modifiers.
func (dt DataType) IsPlain() bool {
	return dt == dt.AsPlain()
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

// Check whether the data type is untyped.
func (dt DataType) IsUntyped() bool {
	return dt.AsPlain() == Untyped
}

// Supported data types for symbol entries, temporaries, literals, and variables.
func (dt DataType) IsSupported() bool {
	return dt.AsPlain() >= Integer64 && dt.AsPlain() <= String
}

// Check whether the data type has a signed representation.
func (dt DataType) IsSigned() bool {
	return dt.AsPlain() >= Integer64 && dt.AsPlain() <= Float32
}

// Check whether the data type has an unsigned representation.
func (dt DataType) IsUnsigned() bool {
	return dt.AsPlain() >= Unsigned64 && dt.AsPlain() <= Boolean
}

// Check whether the data type is a signed integer.
func (dt DataType) IsSignedInteger() bool {
	return dt.AsPlain() >= Integer64 && dt.AsPlain() <= Integer8
}

// Check whether the data type is an unsigned integer.
func (dt DataType) IsUnsignedInteger() bool {
	return dt.AsPlain() >= Unsigned64 && dt.AsPlain() <= Unsigned8
}

// Check whether the data type is an integer.
func (dt DataType) IsInteger() bool {
	return dt.IsSignedInteger() || dt.IsUnsignedInteger()
}

// Check whether the data type is a floating point number.
func (dt DataType) IsFloatingPoint() bool {
	return dt.AsPlain() == Float64 || dt == Float32
}

// Check whether the data type is a boolean.
func (dt DataType) IsBoolean() bool {
	return dt.AsPlain() == Boolean
}

// Check whether the data type is a character.
func (dt DataType) IsCharacter() bool {
	return dt.AsPlain() == Character
}

// Check whether the data type is a string.
func (dt DataType) IsString() bool {
	return dt.AsPlain() == String
}
