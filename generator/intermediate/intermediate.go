// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package intermediate provides the intermediate code language which is based on the three-address code concept.
package intermediate

import (
	"container/list"
	"fmt"

	exp "github.com/petersen65/pl0/v3/export"
	sym "github.com/petersen65/pl0/v3/symbol"
)

// Three-address code operations of the intermediate code.
const (
	_ Operation = iota

	// unary arithmetic or logical operation
	Odd    // unary logical operation 'odd'
	Negate // unary arithmetic operation 'negation'

	// arithmetic operation
	Plus   // arithmetic operation '+'
	Minus  // arithmetic operation '-'
	Times  // arithmetic operation '*'
	Divide // arithmetic operation '/'

	// comparison operation
	Equal        // comparison operation '=='
	NotEqual     // comparison operation '!='
	Less         // comparison operation '<'
	LessEqual    // comparison operation '<='
	Greater      // comparison operation '>'
	GreaterEqual // comparison operation '>='

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

type (
	// Operation represents a three-address code operation in the quadruple.
	Operation int

	// The variant type is used to distinguish between different kinds of addresses in the three-address code concept.
	Variant int

	// Address is the data structure for an argument or a result in the three-address code concept.
	Address struct {
		Variant      Variant `json:"variant"`        // variant of what the address represents
		DataTypeName string  `json:"data_type_name"` // data type of the address
		Name         string  `json:"name"`           // name of an address
		Value        any     `json:"value"`          // value of the address
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

	// A symbol table is a collection of symbols that can be used to look up names and their associated information.
	SymbolTable interface {
		Insert(symbol *sym.Symbol)
		Lookup(name string) *sym.Symbol
	}

	// IntermediateCodeUnit represents a logical unit of instructions created from one source file.
	// It manages a list of instructions and a symbol table with all declared identifiers in this unit of the intermediate code.
	IntermediateCodeUnit interface {
		SymbolTable
		exp.Exporter
		AppendInstruction(operation Operation, arg1, arg2, result *Address, tokenStreamIndex int) *list.Element
		AppendExistingInstruction(instruction *Instruction) *list.Element
		GetIterator() Iterator
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
func NewTemporaryAddress(dataTypeName string, name string) *Address {
	return &Address{Variant: Temporary, DataTypeName: dataTypeName, Name: name}
}

// Create a new three-address code literal address.
func NewLiteralAddress(dataTypeName string, value any) *Address {
	return &Address{Variant: Literal, DataTypeName: dataTypeName, Value: value}
}

// Create a new three-address code variable address.
func NewVariableAddress(dataTypeName string, name string) *Address {
	return &Address{Variant: Variable, DataTypeName: dataTypeName, Name: name}
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
