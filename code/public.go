// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package code implements the intermediate code generation compiler phase by traversing the abstract syntax tree.
package code

import (
	"container/list"
	"fmt"
	"io"

	ast "github.com/petersen65/PL0/v2/ast"
	cor "github.com/petersen65/PL0/v2/core"
)

// UnusedDifference states that an intermediate code instruction does not use a block nesting depth difference.
const UnusedDifference = -1

// NoLabel is a constant for an empty label in an intermediate code instruction.
const NoLabel = ""

// Three-address code operations of the intermediate code.
const (
	_ = Operation(iota)

	// x = op y, where op is a unary arithmetic or logical operation and x, y are addresses
	Odd    // unary logical operation 'odd'
	Negate // unary arithmetic operation 'negation'

	// x = y op z, where op is a binary arithmetic or relational operation, x, y, z are addresses
	Plus         // binary arithmetic operation '+'
	Minus        // binary arithmetic operation '-'
	Times        // binary arithmetic operation '*'
	Divide       // binary arithmetic operation '/'
	Equal        // binary relational operation '='
	NotEqual     // binary relational operation '#'
	Less         // binary relational operation '<'
	LessEqual    // binary relational operation '<='
	Greater      // binary relational operation '>'
	GreaterEqual // binary relational operation '>='

	// an unconditional or conditional jump goto L: the instruction with label L is the next to be executed
	Jump             // unconditional jump
	JumpEqual        // conditional jump '='
	JumpNotEqual     // conditional jump '#'
	JumpLess         // conditional jump '<'
	JumpLessEqual    // conditional jump '<='
	JumpGreater      // conditional jump '>'
	JumpGreaterEqual // conditional jump '>='

	// call f, n and y = call f, n for function calls, n is the number of actual parameters in "call f, n"
	Parameter // pass parameter to function
	Call      // call function
	Prelude   // prelude inside the body of a function
	Epilog    // epilog inside the body of a function
	Return    // return from function
	Standard  // call function of the external standard library
	Branch    // branch operation for placing a logical branch instruction in the intermediate code

	Allocate      // allocate memory for a variable in its logical memory space
	ValueCopy     // copy the value of a constant or literal into an address
	VariableLoad  // load variable value from its location in the logical memory space into an address
	VariableStore // store variable value from an address into its location in the logical memory space
)

// Data types supported for an address of the three-address code concept.
const (
	Void = DataType(iota)
	Label
	UnsignedInteger64
	Integer64
)

// Prefixes for address names define what an address in the three-address code concept represents.
const (
	_ = PrefixType(iota)
	BranchPrefix
	LabelPrefix
	ResultPrefix
	ConstantPrefix
	VariablePrefix
	FunctionPrefix
)

// External standard functions provided for the programming language.
const (
	_ = StandardFunction(iota)
	ReadLn
	WriteLn
)

type (
	// The data type of an address in the three-address code concept.
	DataType int

	// String representation of a data type.
	DataTypeRepresentation string

	// Enumeration of prefixes used for names of addresses.
	PrefixType int

	// Address is the data structure of an address in the three-address code concept.
	Address struct {
		Name     string   `json:"name"`      // name or value of an address (values need to be converted to a string)
		DataType DataType `json:"data_type"` // data type of the address
		Location uint64   `json:"location"`  // location of an address in the logical memory space
	}

	// Type for three-address code operations.
	Operation int32

	// Enumeration of standard functions that belong to the external standard library.
	StandardFunction int64

	// Instruction represents a single three-address code operation with its required metadata (e.g. label, block nesting depth difference).
	Instruction struct {
		Label            string    `json:"label"`              // branch-label for any branching operation
		DepthDifference  int32     `json:"depth_difference"`   // block nesting depth difference between variable use and variable declaration
		TokenStreamIndex int       `json:"token_stream_index"` // index of the token in the token stream
		Code             Quadruple `json:"code"`               // three-address code operation
	}

	// Quadruple represents a single three-address code operation with its three addresses (arg1, arg2, result).
	Quadruple struct {
		Operation Operation `json:"operation"` // three-address code operation
		Arg1      *Address  `json:"arg_1"`     // first address (argument 1)
		Arg2      *Address  `json:"arg_2"`     // second address (argument 2)
		Result    *Address  `json:"result"`    // third address (result)
	}

	// IntermediateCode is the public interface for the intermediate code generation compiler phase.
	IntermediateCode interface {
		Generate()
		GetModule() Module
		NewInstruction(operatiom Operation, arg1, arg2, result *Address, options ...any) *Instruction
		AppendInstruction(instruction *Instruction) *list.Element
	}

	// Module represents a logical unit of instructions created from one source file so that a program can be linked together from multiple modules.
	Module interface {
		AppendInstruction(instruction *Instruction) *list.Element
		GetIterator() Iterator
		Print(print io.Writer, args ...any) error
		Export(format cor.ExportFormat, print io.Writer) error
	}

	// The iterator interface provides navigation methods for the module's intermediate code instructions.
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

var (
	// Map abstract syntax data types to intermediate code data types (they have separate type systems)
	DataTypeMap = map[ast.DataType]DataType{
		ast.Integer64: Integer64,
	}

	// DataTypeNames maps an address data type to its string representation.
	DataTypeNames = map[DataType]string{
		Void:              "void",
		Label:             "label",
		UnsignedInteger64: "uint64",
		Integer64:         "int64",
	}

	// Prefixes used for names of addresses.
	Prefix = map[PrefixType]rune{
		BranchPrefix:   'b',
		LabelPrefix:    'l',
		ResultPrefix:   't',
		ConstantPrefix: 'c',
		VariablePrefix: 'v',
		FunctionPrefix: 'f',
	}

	// NoAddress represents an unused address in the three-address code concept.
	NoAddress = &Address{DataType: Void, Location: 0, Name: "-"}

	// Map three-address code operations of the intermediate code to their string representation.
	OperationNames = map[Operation]string{
		Odd:              "odd",
		Negate:           "negate",
		Plus:             "add",
		Minus:            "subtract",
		Times:            "multiply",
		Divide:           "divide",
		Equal:            "eq",
		NotEqual:         "neq",
		Less:             "lss",
		LessEqual:        "lssEq",
		Greater:          "gtr",
		GreaterEqual:     "gtrEq",
		Jump:             "jmp",
		JumpEqual:        "jmpEq",
		JumpNotEqual:     "jmpNeq",
		JumpLess:         "jmpLss",
		JumpLessEqual:    "jmpLssEq",
		JumpGreater:      "jmpGtr",
		JumpGreaterEqual: "jmpGtrEq",
		Parameter:        "param",
		Call:             "call",
		Prelude:          "prelude",
		Epilog:           "epilog",
		Return:           "return",
		Standard:         "standard",
		Branch:           "branch",
		Allocate:         "alloc",
		ValueCopy:        "valCopy",
		VariableLoad:     "varLoad",
		VariableStore:    "varStore",
	}
)

// Return the public interface of the private intermediate code implementation.
func NewIntermediateCode(abstractSyntax ast.Block) IntermediateCode {
	return newIntermediateCode(abstractSyntax)
}

// Return the public interface of the private intermediate code module implementation.
func NewModule() Module {
	return newModule()
}

// Create a new three-address code argument or result address.
func NewAddress(name any, dataType DataType, location uint64) *Address {
	return &Address{Name: fmt.Sprintf("%v", name), DataType: dataType, Location: location}
}
