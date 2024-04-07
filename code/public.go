// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package code implements the intermediate code generation compiler pass by traversing the abstract syntax tree.
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

// Three-address code operations for the intermediate code.
const (
	_ = Operation(iota)

	// x = op y, where op is a unary arithmetic or logical operation and x, y are compiler-generated temporary variables
	Odd    // unary logical operation 'odd'
	Negate // unary arithmetic operation 'negation'

	// x = y op z, where op is a binary arithmetic or relational operation, x, y, z are compiler-generated temporary variables
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

	// an unconditional or conditional jump goto L: the three-address instruction with label L is the next to be executed
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
	Return    // return from function
	Runtime   // call function of the external runtime library
	Target    // target operation for placing a logical branch address in the intermediate code

	Allocate      // allocate memory for a variable in its logical memory space
	ValueCopy     // copy the value of a constant or literal into a compiler-generated temporary variable
	VariableLoad  // load variable value from its location in the logical memory space into a compiler-generated temporary variable
	VariableStore // store variable value from a compiler-generated temporary variable into its location in the logical memory space
)

// Data types of values and variables in the three-address code concept.
const (
	Void = DataType(iota)
	Label
	UnsignedInteger64
	Integer64
)

// External runtime functions provided for the programming language.
const (
	_ = RuntimeFunction(iota)
	ReadLn
	WriteLn
)

// Label types for the intermediate code.
const (
	_ = LabelType(iota)
	BranchLabel
	FunctionLabel
)

type (
	// The data type of an address in the three-address code concept.
	DataType int

	// String representation of a data type.
	DataTypeRepresentation string

	// Type of a label in the intermediate code.
	LabelType int

	// Address is the representation of an address in the three-address code concept.
	Address struct {
		DataType DataType // data type of the address
		Offset   uint64   // variable offset in the logical memory space
		Variable string   // variable name of the address
	}

	// Type for intermediate code operations.
	Operation int32

	// Enumeration of runtime functions that belong to the external runtime library.
	RuntimeFunction uint64

	// Instruction represents a single operation in the intermediate code that has a target and a block nesting depth difference.
	Instruction struct {
		Label           string    // logical target for any branching operation
		DepthDifference int32     // block nesting depth difference between variable use and variable declaration
		Code            Quadruple // three-address code operation
	}

	// Quadruple represents a single operation in the intermediate code which is based on the three-address code concept.
	// Each address is a space-delimited string-concatenation of a data-type and a variable or a compiler-generated temporary variable.
	Quadruple struct {
		Operation Operation // intermediate code operation
		Arg1      *Address  // first address (argument 1)
		Arg2      *Address  // second address (argument 2)
		Result    *Address  // third address (result)
	}

	// IntermediateCode is the public interface for the intermediate code generation compiler pass.
	IntermediateCode interface {
		Generate()
		GetModule() Module
		NewInstruction(operatiom Operation, label string, difference int32, arg1, arg2, result *Address) *Instruction
		AppendInstruction(instruction *Instruction) *list.Element
	}

	// Module represents a logical unit of instructions created from one source file so that a program can be linked together from multiple modules.
	Module interface {
		AppendInstruction(instruction *Instruction) *list.Element
		Print(print io.Writer, args ...any) error
		Export(format cor.ExportFormat, print io.Writer) error
	}
)

var (
	// Map abstract syntax data types to intermediate code data types (they have separate type systems)
	DataTypeMap = map[ast.DataType]DataType{
		ast.Integer64: Integer64,
	}

	// DataTypeNames maps a data type to its string representation.
	DataTypeNames = map[DataType]string{
		Void:              "void",
		Label:             "label",
		UnsignedInteger64: "uint64",
		Integer64:         "int64",
	}

	// Label prefixes for label types in the intermediate code.
	LabelPrefix = map[LabelType]string{
		BranchLabel:   "B",
		FunctionLabel: "F",
	}

	// NoAddress represents an unused address in the three-address code concept.
	NoAddress = &Address{DataType: Void, Offset: 0, Variable: "-"}

	// OperationNames is a map of operation names for the intermediate code.
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
		Return:           "return",
		Runtime:          "runtime",
		Target:           "target",
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
func NewAddress(dataType DataType, offset uint64, variable any) *Address {
	return &Address{DataType: dataType, Offset: offset, Variable: fmt.Sprintf("%v", variable)}
}
