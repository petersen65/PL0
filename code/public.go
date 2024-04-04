// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package code implements the intermediate code generation compiler pass by traversing the abstract syntax tree.
package code

import ast "github.com/petersen65/PL0/v2/ast"

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

	// call p, n and y = call p, n for procedure and function calls, n is the number of actual parameters in "call p, n"
	Parameter     // pass parameter to procedure
	Call          // call procedure or function
	Return        // return from procedure or function
	NullOperation // null operation for placing a label in the intermediate code

	Allocate      // allocate memory for a variable in its logical memory space
	ValueCopy     // copy the value of a constant or literal into a compiler-generated temporary variable
	VariableLoad  // load variable value from its location in the logical memory space into a compiler-generated temporary variable
	VariableStore // store variable value from a compiler-generated temporary variable into its location in the logical memory space
)

type (
	// Address is the representation of an address in the three-address code concept.
	Address struct {
		DataType ast.DataType // data type of the address
		Variable string       // variable name of the address
	}

	// Type for intermediate code operations.
	Operation int32

	// Module represents a logical unit of instructions created from one source file so that a program can be linked together from multiple modules.
	Module []*Instruction

	// Instruction represents a single operation in the intermediate code that has an optional label.
	Instruction struct {
		Label           string    // optional label
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
		AppendInstruction(instruction *Instruction)
	}
)

// Return the public interface of the private intermediate code implementation.
func NewIntermediateCode(abstractSyntax ast.Block) IntermediateCode {
	return newIntermediateCode(abstractSyntax)
}
