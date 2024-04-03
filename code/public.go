// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package code implements the intermediate code generation compiler pass by traversing the abstract syntax tree.
package code

import "github.com/petersen65/PL0/v2/ast"

// IntermediateCode is the public interface for the intermediate code generation compiler pass.
type IntermediateCode interface {
	Generate()
}

// Return the public interface of the private intermediate code implementation.
func NewIntermediateCode(abstractSyntax ast.Block) IntermediateCode {
	return newIntermediateCode(abstractSyntax)
}

// Three-address code operations for the intermediate code.
const (
	_ = Operation(iota)

	// x = op y, where op is a unary arithmetic or logical operation, and x and y are addresses
	Odd    // unary logical operation 'odd'
	Negate // unary arithmetic operation 'negation'

	// x = y op z, where op is a binary arithmetic or relational operation, and x, y, and z are addresses
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

	// an unconditional or conditional jump goto L
	// the three-address instruction with label L is the next to be executed
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

	// each variable represents a location in its logical memory space
	StackAllocate // allocate memory on the block stack
	HeapAllocate  // allocate memory on the program heap

	// copy operations of the form x = y, where x is assigned the value of y
	ValueCopy // value copy operation of the form x = y and x = 5

	// x = y[i] sets x to the value in the location i memory units beyond location y
	// x[i] = y sets the contents of the location i units beyond x to the value of y
	IndexCopy // indexed copy operation of the form x = y[i] and x[i] = y

	// x = &y sets the r-value of x to be the location (l-value) of y
	PointerCopy // address and pointer copy operation of the form x = &y, x = *y, and *x = y
)

type (
	// Type for intermediate code operations.
	Operation int32

	// Module represents a logical unit of instructions created from one source file so that a program can be linked together from multiple modules.
	Module []Instruction

	// Instruction represents a single operation in the intermediate code that has an optional label.
	Instruction struct {
		Label           string    // optional label
		DepthDifference int32     // block nesting depth difference between variable use and variable declaration
		Code            Quadruple // three-address code operation
	}

	// Quadruple represents a single operation in the intermediate code which is based on the three-address code concept.
	// Each address is a space-delimited string-concatenation of
	//   a data-type and
	//     a name of a symbol-table entry,
	//     or a constant,
	//     or a compiler-generate temporary variable.
	Quadruple struct {
		Operation Operation // intermediate code operation
		Arg1      string    // first address (argument 1)
		Arg2      string    // second address (argument 2)
		Result    string    // third address (result)
	}
)
