// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	tok "github.com/petersen65/pl0/v3/token"
	ts "github.com/petersen65/pl0/v3/typesystem"
)

// Operators with one operand.
const (
	Odd UnaryOperator = iota
	Negate
)

// Arithmetic operators with two operands.
const (
	Plus ArithmeticOperator = iota
	Minus
	Times
	Divide
)

// Comparison operators with two operands.
const (
	Equal ComparisonOperator = iota
	NotEqual
	Less
	LessEqual
	Greater
	GreaterEqual
)

// Categories of operators that define their data type requirements.
const (
	Arithmetic OperatorCategory = iota
	Comparison
	Logical
	Bitwise
)

type (
	// Take one operand and perform an operation on it.
	UnaryOperator int

	// Take two operands and perform an arithmetic operation on them.
	ArithmeticOperator int

	// Take two operands and perform a comparison operation on them.
	ComparisonOperator int

	// Each operator belongs to a category, which is used to define their data type requirements.
	OperatorCategory int

	// An unary operation node in the abstract syntax tree.
	UnaryOperation interface {
		Expression
		Operation() UnaryOperator
		Operand() Expression
	}

	// An arithmetic operation node in the abstract syntax tree.
	ArithmeticOperation interface {
		Expression
		Operation() ArithmeticOperator
		Left() Expression
		Right() Expression
	}

	// A comparison operation node in the abstract syntax tree.
	ComparisonOperation interface {
		Expression
		Operation() ComparisonOperator
		Left() Expression
		Right() Expression
	}

	// An expression represented as an abstract syntax tree.
	Expression interface {
		Node
		IsConstant() bool
		DataType() ts.TypeDescriptor
	}
)

// An empty expression is a 0 literal, should only be used in the context of parser errors, and is free from any side-effect.
func NewEmptyExpression() Expression {
	return newLiteralUse(int64(0), NoHint, tok.NoTokenStreamIndex)
}

// Create a new unary operation node in the abstract syntax tree.
func NewUnaryOperation(operation UnaryOperator, operand Expression, index int) UnaryOperation {
	return newUnaryOperation(operation, operand, index)
}

// Create a new arithmetic operation node in the abstract syntax tree.
func NewArithmeticOperation(operation ArithmeticOperator, left, right Expression, index int) ArithmeticOperation {
	return newArithmeticOperation(operation, left, right, index)
}

// Create a new comparison operation node in the abstract syntax tree.
func NewComparisonOperation(operation ComparisonOperator, left, right Expression, index int) ComparisonOperation {
	return newComparisonOperation(operation, left, right, index)
}
