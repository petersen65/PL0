// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import tok "github.com/petersen65/pl0/v3/token"

// Operators with one operand.
const (
	Odd UnaryOperator = iota
	Negate
)

// Operators with two operands.
const (
	Plus BinaryOperator = iota
	Minus
	Times
	Divide
)

// Operators for comparison.
const (
	Equal ComparisonOperator = iota
	NotEqual
	Less
	LessEqual
	Greater
	GreaterEqual
)

type (
	// Take one operand and perform an operation on it.
	UnaryOperator int

	// Take two operands and perform an operation on them.
	BinaryOperator int

	// Take two operands and perform a comparison on them.
	ComparisonOperator int

	// An unary operation node in the abstract syntax tree.
	UnaryOperation interface {
		Expression
		Operation() UnaryOperator
		Operand() Expression
	}

	// A binary operation node in the abstract syntax tree.
	BinaryOperation interface {
		Expression
		Operation() BinaryOperator
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

// Create a new binary operation node in the abstract syntax tree.
func NewBinaryOperation(operation BinaryOperator, left, right Expression, index int) BinaryOperation {
	return newBinaryOperation(operation, left, right, index)
}

// Create a new comparison operation node in the abstract syntax tree.
func NewComparisonOperation(operation ComparisonOperator, left, right Expression, index int) ComparisonOperation {
	return newComparisonOperation(operation, left, right, index)
}
