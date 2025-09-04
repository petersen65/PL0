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

	// Literal node represents the usage of a literal value in the AST.
	LiteralNode struct {
		expressionNode     // embedded expression node
		Value          any `json:"value"` // literal value
	}

	// UnaryOperation node represents a unary operation in the AST.
	UnaryOperationNode struct {
		expressionNode               // embedded expression node
		Operation      UnaryOperator `json:"operation"` // unary operation
		Operand        Expression    `json:"operand"`   // operand of the unary operation
	}

	// BinaryOperation node represents a binary operation in the AST.
	BinaryOperationNode struct {
		expressionNode                // embedded expression node
		Operation      BinaryOperator `json:"operation"` // binary operation
		Left           Expression     `json:"left"`      // left operand of the binary operation
		Right          Expression     `json:"right"`     // right operand of the binary operation
	}

	// ComparisonOperationNode node represents a comparison operation in the AST.
	ComparisonOperationNode struct {
		expressionNode                    // embedded expression node
		Operation      ComparisonOperator `json:"operation"` // comparison operation
		Left           Expression         `json:"left"`      // left operand of the comparison operation
		Right          Expression         `json:"right"`     // right operand of the comparison operation
	}

	// An expression represented as an abstract syntax tree.
	Expression interface {
		Node
		IsConstant() bool
	}
)

// An empty expression is a 0 literal, should only be used in the context of parser errors, and is free from any side-effect.
func NewEmptyExpression() Expression {
	return newLiteral(int64(0), tok.NoTokenStreamIndex)
}

// NewLiteral creates a new literal node in the abstract syntax tree.
func NewLiteral(value any, index int) Expression {
	return newLiteral(value, index)
}

// NewUnaryOperation creates a new unary operation node in the abstract syntax tree.
func NewUnaryOperation(operation UnaryOperator, operand Expression, index int) Expression {
	return newUnaryOperation(operation, operand, index)
}

// NewBinaryOperation creates a new binary operation node in the abstract syntax tree.
func NewBinaryOperation(operation BinaryOperator, left, right Expression, index int) Expression {
	return newBinaryOperation(operation, left, right, index)
}

// NewComparisonOperation creates a new comparison operation node in the abstract syntax tree.
func NewComparisonOperation(operation ComparisonOperator, left, right Expression, index int) Expression {
	return newComparisonOperation(operation, left, right, index)
}
