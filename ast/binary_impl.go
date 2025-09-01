// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	eh "github.com/petersen65/pl0/v3/errors"
	sym "github.com/petersen65/pl0/v3/symbol"
)

// Create a new binary operation node in the abstract syntax tree.
func newBinaryOperation(scope sym.Scope[Declaration], operation BinaryOperator, left, right Expression, index int) Expression {
	binary := &BinaryOperationNode{
		TypeName:         nodeKindNames[KindBinaryOperation],
		Operation:        operation,
		Left:             left,
		Right:            right,
		TokenStreamIndex: index,
	}

	left.SetParent(binary)
	right.SetParent(binary)
	return binary
}

// Kind of the binary operation node.
func (e *BinaryOperationNode) Kind() NodeKind {
	return KindBinaryOperation
}

// Set the parent Node of the binary operation node.
func (e *BinaryOperationNode) SetParent(parent Node) {
	e.ParentNode = parent
}

// String of the binary operation node.
func (e *BinaryOperationNode) String() string {
	switch e.Operation {
	case Plus:
		return "addition"

	case Minus:
		return "subtraction"

	case Times:
		return "multiplication"

	case Divide:
		return "division"

	default:
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownBinaryOperation, nil, nil))
	}
}

// Parent node of the binary operation node.
func (e *BinaryOperationNode) Parent() Node {
	return e.ParentNode
}

// Children nodes of the binary operation node.
func (e *BinaryOperationNode) Children() []Node {
	return []Node{e.Left, e.Right}
}

// Index returns the token stream index of the binary operation node.
func (e *BinaryOperationNode) Index() int {
	return e.TokenStreamIndex
}

// ExpressionString returns the string representation of the binary operation expression.
func (e *BinaryOperationNode) ExpressionString() string {
	return e.String()
}

// Accept the visitor for the binary operation node.
func (e *BinaryOperationNode) Accept(visitor Visitor) {
	visitor.VisitBinaryOperation(e)
}

