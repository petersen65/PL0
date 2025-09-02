// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import eh "github.com/petersen65/pl0/v3/errors"

// Formats for the string representation of binary operation nodes.
var binaryOperationFormats = map[BinaryOperator]string{
	Plus:   "addition",
	Minus:  "subtraction",
	Times:  "multiplication",
	Divide: "division",
}

// Create a new binary operation node in the abstract syntax tree.
func newBinaryOperation(operation BinaryOperator, left, right Expression, index int) Expression {
	binaryNode := &BinaryOperationNode{
		commonNode:     commonNode{NodeKind: KindBinaryOperation},
		expressionNode: expressionNode{TokenStreamIndex: index},
		Operation:      operation,
		Left:           left,
		Right:          right,
	}

	left.SetParent(binaryNode)
	right.SetParent(binaryNode)
	return binaryNode
}

// Children nodes of the binary operation node.
func (e *BinaryOperationNode) Children() []Node {
	return []Node{e.Left, e.Right}
}

// String representation of the binary operation node.
func (e *BinaryOperationNode) String() string {
	switch e.Operation {
	case Plus, Minus, Times, Divide:
		return binaryOperationFormats[e.Operation]

	default:
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownBinaryOperation, e.Operation, nil))
	}
}

// Accept the visitor for the binary operation node.
func (e *BinaryOperationNode) Accept(visitor Visitor) {
	visitor.VisitBinaryOperation(e)
}

// Find the current block node that contains this binary operation node.
func (e *BinaryOperationNode) CurrentBlock() Block {
	return searchBlock(e, CurrentBlock)
}
