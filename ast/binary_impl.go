// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import eh "github.com/petersen65/pl0/v3/errors"

// The binary operation node represents a binary operation in the abstract syntax tree.
type binaryOperationNode struct {
	expressionNode                 // embedded expression node
	BinaryOperation BinaryOperator `json:"operation"` // binary operation
	BinaryLeft      Expression     `json:"left"`      // left operand of the binary operation
	BinaryRight     Expression     `json:"right"`     // right operand of the binary operation
}

// Formats for the string representation of binary operation nodes.
var binaryOperationFormats = map[BinaryOperator]string{
	Plus:   "addition",
	Minus:  "subtraction",
	Times:  "multiplication",
	Divide: "division",
}

// Create a new binary operation node in the abstract syntax tree.
func newBinaryOperation(operation BinaryOperator, left, right Expression, index int) BinaryOperation {
	binaryNode := &binaryOperationNode{
		expressionNode: expressionNode{
			commonNode:       commonNode{NodeKind: KindBinaryOperation},
			TokenStreamIndex: index,
		},
		BinaryOperation: operation,
		BinaryLeft:      left,
		BinaryRight:     right,
	}

	left.SetParent(binaryNode)
	right.SetParent(binaryNode)
	return binaryNode
}

// Children nodes of the binary operation node.
func (e *binaryOperationNode) Children() []Node {
	return []Node{e.BinaryLeft, e.BinaryRight}
}

// String representation of the binary operation node.
func (e *binaryOperationNode) String() string {
	switch e.BinaryOperation {
	case Plus, Minus, Times, Divide:
		return binaryOperationFormats[e.BinaryOperation]

	default:
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownBinaryOperation, e.BinaryOperation, nil))
	}
}

// Accept the visitor for the binary operation node.
func (e *binaryOperationNode) Accept(visitor Visitor) {
	visitor.VisitBinaryOperation(e)
}

// Find the current block node that contains this binary operation node.
func (e *binaryOperationNode) CurrentBlock() Block {
	return searchBlock(e, CurrentBlock)
}

// Determine if the binary operation node represents a constant value.
func (e *binaryOperationNode) IsConstant() bool {
	// a binary operation is constant if both its operands are constant
	return e.BinaryLeft.IsConstant() && e.BinaryRight.IsConstant()
}

// Binary operation of the binary operation node.
func (n *binaryOperationNode) Operation() BinaryOperator {
	return n.BinaryOperation
}

// Left operand of the binary operation node.
func (n *binaryOperationNode) Left() Expression {
	return n.BinaryLeft
}

// Right operand of the binary operation node.
func (n *binaryOperationNode) Right() Expression {
	return n.BinaryRight
}
