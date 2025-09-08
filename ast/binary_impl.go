// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	eh "github.com/petersen65/pl0/v3/errors"
	ts "github.com/petersen65/pl0/v3/typesystem"
)

// The binary operation node represents a binary operation in the abstract syntax tree.
type binaryOperationNode struct {
	expressionNode                // embedded expression node
	Operation_     BinaryOperator `json:"binary_operation"` // binary operation
	Left_          Expression     `json:"left_operand"`     // left operand of the binary operation
	Right_         Expression     `json:"right_operand"`    // right operand of the binary operation
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
		Operation_: operation,
		Left_:      left,
		Right_:     right,
	}

	left.SetParent(binaryNode)
	right.SetParent(binaryNode)
	return binaryNode
}

// Children nodes of the binary operation node.
func (e *binaryOperationNode) Children() []Node {
	return []Node{e.Left_, e.Right_}
}

// String representation of the binary operation node.
func (e *binaryOperationNode) String() string {
	switch e.Operation_ {
	case Plus, Minus, Times, Divide:
		return binaryOperationFormats[e.Operation_]

	default:
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownBinaryOperation, e.Operation_, nil))
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
	return e.Left_.IsConstant() && e.Right_.IsConstant()
}

// Determine the data type of the binary operation node.
// If the data types of the left and right operands do not match, nil is returned.
// If the data type of the operands cannot be determined, nil is returned.
func (n *binaryOperationNode) DataType() ts.TypeDescriptor {
	left := n.Left_.DataType()
	right := n.Right_.DataType()

	if left == nil || left != right {
		return nil
	}

	return left
}

// Binary operation of the binary operation node.
func (n *binaryOperationNode) Operation() BinaryOperator {
	return n.Operation_
}

// Left operand of the binary operation node.
func (n *binaryOperationNode) Left() Expression {
	return n.Left_
}

// Right operand of the binary operation node.
func (n *binaryOperationNode) Right() Expression {
	return n.Right_
}
