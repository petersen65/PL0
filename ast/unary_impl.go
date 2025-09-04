// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import eh "github.com/petersen65/pl0/v3/errors"

// Formats for the string representation of unary operation nodes.
var unaryOperationFormats = map[UnaryOperator]string{
	Odd:    "odd",
	Negate: "negate",
}

// The unary operation node represents a unary operation in the abstract syntax tree.
type unaryOperationNode struct {
	expressionNode               // embedded expression node
	UnaryOperation UnaryOperator `json:"operation"` // unary operation
	UnaryOperand   Expression    `json:"operand"`   // operand of the unary operation
}

// Create a new unary operation node in the abstract syntax tree.
func newUnaryOperation(operation UnaryOperator, operand Expression, index int) UnaryOperation {
	unaryNode := &unaryOperationNode{
		expressionNode: expressionNode{
			commonNode:       commonNode{NodeKind: KindUnaryOperation},
			TokenStreamIndex: index,
		},
		UnaryOperation: operation,
		UnaryOperand:   operand,
	}

	operand.SetParent(unaryNode)
	return unaryNode
}

// Children nodes of the unary operation node.
func (n *unaryOperationNode) Children() []Node {
	return []Node{n.UnaryOperand}
}

// String representation of the unary operation node.
func (n *unaryOperationNode) String() string {
	switch n.UnaryOperation {
	case Odd, Negate:
		return unaryOperationFormats[n.UnaryOperation]

	default:
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownUnaryOperation, n.UnaryOperation, nil))
	}
}

// Accept the visitor for the unary operation node.
func (n *unaryOperationNode) Accept(visitor Visitor) {
	visitor.VisitUnaryOperation(n)
}

// Find the current block node that contains this unary operation node.
func (n *unaryOperationNode) CurrentBlock() Block {
	return searchBlock(n, CurrentBlock)
}

// Determine if the unary operation node represents a constant value.
func (n *unaryOperationNode) IsConstant() bool {
	// a unary operation is constant if its operand is constant
	return n.UnaryOperand.IsConstant()
}

// Unary operation of the unary operation node.
func (n *unaryOperationNode) Operation() UnaryOperator {
	return n.UnaryOperation
}

// Operand of the unary operation node.
func (n *unaryOperationNode) Operand() Expression {
	return n.UnaryOperand
}
