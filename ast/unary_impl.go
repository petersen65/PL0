// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	eh "github.com/petersen65/pl0/v3/errors"
	ts "github.com/petersen65/pl0/v3/typesystem"
)

// Formats for the string representation of unary operation nodes.
var unaryOperationFormats = map[UnaryOperator]string{
	Odd:    "odd",
	Negate: "negate",
}

// The unary operation node represents a unary operation in the abstract syntax tree.
type unaryOperationNode struct {
	expressionNode                       // embedded expression node
	Requirements_  ts.DataTypeCapability `json:"unary_requirements"` // required data type capabilities
	Operation_     UnaryOperator         `json:"unary_operation"`    // unary operation
	Operand_       Expression            `json:"unary_operand"`      // operand of the unary operation
}

// Create a new unary operation node in the abstract syntax tree.
func newUnaryOperation(operation UnaryOperator, operand Expression, index int) UnaryOperation {
	unaryNode := &unaryOperationNode{
		expressionNode: expressionNode{
			commonNode:       commonNode{NodeKind: KindUnaryOperation},
			TokenStreamIndex: index,
		},
		Operation_: operation,
		Operand_:   operand,
	}

	// the parent of the operand is the unary operation node
	operand.SetParent(unaryNode)

	// define data type requirements for the unary operation
	switch operation {
	case Odd:
		// the odd operation requires an odd-checkable data type
		unaryNode.Requirements_ = ts.OddCheckable

	case Negate:
		// the negate operation requires a negatable data type
		unaryNode.Requirements_ = ts.Negatable

	default:
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownUnaryOperation, nil, operation))
	}

	return unaryNode
}

// Children nodes of the unary operation node.
func (n *unaryOperationNode) Children() []Node {
	return []Node{n.Operand_}
}

// String representation of the unary operation node.
func (n *unaryOperationNode) String() string {
	switch n.Operation_ {
	case Odd, Negate:
		return unaryOperationFormats[n.Operation_]

	default:
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownUnaryOperation, nil, n.Operation_))
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
	return n.Operand_.IsConstant()
}

// Determine the data type of the unary operation node. If the data type of the operand cannot be determined, nil is returned.
func (n *unaryOperationNode) DataType() ts.TypeDescriptor {
	return n.Operand_.DataType()
}

// Unary operation of the unary operation node.
func (n *unaryOperationNode) Operation() UnaryOperator {
	return n.Operation_
}

// Operand of the unary operation node.
func (n *unaryOperationNode) Operand() Expression {
	return n.Operand_
}
