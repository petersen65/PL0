// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	eh "github.com/petersen65/pl0/v3/errors"
	sym "github.com/petersen65/pl0/v3/symbol"
)

// Formats for the string representation of unary operation nodes.
var unaryOperationFormats = map[UnaryOperator]string{
	Odd:    "odd",
	Negate: "negate",
}

// Create a new unary operation node in the abstract syntax tree.
func newUnaryOperation(scope sym.Scope, operation UnaryOperator, operand Expression, index int) Expression {
	unaryNode := &UnaryOperationNode{
		CommonNode:     CommonNode{NodeKind: KindUnaryOperation},
		ExpressionNode: ExpressionNode{Scope: scope, TokenStreamIndex: index},
		Operation:      operation,
		Operand:        operand,
	}

	operand.SetParent(unaryNode)
	return unaryNode
}

// Children nodes of the unary operation node.
func (n *UnaryOperationNode) Children() []Node {
	return []Node{n.Operand}
}

// String representation of the unary operation node.
func (n *UnaryOperationNode) String() string {
	switch n.Operation {
	case Odd, Negate:
		return unaryOperationFormats[n.Operation]

	default:
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownUnaryOperation, nil, nil))
	}
}

// Accept the visitor for the unary operation node.
func (n *UnaryOperationNode) Accept(visitor Visitor) {
	visitor.VisitUnaryOperation(n)
}
