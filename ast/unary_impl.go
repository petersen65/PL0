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
func newUnaryOperation(scope sym.Scope[Declaration], operation UnaryOperator, operand Expression, index int) Expression {
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
func (e *UnaryOperationNode) Children() []Node {
	return []Node{e.Operand}
}

// String of the unary operation node.
func (e *UnaryOperationNode) String() string {
	switch e.Operation {
	case Odd, Negate:
		return unaryOperationFormats[e.Operation]

	default:
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownUnaryOperation, nil, nil))
	}
}

// Accept the visitor for the unary operation node.
func (e *UnaryOperationNode) Accept(visitor Visitor) {
	visitor.VisitUnaryOperation(e)
}
