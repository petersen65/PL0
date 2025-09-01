// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	eh "github.com/petersen65/pl0/v3/errors"
	sym "github.com/petersen65/pl0/v3/symbol"
)

// Create a new comparison operation node in the abstract syntax tree.
func newComparisonOperation(scope sym.Scope[Declaration], operation ComparisonOperator, left, right Expression, index int) Expression {
	comparison := &ComparisonOperationNode{
		TypeName:         nodeKindNames[KindComparisonOperation],
		Operation:        operation,
		Left:             left,
		Right:            right,
		TokenStreamIndex: index,
	}

	left.SetParent(comparison)
	right.SetParent(comparison)
	return comparison
}

// Kind of the comparison operation node.
func (e *ComparisonOperationNode) Kind() NodeKind {
	return KindComparisonOperation
}

// Set the parent Node of the comparison operation node.
func (e *ComparisonOperationNode) SetParent(parent Node) {
	e.ParentNode = parent
}

// String of the comparison operation node.
func (e *ComparisonOperationNode) String() string {
	switch e.Operation {
	case Equal:
		return "equal"

	case NotEqual:
		return "not equal"

	case Less:
		return "less"

	case LessEqual:
		return "less equal"

	case Greater:
		return "greater"

	case GreaterEqual:
		return "greater equal"

	default:
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownComparisonOperation, nil, nil))

	}
}

// Parent node of the comparison operation node.
func (e *ComparisonOperationNode) Parent() Node {
	return e.ParentNode
}

// Children nodes of the comparison operation node.
func (e *ComparisonOperationNode) Children() []Node {
	return []Node{e.Left, e.Right}
}

// Index returns the token stream index of the comparison operation node.
func (e *ComparisonOperationNode) Index() int {
	return e.TokenStreamIndex
}

// ExpressionString returns the string representation of the comparison operation expression.
func (e *ComparisonOperationNode) ExpressionString() string {
	return e.String()
}

// Accept the visitor for the comparison operation node.
func (e *ComparisonOperationNode) Accept(visitor Visitor) {
	visitor.VisitComparisonOperation(e)
}
