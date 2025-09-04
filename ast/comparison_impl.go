// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import eh "github.com/petersen65/pl0/v3/errors"

// Formats for the string representation of comparison operation nodes.
var comparisonOperationFormats = map[ComparisonOperator]string{
	Equal:        "equal",
	NotEqual:     "not equal",
	Less:         "less",
	LessEqual:    "less equal",
	Greater:      "greater",
	GreaterEqual: "greater equal",
}

// Create a new comparison operation node in the abstract syntax tree.
func newComparisonOperation(operation ComparisonOperator, left, right Expression, index int) Expression {
	comparisonNode := &ComparisonOperationNode{
		expressionNode: expressionNode{
			commonNode:       commonNode{NodeKind: KindComparisonOperation},
			TokenStreamIndex: index,
		},
		Operation: operation,
		Left:      left,
		Right:     right,
	}

	left.SetParent(comparisonNode)
	right.SetParent(comparisonNode)
	return comparisonNode
}

// Children nodes of the comparison operation node.
func (e *ComparisonOperationNode) Children() []Node {
	return []Node{e.Left, e.Right}
}

// String representation of the comparison operation node.
func (e *ComparisonOperationNode) String() string {
	switch e.Operation {
	case Equal, NotEqual, Less, LessEqual, Greater, GreaterEqual:
		return comparisonOperationFormats[e.Operation]

	default:
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownComparisonOperation, e.Operation, nil))

	}
}

// Accept the visitor for the comparison operation node.
func (e *ComparisonOperationNode) Accept(visitor Visitor) {
	visitor.VisitComparisonOperation(e)
}

// Find the current block node that contains this comparison operation node.
func (e *ComparisonOperationNode) CurrentBlock() Block {
	return searchBlock(e, CurrentBlock)
}

// Determine if the comparison operation node represents a constant value.
func (e *ComparisonOperationNode) IsConstant() bool {
	// a comparison operation is constant if both its operands are constant
	return e.Left.IsConstant() && e.Right.IsConstant()
}
