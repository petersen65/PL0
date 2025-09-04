// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import eh "github.com/petersen65/pl0/v3/errors"

// The comparison operation node represents a comparison operation in the abstract syntax tree.
type comparisonOperationNode struct {
	expressionNode                         // embedded expression node
	ComparisonOperation ComparisonOperator `json:"operation"` // comparison operation
	ComparisonLeft      Expression         `json:"left"`      // left operand of the comparison operation
	ComparisonRight     Expression         `json:"right"`     // right operand of the comparison operation
}

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
func newComparisonOperation(operation ComparisonOperator, left, right Expression, index int) ComparisonOperation {
	comparisonNode := &comparisonOperationNode{
		expressionNode: expressionNode{
			commonNode:       commonNode{NodeKind: KindComparisonOperation},
			TokenStreamIndex: index,
		},
		ComparisonOperation: operation,
		ComparisonLeft:      left,
		ComparisonRight:     right,
	}

	left.SetParent(comparisonNode)
	right.SetParent(comparisonNode)
	return comparisonNode
}

// Children nodes of the comparison operation node.
func (e *comparisonOperationNode) Children() []Node {
	return []Node{e.ComparisonLeft, e.ComparisonRight}
}

// String representation of the comparison operation node.
func (e *comparisonOperationNode) String() string {
	switch e.ComparisonOperation {
	case Equal, NotEqual, Less, LessEqual, Greater, GreaterEqual:
		return comparisonOperationFormats[e.ComparisonOperation]

	default:
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownComparisonOperation, e.ComparisonOperation, nil))

	}
}

// Accept the visitor for the comparison operation node.
func (e *comparisonOperationNode) Accept(visitor Visitor) {
	visitor.VisitComparisonOperation(e)
}

// Find the current block node that contains this comparison operation node.
func (e *comparisonOperationNode) CurrentBlock() Block {
	return searchBlock(e, CurrentBlock)
}

// Determine if the comparison operation node represents a constant value.
func (e *comparisonOperationNode) IsConstant() bool {
	// a comparison operation is constant if both its operands are constant
	return e.ComparisonLeft.IsConstant() && e.ComparisonRight.IsConstant()
}

// Comparison operation of the comparison operation node.
func (n *comparisonOperationNode) Operation() ComparisonOperator {
	return n.ComparisonOperation
}

// Left operand of the comparison operation node.
func (n *comparisonOperationNode) Left() Expression {
	return n.ComparisonLeft
}

// Right operand of the comparison operation node.
func (n *comparisonOperationNode) Right() Expression {
	return n.ComparisonRight
}
