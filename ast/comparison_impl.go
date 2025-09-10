// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	eh "github.com/petersen65/pl0/v3/errors"
	ts "github.com/petersen65/pl0/v3/typesystem"
)

// The comparison operation node represents a comparison operation in the abstract syntax tree.
type comparisonOperationNode struct {
	expressionNode                       // embedded expression node
	Requirements_  ts.DataTypeCapability `json:"comparison_requirements"` // required data type capabilities
	DataType_      ts.TypeDescriptor     `json:"data_type"`               // data type of the comparison operation (always boolean if operands are valid)
	Operation_     ComparisonOperator    `json:"comparison_operation"`    // comparison operation
	Left_          Expression            `json:"left_operand"`            // left operand of the comparison operation
	Right_         Expression            `json:"right_operand"`           // right operand of the comparison operation
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
		DataType_:  left.CurrentBlock().BuiltInDataType(ts.Boolean.String()),
		Operation_: operation,
		Left_:      left,
		Right_:     right,
	}

	// the parent of the left and right operand is the comparison operation node
	left.SetParent(comparisonNode)
	right.SetParent(comparisonNode)

	// define data type requirements for the comparison operation
	switch operation {
	case Equal, NotEqual:
		// equality operations require equality-comparable data types
		comparisonNode.Requirements_ = ts.Equality

	case Less, LessEqual, Greater, GreaterEqual:
		// ordering operations require orderable data types
		comparisonNode.Requirements_ = ts.Ordered

	default:
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownComparisonOperation, nil, operation))
	}

	return comparisonNode
}

// Children nodes of the comparison operation node.
func (e *comparisonOperationNode) Children() []Node {
	return []Node{e.Left_, e.Right_}
}

// String representation of the comparison operation node.
func (e *comparisonOperationNode) String() string {
	switch e.Operation_ {
	case Equal, NotEqual, Less, LessEqual, Greater, GreaterEqual:
		return comparisonOperationFormats[e.Operation_]

	default:
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownComparisonOperation, nil, e.Operation_))

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
	return e.Left_.IsConstant() && e.Right_.IsConstant()
}

// Determine the data type of the comparison operation node which is always boolean.
// If the data type of the operands cannot be determined, nil is returned.
// If the data types of the left and right operands do not match, nil is returned.
func (n *comparisonOperationNode) DataType() ts.TypeDescriptor {
	left := n.Left_.DataType()
	right := n.Right_.DataType()

	// if either operand's data type cannot be determined, return nil
	if left == nil || right == nil {
		return nil
	}

	// if the data types of the left and right operands do not match, return nil
	if !left.Equal(right) {
		return nil
	}

	// take the data type of the left operand as the data type of the comparison operation
	return left
}

// Comparison operation of the comparison operation node.
func (n *comparisonOperationNode) Operation() ComparisonOperator {
	return n.Operation_
}

// Left operand of the comparison operation node.
func (n *comparisonOperationNode) Left() Expression {
	return n.Left_
}

// Right operand of the comparison operation node.
func (n *comparisonOperationNode) Right() Expression {
	return n.Right_
}
