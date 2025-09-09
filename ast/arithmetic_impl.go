// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	eh "github.com/petersen65/pl0/v3/errors"
	ts "github.com/petersen65/pl0/v3/typesystem"
)

// The arithmetic operation node represents a arithmetic operation in the abstract syntax tree.
type arithmeticOperationNode struct {
	expressionNode                       // embedded expression node
	Requirements_  ts.DataTypeCapability `json:"arithmetic_requirements"` // required data type capabilities
	Operation_     ArithmeticOperator    `json:"arithmetic_operation"`    // arithmetic operation
	Left_          Expression            `json:"left_operand"`            // left operand of the arithmetic operation
	Right_         Expression            `json:"right_operand"`           // right operand of the arithmetic operation
}

// Formats for the string representation of arithmetic operation nodes.
var arithmeticOperationFormats = map[ArithmeticOperator]string{
	Plus:   "addition",
	Minus:  "subtraction",
	Times:  "multiplication",
	Divide: "division",
}

// Create a new arithmetic operation node in the abstract syntax tree.
func newArithmeticOperation(operation ArithmeticOperator, left, right Expression, index int) ArithmeticOperation {
	binaryNode := &arithmeticOperationNode{
		expressionNode: expressionNode{
			commonNode:       commonNode{NodeKind: KindArithmeticOperation},
			TokenStreamIndex: index,
		},
		Operation_: operation,
		Left_:      left,
		Right_:     right,
	}

	// the parent of the left and right operand is the arithmetic operation node
	left.SetParent(binaryNode)
	right.SetParent(binaryNode)

	// define data type requirements for the arithmetic operation
	switch operation {
	case Plus, Minus, Times, Divide:
		// all arithmetic operations require numeric data types
		binaryNode.Requirements_ = ts.Numeric

	default:
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownArithmeticOperation, operation, nil))
	}

	return binaryNode
}

// Children nodes of the arithmetic operation node.
func (e *arithmeticOperationNode) Children() []Node {
	return []Node{e.Left_, e.Right_}
}

// String representation of the arithmetic operation node.
func (e *arithmeticOperationNode) String() string {
	switch e.Operation_ {
	case Plus, Minus, Times, Divide:
		return arithmeticOperationFormats[e.Operation_]

	default:
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownArithmeticOperation, e.Operation_, nil))
	}
}

// Accept the visitor for the arithmetic operation node.
func (e *arithmeticOperationNode) Accept(visitor Visitor) {
	visitor.VisitArithmeticOperation(e)
}

// Find the current block node that contains this arithmetic operation node.
func (e *arithmeticOperationNode) CurrentBlock() Block {
	return searchBlock(e, CurrentBlock)
}

// Determine if the arithmetic operation node represents a constant value.
func (e *arithmeticOperationNode) IsConstant() bool {
	// an arithmetic operation is constant if both its operands are constant
	return e.Left_.IsConstant() && e.Right_.IsConstant()
}

// Determine the data type of the arithmetic operation node.
// If the data type of the operands cannot be determined, nil is returned.
// If the data types of the left and right operands do not match, nil is returned.
func (n *arithmeticOperationNode) DataType() ts.TypeDescriptor {
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

	// take the data type of the left operand as the data type of the arithmetic operation
	return left
}

// Binary operation of the arithmetic operation node.
func (n *arithmeticOperationNode) Operation() ArithmeticOperator {
	return n.Operation_
}

// Left operand of the arithmetic operation node.
func (n *arithmeticOperationNode) Left() Expression {
	return n.Left_
}

// Right operand of the arithmetic operation node.
func (n *arithmeticOperationNode) Right() Expression {
	return n.Right_
}
