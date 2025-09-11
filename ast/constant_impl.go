// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import "fmt"

// Format for the string representation of a constant declaration node.
const constantFormat = "declaration(%v,name=%v,value=%v,type=%v,used=%v)"

// Format for the string representation of a constant declaration node with missing value and type.
const constantErrorFormat = "declaration(%v,name=%v,type=%v,used=%v)"

// The node represents a constant declaration in the abstract syntax tree.
type constantDeclarationNode struct {
	declarationNode            // embedded declaration node
	Value_          any        `json:"constant_value"` // inferred value of the constant, might be nil if it was not or cannot be inferred
	Expression_     Expression `json:"expression"`     // expression on the right side of the constant identifier
}

// Create a new constant declaration node in the abstract syntax tree.
func newConstantDeclaration(identifierName string, expression Expression, index int) ConstantDeclaration {
	constantNode := &constantDeclarationNode{
		declarationNode: declarationNode{
			commonNode:                 commonNode{NodeKind: KindConstantDeclaration},
			IdentifierName_:            identifierName,
			DataTypeName_:              DataTypeUnknown,
			Usage_:                     make([]Expression, 0),
			TokenStreamIndexIdentifier: index,
		},
		Expression_: expression,
	}

	expression.SetParent(constantNode)
	return constantNode
}

// Children nodes of the constant declaration node.
func (n *constantDeclarationNode) Children() []Node {
	return make([]Node, 0)
}

// String representation of the constant declaration node.
func (n *constantDeclarationNode) String() string {
	if n.Value_ == nil {
		return fmt.Sprintf(constantErrorFormat, n.NodeKind, n.IdentifierName_, n.DataTypeName_, len(n.Usage_))
	}

	return fmt.Sprintf(constantFormat, n.NodeKind, n.IdentifierName_, n.Value_, n.DataTypeName_, len(n.Usage_))
}

// Accept the visitor for the constant declaration node.
func (n *constantDeclarationNode) Accept(visitor Visitor) {
	visitor.VisitConstantDeclaration(n)
}

// Find the current block node that contains this constant declaration node.
func (n *constantDeclarationNode) CurrentBlock() Block {
	return searchBlock(n, CurrentBlock)
}

// Block nesting depth of the constant declaration.
func (n *constantDeclarationNode) Depth() int {
	return n.CurrentBlock().Depth()
}

// Expression on the right side of the constant identifier in the constant declaration node.
func (n *constantDeclarationNode) Expression() Expression {
	return n.Expression_
}

// Value of the constant in the constant declaration node.
func (n *constantDeclarationNode) Value() any {
	return n.Value_
}

// The data type name of the constant declaration will be set by the semantic analyzer after a data type for the constant has been determined.
func (n *constantDeclarationNode) SetDataTypeName(dataTypeName string) {
	n.DataTypeName_ = dataTypeName
}
