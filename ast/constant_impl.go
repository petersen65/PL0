// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import "fmt"

// Format for the string representation of a constant declaration node.
const constantFormat = "declaration(%v,name=%v,value=%v,type=%v,used=%v)"

// The node represents a constant declaration in the abstract syntax tree.
type constantDeclarationNode struct {
	declarationNode     // embedded declaration node
	ConstantValue   any `json:"value"` // value of the constant
}

// Create a new constant declaration node in the abstract syntax tree.
func newConstantDeclaration(identifierName, dataTypeName string, value any, index int) ConstantDeclaration {
	return &constantDeclarationNode{
		declarationNode: declarationNode{
			commonNode:                 commonNode{NodeKind: KindConstantDeclaration},
			Identifier:                 identifierName,
			DataType:                   dataTypeName,
			IdentifierUsage:            make([]Expression, 0),
			TokenStreamIndexIdentifier: index,
		},
		ConstantValue: value,
	}
}

// Children nodes of the constant declaration node.
func (n *constantDeclarationNode) Children() []Node {
	return make([]Node, 0)
}

// String representation of the constant declaration node.
func (n *constantDeclarationNode) String() string {
	return fmt.Sprintf(constantFormat, n.NodeKind, n.Identifier, n.ConstantValue, n.DataType, len(n.IdentifierUsage))
}

// Accept the visitor for the constant declaration node.
func (n *constantDeclarationNode) Accept(visitor Visitor) {
	visitor.VisitConstantDeclaration(n)
}

// Find the current block node that contains this constant declaration node.
func (n *constantDeclarationNode) CurrentBlock() Block {
	return searchBlock(n, CurrentBlock)
}

// Value of the constant in the constant declaration node.
func (n *constantDeclarationNode) Value() any {
	return n.ConstantValue
}
