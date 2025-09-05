// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import "fmt"

// Format for the string representation of a variable declaration node.
const variableFormat = "declaration(%v,name=%v,type=%v,used=%v)"

// The node represents a variable declaration in the abstract syntax tree.
type variableDeclarationNode struct {
	declarationNode // embedded declaration node
}

// Create a new variable declaration node in the abstract syntax tree.
func newVariableDeclaration(identifierName, dataTypeName string, identifierIndex, dataTypeIndex int) VariableDeclaration {
	return &variableDeclarationNode{
		declarationNode: declarationNode{
			commonNode:                 commonNode{NodeKind: KindVariableDeclaration},
			Identifier:                 identifierName,
			DataType:                   dataTypeName,
			IdentifierUsage:            make([]Expression, 0),
			TokenStreamIndexIdentifier: identifierIndex,
			TokenStreamIndexDataType:   dataTypeIndex,
		},
	}
}

// Children nodes of the variable declaration node.
func (n *variableDeclarationNode) Children() []Node {
	return make([]Node, 0)
}

// String representation of the variable declaration node.
func (n *variableDeclarationNode) String() string {
	return fmt.Sprintf(variableFormat, n.NodeKind, n.Identifier, n.DataType, len(n.IdentifierUsage))
}

// Accept the visitor for the variable declaration node.
func (n *variableDeclarationNode) Accept(visitor Visitor) {
	visitor.VisitVariableDeclaration(n)
}

// Find the current block node that contains this variable declaration node.
func (n *variableDeclarationNode) CurrentBlock() Block {
	return searchBlock(n, CurrentBlock)
}

// Block nesting depth of the variable declaration.
func (n *variableDeclarationNode) Depth() int {
	return n.CurrentBlock().Depth()
}
