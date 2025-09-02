// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import "fmt"

// Format for the string representation of a variable declaration node.
const variableFormat = "declaration(%v,name=%v,type=%v,used=%v)"

// Create a new variable declaration node in the abstract syntax tree.
func newVariableDeclaration(name, dataTypeName string, index int) Declaration {
	return &VariableDeclarationNode{
		commonNode:      commonNode{NodeKind: KindVariableDeclaration},
		declarationNode: declarationNode{Name: name, DataTypeName: dataTypeName, IdentifierUsage: make([]Expression, 0), TokenStreamIndex: index},
	}
}

// Children nodes of the variable declaration node.
func (n *VariableDeclarationNode) Children() []Node {
	return make([]Node, 0)
}

// String representation of the variable declaration node.
func (n *VariableDeclarationNode) String() string {
	return fmt.Sprintf(variableFormat, n.Kind(), n.Name, n.DataTypeName, len(n.IdentifierUsage))
}

// Accept the visitor for the variable declaration node.
func (n *VariableDeclarationNode) Accept(visitor Visitor) {
	visitor.VisitVariableDeclaration(n)
}

// Find the current block node that contains this variable declaration node.
func (n *VariableDeclarationNode) CurrentBlock() Block {
	return searchBlock(n, CurrentBlock)
}
