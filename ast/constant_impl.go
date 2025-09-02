// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import "fmt"

// Format for the string representation of a constant declaration node.
const constantFormat = "declaration(%v,name=%v,value=%v,type=%v,used=%v)"

// Create a new constant declaration node in the abstract syntax tree.
func newConstantDeclaration(name, dataTypeName string, value any, index int) Declaration {
	return &ConstantDeclarationNode{
		commonNode:      commonNode{NodeKind: KindConstantDeclaration},
		declarationNode: declarationNode{Name: name, DataTypeName: dataTypeName, IdentifierUsage: make([]Expression, 0), TokenStreamIndex: index},
		Value:           value,
	}
}

// Children nodes of the constant declaration node.
func (n *ConstantDeclarationNode) Children() []Node {
	return make([]Node, 0)
}

// String representation of the constant declaration node.
func (n *ConstantDeclarationNode) String() string {
	return fmt.Sprintf(constantFormat, n.Kind(), n.Name, n.Value, n.DataTypeName, len(n.IdentifierUsage))
}

// Accept the visitor for the constant declaration node.
func (n *ConstantDeclarationNode) Accept(visitor Visitor) {
	visitor.VisitConstantDeclaration(n)
}

// Find the current block node that contains this constant declaration node.
func (n *ConstantDeclarationNode) CurrentBlock() Block {
	return searchBlock(n, CurrentBlock)
}
