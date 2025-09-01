// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"fmt"

	sym "github.com/petersen65/pl0/v3/symbol"
)

// Format for the string representation of a variable declaration node.
const variableFormat = "declaration(%v,name=%v,type=%v,used=%v)"

// Create a new variable declaration node in the abstract syntax tree.
func newVariableDeclaration(name, dataTypeName string, scope sym.Scope[Declaration], index int) Declaration {
	return &VariableDeclarationNode{
		CommonNode:       CommonNode{NodeKind: KindVariableDeclaration},
		DeclarationNode:  DeclarationNode{Name: name, DataTypeName: dataTypeName, Scope: scope, IdentifierUsage: make([]Expression, 0), TokenStreamIndex: index},
	}
}

// Children nodes of the variable declaration node.
func (d *VariableDeclarationNode) Children() []Node {
	return make([]Node, 0)
}

// String of the variable declaration node.
func (d *VariableDeclarationNode) String() string {
	return fmt.Sprintf(variableFormat, sym.VariableEntry, d.Name, d.DataTypeName, len(d.IdentifierUsage))
}

// Accept the visitor for the variable declaration node.
func (d *VariableDeclarationNode) Accept(visitor Visitor) {
	visitor.VisitVariableDeclaration(d)
}
