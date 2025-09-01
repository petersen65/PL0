// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"fmt"

	sym "github.com/petersen65/pl0/v3/symbol"
)

// Format for the string representation of a constant declaration node.
const constantFormat = "declaration(%v,name=%v,value=%v,type=%v,used=%v)"

// Create a new constant declaration node in the abstract syntax tree.
func newConstantDeclaration(name, dataTypeName string, value any, scope sym.Scope[Declaration], index int) Declaration {
	return &ConstantDeclarationNode{
		CommonNode:      CommonNode{NodeKind: KindConstantDeclaration},
		DeclarationNode: DeclarationNode{Name: name, DataTypeName: dataTypeName, Scope: scope, IdentifierUsage: make([]Expression, 0), TokenStreamIndex: index},
		Value:           value,
	}
}

// Children nodes of the constant declaration node.
func (d *ConstantDeclarationNode) Children() []Node {
	return make([]Node, 0)
}

// String of the constant declaration node.
func (d *ConstantDeclarationNode) String() string {
	return fmt.Sprintf(constantFormat, sym.ConstantEntry, d.Name, d.Value, d.DataTypeName, len(d.IdentifierUsage))
}

// Accept the visitor for the constant declaration node.
func (d *ConstantDeclarationNode) Accept(visitor Visitor) {
	visitor.VisitConstantDeclaration(d)
}
