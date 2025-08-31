// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"fmt"

	sym "github.com/petersen65/pl0/v3/symbol"
	ts "github.com/petersen65/pl0/v3/typesystem"
)

// Format for the string representation of a constant declaration node.
const constantFormat = "declaration(%v,name=%v,value=%v,type=%v,used=%v)"

// Create a new constant declaration node in the abstract syntax tree.
func newConstantDeclaration(name string, value any, dataType ts.TypeDescriptor, scope sym.Scope[Declaration], index int) Declaration {
	return &ConstantDeclarationNode{
		CommonNode:      CommonNode{NodeKind: KindConstantDeclaration},
		DeclarationNode: DeclarationNode{Name: name, DataType: dataType, Scope: scope, TokenStreamIndex: index},
		Value:           value,
	}
}

// Children nodes of the constant declaration node.
func (d *ConstantDeclarationNode) Children() []Node {
	return make([]Node, 0)
}

// String of the constant declaration node.
func (d *ConstantDeclarationNode) String() string {
	return fmt.Sprintf(constantFormat, sym.ConstantEntry, d.Name, d.Value, d.DataType, len(d.IdentifierUsage))
}

// Accept the visitor for the constant declaration node.
func (d *ConstantDeclarationNode) Accept(visitor Visitor) {
	visitor.VisitConstantDeclaration(d)
}
