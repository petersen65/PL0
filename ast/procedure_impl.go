// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"fmt"

	sym "github.com/petersen65/pl0/v3/symbol"
)

// Format for the string representation of a procedure declaration node.
const procedureFormat = "declaration(%v,name=%v,used=%v)"

// Create a new procedure declaration node in the abstract syntax tree.
func newProcedureDeclaration(name string, block Block, scope sym.Scope, index int) Declaration {
	return &ProcedureDeclarationNode{
		CommonNode:       CommonNode{NodeKind: KindProcedureDeclaration},
		DeclarationNode:  DeclarationNode{Name: name, DataTypeName: "", Scope: scope, IdentifierUsage: make([]Expression, 0), TokenStreamIndex: index},
		Block:            block,
	}
}

// Children nodes of the procedure declaration node.
func (n *ProcedureDeclarationNode) Children() []Node {
	return []Node{n.Block}
}

// String representation of the procedure declaration node.
func (n *ProcedureDeclarationNode) String() string {
	return fmt.Sprintf(procedureFormat, n.Kind(), n.Name, len(n.IdentifierUsage))
}

// Accept the visitor for the procedure declaration node.
func (n *ProcedureDeclarationNode) Accept(visitor Visitor) {
	visitor.VisitProcedureDeclaration(n)
}
