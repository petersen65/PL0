// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import "fmt"

// Format for the string representation of a procedure declaration node.
const procedureFormat = "declaration(%v,name=%v,used=%v)"

// Create a new procedure declaration node in the abstract syntax tree.
func newProcedureDeclaration(name string, block Block, index int) Declaration {
	return &ProcedureDeclarationNode{
		commonNode:      commonNode{NodeKind: KindProcedureDeclaration},
		declarationNode: declarationNode{Name: name, DataTypeName: "", IdentifierUsage: make([]Expression, 0), TokenStreamIndex: index},
		ProcedureBlock:  block,
	}
}

// Children nodes of the procedure declaration node.
func (n *ProcedureDeclarationNode) Children() []Node {
	return []Node{n.ProcedureBlock}
}

// String representation of the procedure declaration node.
func (n *ProcedureDeclarationNode) String() string {
	return fmt.Sprintf(procedureFormat, n.Kind(), n.Name, len(n.IdentifierUsage))
}

// Accept the visitor for the procedure declaration node.
func (n *ProcedureDeclarationNode) Accept(visitor Visitor) {
	visitor.VisitProcedureDeclaration(n)
}

// Find a block node that contains this procedure declaration node.
func (n *ProcedureDeclarationNode) Block(mode BlockSearchMode) *BlockNode {
	return searchBlock(n, mode)
}
