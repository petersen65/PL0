// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import "fmt"

// Format for the string representation of a procedure declaration node.
const procedureFormat = "declaration(%v,name=%v,used=%v)"

// The node represents a procedure declaration in the abstract syntax tree.
type procedureDeclarationNode struct {
	commonNode            // embedded common node
	declarationNode       // embedded declaration node
	Block           Block `json:"procedure_block"` // block of the procedure
}

// Create a new procedure declaration node in the abstract syntax tree.
func newProcedureDeclaration(name string, block Block, index int) ProcedureDeclaration {
	return &procedureDeclarationNode{
		commonNode:      commonNode{NodeKind: KindProcedureDeclaration},
		declarationNode: declarationNode{Identifier: name, DataType: "", IdentifierUsage: make([]Expression, 0), TokenStreamIndex: index},
		Block:           block,
	}
}

// Children nodes of the procedure declaration node.
func (n *procedureDeclarationNode) Children() []Node {
	return []Node{n.Block}
}

// String representation of the procedure declaration node.
func (n *procedureDeclarationNode) String() string {
	return fmt.Sprintf(procedureFormat, n.NodeKind, n.Identifier, len(n.IdentifierUsage))
}

// Accept the visitor for the procedure declaration node.
func (n *procedureDeclarationNode) Accept(visitor Visitor) {
	visitor.VisitProcedureDeclaration(n)
}

// Find the current block node that contains this procedure declaration node.
func (n *procedureDeclarationNode) CurrentBlock() Block {
	return searchBlock(n, CurrentBlock)
}
