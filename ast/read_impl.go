// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// The read statement node represents a read statement in the abstract syntax tree.
type readStatementNode struct {
	statementNode
	Variable_ IdentifierUse `json:"variable_use"` // variable use of the read statement
}

// Create a new read statement node in the abstract syntax tree.
func newReadStatement(variable IdentifierUse, beginIndex, endIndex int) ReadStatement {
	readNode := &readStatementNode{
		statementNode: statementNode{
			commonNode:            commonNode{NodeKind: KindReadStatement},
			TokenStreamIndexBegin: beginIndex,
			TokenStreamIndexEnd:   endIndex,
		},
		Variable_: variable,
	}

	variable.SetParent(readNode)
	return readNode
}

// Children nodes of the read statement node.
func (n *readStatementNode) Children() []Node {
	return []Node{n.Variable_}
}

// String representation of the read statement node.
func (n *readStatementNode) String() string {
	return n.Kind().String()
}

// Accept the visitor for the read statement node.
func (n *readStatementNode) Accept(visitor Visitor) {
	visitor.VisitReadStatement(n)
}

// Find the current block node that contains this read statement node.
func (n *readStatementNode) CurrentBlock() Block {
	return searchBlock(n, CurrentBlock)
}

// Depth of the block that contains this read statement node.
func (n *readStatementNode) Depth() int {
	return n.CurrentBlock().Depth()
}

// Variable use of the read statement.
func (n *readStatementNode) Variable() IdentifierUse {
	return n.Variable_
}
