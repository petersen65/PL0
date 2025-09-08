// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// The write statement node represents a write statement in the abstract syntax tree.
type writeStatementNode struct {
	statementNode
	Expression_ Expression `json:"expression"` // expression of the write statement
}

// Create a new write statement node in the abstract syntax tree.
func newWriteStatement(expression Expression, beginIndex, endIndex int) WriteStatement {
	writeNode := &writeStatementNode{
		statementNode: statementNode{
			commonNode:            commonNode{NodeKind: KindWriteStatement},
			TokenStreamIndexBegin: beginIndex,
			TokenStreamIndexEnd:   endIndex,
		},
		Expression_: expression,
	}

	expression.SetParent(writeNode)
	return writeNode
}

// Children nodes of the write statement node.
func (n *writeStatementNode) Children() []Node {
	return []Node{n.Expression_}
}

// String representation of the write statement node.
func (n *writeStatementNode) String() string {
	return n.Kind().String()
}

// Accept the visitor for the write statement node.
func (n *writeStatementNode) Accept(visitor Visitor) {
	visitor.VisitWriteStatement(n)
}

// Find the current block node that contains this write statement node.
func (n *writeStatementNode) CurrentBlock() Block {
	return searchBlock(n, CurrentBlock)
}

// Depth of the block that contains this write statement node.
func (n *writeStatementNode) Depth() int {
	return n.CurrentBlock().Depth()
}

// Expression of the write statement.
func (n *writeStatementNode) Expression() Expression {
	return n.Expression_
}
