// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// The while statement node represents a while-do statement in the abstract syntax tree.
type whileStatementNode struct {
	statementNode
	Condition_ Expression `json:"condition"` // while-condition of the while-do statement
	Statement_ Statement  `json:"statement"` // do-statement of the while-do statement
}

// Create a new while-do statement node in the abstract syntax tree.
func newWhileStatement(condition Expression, statement Statement, beginIndex, endIndex int) WhileStatement {
	whileNode := &whileStatementNode{
		statementNode: statementNode{
			commonNode:            commonNode{NodeKind: KindWhileStatement},
			TokenStreamIndexBegin: beginIndex,
			TokenStreamIndexEnd:   endIndex,
		},
		Condition_: condition,
		Statement_: statement,
	}

	condition.SetParent(whileNode)
	statement.SetParent(whileNode)
	return whileNode
}

// Children nodes of the while-do statement node.
func (n *whileStatementNode) Children() []Node {
	return []Node{n.Condition_, n.Statement_}
}

// String representation of the while-do statement node.
func (n *whileStatementNode) String() string {
	return n.Kind().String()
}

// Accept the visitor for the while-do statement node.
func (n *whileStatementNode) Accept(visitor Visitor) {
	visitor.VisitWhileStatement(n)
}

// Find the current block node that contains this while-do statement node.
func (n *whileStatementNode) CurrentBlock() Block {
	return searchBlock(n, CurrentBlock)
}

// Depth of the block that contains this while-do statement node.
func (n *whileStatementNode) Depth() int {
	return n.CurrentBlock().Depth()
}

// While-condition of the while-do statement.
func (n *whileStatementNode) Condition() Expression {
	return n.Condition_
}

// Do-statement of the while-do statement.
func (n *whileStatementNode) Statement() Statement {
	return n.Statement_
}
