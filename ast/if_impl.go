// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// The if statement node represents an if-then statement in the abstract syntax tree.
type ifStatementNode struct {
	statementNode
	Condition_   Expression `json:"condition"` // if-condition of the if-then statement
	Statement_ Statement  `json:"statement"` // then-statement of the if-then statement
}

// Create a new if-then statement node in the abstract syntax tree.
func newIfStatement(condition Expression, statement Statement, beginIndex, endIndex int) IfStatement {
	ifNode := &ifStatementNode{
		statementNode: statementNode{
			commonNode:            commonNode{NodeKind: KindIfStatement},
			TokenStreamIndexBegin: beginIndex,
			TokenStreamIndexEnd:   endIndex,
		},
		Condition_:   condition,
		Statement_: statement,
	}

	condition.SetParent(ifNode)
	statement.SetParent(ifNode)
	return ifNode
}

// Children nodes of the if-then statement node.
func (n *ifStatementNode) Children() []Node {
	return []Node{n.Condition_, n.Statement_}
}

// String representation of the if-then statement node.
func (n *ifStatementNode) String() string {
	return n.Kind().String()
}

// Accept the visitor for the if-then statement node.
func (n *ifStatementNode) Accept(visitor Visitor) {
	visitor.VisitIfStatement(n)
}

// Find the current block node that contains this if-then statement node.
func (n *ifStatementNode) CurrentBlock() Block {
	return searchBlock(n, CurrentBlock)
}

// Depth of the block that contains this if-then statement node.
func (n *ifStatementNode) Depth() int {
	return n.CurrentBlock().Depth()
}

// If-condition of the if-then statement.
func (n *ifStatementNode) Condition() Expression {
	return n.Condition_
}

// Then-statement of the if-then statement.
func (n *ifStatementNode) Statement() Statement {
	return n.Statement_
}
