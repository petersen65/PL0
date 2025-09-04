// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// The if statement node represents an if-then statement in the abstract syntax tree.
type ifStatementNode struct {
	statementNode
	IfCondition   Expression `json:"condition"` // if-condition of the if-then statement
	ThenStatement Statement  `json:"statement"` // then-statement of the if-then statement
}

// Create a new if-then statement node in the abstract syntax tree.
func newIfStatement(condition Expression, statement Statement, beginIndex, endIndex int) IfStatement {
	ifNode := &ifStatementNode{
		statementNode: statementNode{
			commonNode:            commonNode{NodeKind: KindIfStatement},
			TokenStreamIndexBegin: beginIndex,
			TokenStreamIndexEnd:   endIndex,
		},
		IfCondition:   condition,
		ThenStatement: statement,
	}

	condition.SetParent(ifNode)
	statement.SetParent(ifNode)
	return ifNode
}

// Children nodes of the if-then statement node.
func (n *ifStatementNode) Children() []Node {
	return []Node{n.IfCondition, n.ThenStatement}
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

// If-condition of the if-then statement.
func (n *ifStatementNode) Condition() Expression {
	return n.IfCondition
}

// Then-statement of the if-then statement.
func (n *ifStatementNode) Statement() Statement {
	return n.ThenStatement
}
