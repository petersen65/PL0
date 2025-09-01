// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// Create a new while-do statement node in the abstract syntax tree.
func newWhileStatement(condition Expression, statement Statement, beginIndex, endIndex int) Statement {
	whileNode := &WhileStatementNode{
		CommonNode:    CommonNode{NodeKind: KindWhileStatement},
		StatementNode: StatementNode{TokenStreamIndexBegin: beginIndex, TokenStreamIndexEnd: endIndex},
		Condition:     condition,
		Statement:     statement,
	}

	condition.SetParent(whileNode)
	statement.SetParent(whileNode)
	return whileNode
}

// Children nodes of the while-do statement node.
func (n *WhileStatementNode) Children() []Node {
	return []Node{n.Condition, n.Statement}
}

// String representation of the while-do statement node.
func (n *WhileStatementNode) String() string {
	return n.Kind().String()
}	

// Accept the visitor for the while-do statement node.
func (n *WhileStatementNode) Accept(visitor Visitor) {
	visitor.VisitWhileStatement(n)
}
