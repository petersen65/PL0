// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// Format for the string representation of the while statement node.
const whileStatementFormat = "while"

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
func (s *WhileStatementNode) Children() []Node {
	return []Node{s.Condition, s.Statement}
}

// String of the while-do statement node.
func (s *WhileStatementNode) String() string {
	return whileStatementFormat
}	

// Accept the visitor for the while-do statement node.
func (s *WhileStatementNode) Accept(visitor Visitor) {
	visitor.VisitWhileStatement(s)
}
