// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// Format for the string representation of the if statement node.
const ifStatementFormat = "if"

// Create a new if-then statement node in the abstract syntax tree.
func newIfStatement(condition Expression, statement Statement, beginIndex, endIndex int) Statement {
	ifNode := &IfStatementNode{
		CommonNode:    CommonNode{NodeKind: KindIfStatement},
		StatementNode: StatementNode{TokenStreamIndexBegin: beginIndex, TokenStreamIndexEnd: endIndex},
		Condition:     condition,
		Statement:     statement,
	}

	condition.SetParent(ifNode)
	statement.SetParent(ifNode)
	return ifNode
}

// Children nodes of the if-then statement node.
func (s *IfStatementNode) Children() []Node {
	return []Node{s.Condition, s.Statement}
}

// String of the if-then statement node.
func (s *IfStatementNode) String() string {
	return ifStatementFormat
}	

// Accept the visitor for the if-then statement node.
func (s *IfStatementNode) Accept(visitor Visitor) {
	visitor.VisitIfStatement(s)
}
