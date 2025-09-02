// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// Format for the string representation of the assignment statement node.
const assignmentStatementFormat = "assignment"

// Create a new assignment statement node in the abstract syntax tree.
func newAssignmentStatement(variable, expression Expression, beginIndex, endIndex int) Statement {
	assignationNode := &AssignmentStatementNode{
		commonNode:    commonNode{NodeKind: KindAssignmentStatement},
		statementNode: statementNode{TokenStreamIndexBegin: beginIndex, TokenStreamIndexEnd: endIndex},
		Variable:      variable,
		Expression:    expression,
	}

	variable.SetParent(assignationNode)
	expression.SetParent(assignationNode)
	return assignationNode
}

// Children nodes of the assignment statement node.
func (s *AssignmentStatementNode) Children() []Node {
	return []Node{s.Variable, s.Expression}
}

// String representation of the assignment statement node.
func (s *AssignmentStatementNode) String() string {
	return assignmentStatementFormat
}

// Accept the visitor for the assignment statement node.
func (s *AssignmentStatementNode) Accept(visitor Visitor) {
	visitor.VisitAssignmentStatement(s)
}

// Find the current block node that contains this assignment statement node.
func (s *AssignmentStatementNode) CurrentBlock() Block {
	return searchBlock(s, CurrentBlock)
}
