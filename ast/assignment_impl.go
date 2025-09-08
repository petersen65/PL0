// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// Format for the string representation of the assignment statement node.
const assignmentStatementFormat = "assignment"

// The assignment statement node represents an assignment statement in the abstract syntax tree.
type assignmentStatementNode struct {
	statementNode
	Variable_   IdentifierUse `json:"variable_use"` // variable use on the left side of the assignment statement
	Expression_ Expression    `json:"expression"`   // expression on the right side of the assignment statement
}

// Create a new assignment statement node in the abstract syntax tree.
func newAssignmentStatement(variableUse IdentifierUse, expression Expression, beginIndex, endIndex int) AssignmentStatement {
	assignationNode := &assignmentStatementNode{
		statementNode: statementNode{
			commonNode:            commonNode{NodeKind: KindAssignmentStatement},
			TokenStreamIndexBegin: beginIndex,
			TokenStreamIndexEnd:   endIndex,
		},
		Variable_:   variableUse,
		Expression_: expression,
	}

	variableUse.SetParent(assignationNode)
	expression.SetParent(assignationNode)
	return assignationNode
}

// Children nodes of the assignment statement node.
func (s *assignmentStatementNode) Children() []Node {
	return []Node{s.Variable_, s.Expression_}
}

// String representation of the assignment statement node.
func (s *assignmentStatementNode) String() string {
	return assignmentStatementFormat
}

// Accept the visitor for the assignment statement node.
func (s *assignmentStatementNode) Accept(visitor Visitor) {
	visitor.VisitAssignmentStatement(s)
}

// Find the current block node that contains this assignment statement node.
func (s *assignmentStatementNode) CurrentBlock() Block {
	return searchBlock(s, CurrentBlock)
}

// Depth of the block that contains this assignment statement node.
func (s *assignmentStatementNode) Depth() int {
	return s.CurrentBlock().Depth()
}

// Variable use on the left side of the assignment statement.
func (s *assignmentStatementNode) Variable() IdentifierUse {
	return s.Variable_
}

// Expression on the right side of the assignment statement.
func (s *assignmentStatementNode) Expression() Expression {
	return s.Expression_
}
