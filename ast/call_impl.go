// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// Format for the string representation of the call statement node.
const callStatementFormat = "call"

// Create a new call statement node in the abstract syntax tree.
func newCallStatement(procedure Expression, beginIndex, endIndex int) Statement {
	callNode := &CallStatementNode{
		CommonNode:    CommonNode{NodeKind: KindCallStatement},
		StatementNode: StatementNode{TokenStreamIndexBegin: beginIndex, TokenStreamIndexEnd: endIndex},
		Procedure:     procedure,
	}

	procedure.SetParent(callNode)
	return callNode
}

// Children nodes of the call statement node.
func (s *CallStatementNode) Children() []Node {
	return []Node{s.Procedure}
}

// String of the call statement node.
func (s *CallStatementNode) String() string {
	return callStatementFormat
}	

// Accept the visitor for the call statement node.
func (s *CallStatementNode) Accept(visitor Visitor) {
	visitor.VisitCallStatement(s)
}
