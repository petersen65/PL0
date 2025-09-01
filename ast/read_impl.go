// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// Format for the string representation of the read statement node.
const readStatementFormat = "read"

// Create a new read statement node in the abstract syntax tree.
func newReadStatement(variable Expression, beginIndex, endIndex int) Statement {
	readNode := &ReadStatementNode{
		CommonNode:    CommonNode{NodeKind: KindReadStatement},
		StatementNode: StatementNode{TokenStreamIndexBegin: beginIndex, TokenStreamIndexEnd: endIndex},
		Variable:      variable,
	}

	variable.SetParent(readNode)
	return readNode
}

// Children nodes of the read statement node.
func (s *ReadStatementNode) Children() []Node {
	return []Node{s.Variable}
}

// String of the read statement node.
func (s *ReadStatementNode) String() string {
	return readStatementFormat
}	

// Accept the visitor for the read statement node.
func (s *ReadStatementNode) Accept(visitor Visitor) {
	visitor.VisitReadStatement(s)
}
