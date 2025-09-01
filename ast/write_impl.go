// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// Format for the string representation of the write statement node.
const writeStatementFormat = "write"

// Create a new write statement node in the abstract syntax tree.
func newWriteStatement(expression Expression, beginIndex, endIndex int) Statement {
	writeNode := &WriteStatementNode{
		CommonNode:    CommonNode{NodeKind: KindWriteStatement},
		StatementNode: StatementNode{TokenStreamIndexBegin: beginIndex, TokenStreamIndexEnd: endIndex},
		Expression:    expression,
	}

	expression.SetParent(writeNode)
	return writeNode
}

// Children nodes of the write statement node.
func (s *WriteStatementNode) Children() []Node {
	return []Node{s.Expression}
}

// String of the write statement node.
func (s *WriteStatementNode) String() string {
	return writeStatementFormat
}	

// Accept the visitor for the write statement node.
func (s *WriteStatementNode) Accept(visitor Visitor) {
	visitor.VisitWriteStatement(s)
}
