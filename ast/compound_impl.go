// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// Format for the string representation of the compound statement node.
const compoundStatementFormat = "compound"

// Create a new compound statement node in the abstract syntax tree.
func newCompoundStatement(statements []Statement, beginIndex, endIndex int) Statement {
	compoundNode := &CompoundStatementNode{
		CommonNode:    CommonNode{NodeKind: KindCompoundStatement},
		StatementNode: StatementNode{TokenStreamIndexBegin: beginIndex, TokenStreamIndexEnd: endIndex},
		Statements:    statements,
	}

	for _, statement := range compoundNode.Statements {
		statement.SetParent(compoundNode)
	}

	return compoundNode
}

// Children nodes of the compound statement node.
func (s *CompoundStatementNode) Children() []Node {
	children := make([]Node, 0, len(s.Statements))

	for _, statement := range s.Statements {
		children = append(children, statement)
	}

	return children
}

// String of the compound statement node.
func (s *CompoundStatementNode) String() string {
	return compoundStatementFormat
}	

// Accept the visitor for the compound statement node.
func (s *CompoundStatementNode) Accept(visitor Visitor) {
	visitor.VisitCompoundStatement(s)
}
