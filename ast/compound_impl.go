// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// Create a new compound statement node in the abstract syntax tree.
func newCompoundStatement(statements []Statement, beginIndex, endIndex int) Statement {
	compoundNode := &CompoundStatementNode{
		commonNode:    commonNode{NodeKind: KindCompoundStatement},
		statementNode: statementNode{TokenStreamIndexBegin: beginIndex, TokenStreamIndexEnd: endIndex},
		Statements:    statements,
	}

	for _, statement := range compoundNode.Statements {
		statement.SetParent(compoundNode)
	}

	return compoundNode
}

// Children nodes of the compound statement node.
func (n *CompoundStatementNode) Children() []Node {
	children := make([]Node, 0, len(n.Statements))

	for _, statement := range n.Statements {
		children = append(children, statement)
	}

	return children
}

// String representation of the compound statement node.
func (n *CompoundStatementNode) String() string {
	return n.Kind().String()
}

// Accept the visitor for the compound statement node.
func (n *CompoundStatementNode) Accept(visitor Visitor) {
	visitor.VisitCompoundStatement(n)
}

// Find a block node that contains this compound statement node.
func (n *CompoundStatementNode) Block(mode BlockSearchMode) *BlockNode {
	return searchBlock(n, mode)
}
