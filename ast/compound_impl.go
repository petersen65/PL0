// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// The compound statement node represents a begin-end statement in the abstract syntax tree.
type compoundStatementNode struct {
	statementNode
	Statements_ []Statement `json:"statements"` // all statements of the begin-end compound statement
}

// Create a new compound statement node in the abstract syntax tree.
func newCompoundStatement(statements []Statement, beginIndex, endIndex int) CompoundStatement {
	compoundNode := &compoundStatementNode{
		statementNode: statementNode{
			commonNode:            commonNode{NodeKind: KindCompoundStatement},
			TokenStreamIndexBegin: beginIndex,
			TokenStreamIndexEnd:   endIndex,
		},
		Statements_: statements,
	}

	for _, statement := range compoundNode.Statements_ {
		statement.SetParent(compoundNode)
	}

	return compoundNode
}

// Children nodes of the compound statement node.
func (n *compoundStatementNode) Children() []Node {
	children := make([]Node, 0, len(n.Statements_))

	for _, statement := range n.Statements_ {
		children = append(children, statement)
	}

	return children
}

// String representation of the compound statement node.
func (n *compoundStatementNode) String() string {
	return n.Kind().String()
}

// Accept the visitor for the compound statement node.
func (n *compoundStatementNode) Accept(visitor Visitor) {
	visitor.VisitCompoundStatement(n)
}

// Find the current block node that contains this compound statement node.
func (n *compoundStatementNode) CurrentBlock() Block {
	return searchBlock(n, CurrentBlock)
}

// Depth of the block that contains this compound statement node.
func (n *compoundStatementNode) Depth() int {
	return n.CurrentBlock().Depth()
}

// All statements of the compound statement.
func (n *compoundStatementNode) Statements() []Statement {
	return n.Statements_
}
