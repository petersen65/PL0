// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// Create a new write statement node in the abstract syntax tree.
func newWriteStatement(expression Expression, beginIndex, endIndex int) Statement {
	writeNode := &WriteStatementNode{
		commonNode:    commonNode{NodeKind: KindWriteStatement},
		statementNode: statementNode{TokenStreamIndexBegin: beginIndex, TokenStreamIndexEnd: endIndex},
		Expression:    expression,
	}

	expression.SetParent(writeNode)
	return writeNode
}

// Children nodes of the write statement node.
func (n *WriteStatementNode) Children() []Node {
	return []Node{n.Expression}
}

// String representation of the write statement node.
func (n *WriteStatementNode) String() string {
	return n.Kind().String()
}

// Accept the visitor for the write statement node.
func (n *WriteStatementNode) Accept(visitor Visitor) {
	visitor.VisitWriteStatement(n)
}

// Find a block node that contains this write statement node.
func (n *WriteStatementNode) Block(mode BlockSearchMode) *BlockNode {
	return searchBlock(n, mode)
}
