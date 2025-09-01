// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

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
func (n *IfStatementNode) Children() []Node {
	return []Node{n.Condition, n.Statement}
}

// String representation of the if-then statement node.
func (n *IfStatementNode) String() string {
	return n.Kind().String()
}

// Accept the visitor for the if-then statement node.
func (n *IfStatementNode) Accept(visitor Visitor) {
	visitor.VisitIfStatement(n)
}
