// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

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
func (n *CallStatementNode) Children() []Node {
	return []Node{n.Procedure}
}

// String representation of the call statement node.
func (n *CallStatementNode) String() string {
	return n.Kind().String()
}

// Accept the visitor for the call statement node.
func (n *CallStatementNode) Accept(visitor Visitor) {
	visitor.VisitCallStatement(n)
}
