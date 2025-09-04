// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// The call statement node represents a call statement in the abstract syntax tree.
type callStatementNode struct {
	statementNode
	CallFunction Expression `json:"function"` // function or procedure use of the call statement
}

// Create a new call statement node in the abstract syntax tree.
func newCallStatement(procedure Expression, beginIndex, endIndex int) CallStatement {
	callNode := &callStatementNode{
		statementNode: statementNode{
			commonNode:            commonNode{NodeKind: KindCallStatement},
			TokenStreamIndexBegin: beginIndex,
			TokenStreamIndexEnd:   endIndex,
		},
		CallFunction: procedure,
	}

	procedure.SetParent(callNode)
	return callNode
}

// Children nodes of the call statement node.
func (n *callStatementNode) Children() []Node {
	return []Node{n.CallFunction}
}

// String representation of the call statement node.
func (n *callStatementNode) String() string {
	return n.Kind().String()
}

// Accept the visitor for the call statement node.
func (n *callStatementNode) Accept(visitor Visitor) {
	visitor.VisitCallStatement(n)
}

// Find the current block node that contains this call statement node.
func (n *callStatementNode) CurrentBlock() Block {
	return searchBlock(n, CurrentBlock)
}

// Function or procedure use of the call statement.
func (n *callStatementNode) Function() Expression {
	return n.CallFunction
}
