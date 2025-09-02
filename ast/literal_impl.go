// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import "fmt"

// Format for the string representation of a literal node.
const literalFormat = "%v(%v)"

// Create a new literal node in the abstract syntax tree.
func newLiteral(value any, index int) Expression {
	return &LiteralNode{
		commonNode:     commonNode{NodeKind: KindLiteral},
		expressionNode: expressionNode{TokenStreamIndex: index},
		Value:          value,
	}
}

// Children nodes of the literal node.
func (n *LiteralNode) Children() []Node {
	return make([]Node, 0)
}

// String representation of the literal node.
func (n *LiteralNode) String() string {
	return fmt.Sprintf(literalFormat, n.Kind(), n.Value)
}

// Accept the visitor for the literal node.
func (n *LiteralNode) Accept(visitor Visitor) {
	visitor.VisitLiteral(n)
}

// Find the current block node that contains this literal node.
func (n *LiteralNode) CurrentBlock() Block {
	return searchBlock(n, CurrentBlock)
}
