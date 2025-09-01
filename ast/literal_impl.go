// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"fmt"

	sym "github.com/petersen65/pl0/v3/symbol"
)

// Format for the string representation of a literal node.
const literalFormat = "%v(%v)"

// Create a new literal node in the abstract syntax tree.
func newLiteral(value any, scope sym.Scope, index int) Expression {
	return &LiteralNode{
		CommonNode:     CommonNode{NodeKind: KindLiteral},
		ExpressionNode: ExpressionNode{Scope: scope, TokenStreamIndex: index},
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
