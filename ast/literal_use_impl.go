// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import "fmt"

// Format for the string representation of a literal-use node.
const literalUseFormat = "use(kind=%v,value=%v)"

// Represents a single use of a literal value.
type literalUseNode struct {
	expressionNode     // embedded expression node
	Value_         any `json:"value"` // value of the literal
}

// Create a new literal-use node in the abstract syntax tree.
func newLiteralUse(value any, index int) LiteralUse {
	return &literalUseNode{
		expressionNode: expressionNode{
			commonNode:       commonNode{NodeKind: KindLiteralUse},
			TokenStreamIndex: index,
		},
		Value_: value,
	}
}

// Children nodes of the literal-use node.
func (n *literalUseNode) Children() []Node {
	return make([]Node, 0)
}

// String representation of the literal-use node.
func (n *literalUseNode) String() string {
	return fmt.Sprintf(literalUseFormat, n.Kind(), n.Value_)
}

// Accept the visitor for the literal-use node.
func (n *literalUseNode) Accept(visitor Visitor) {
	visitor.VisitLiteralUse(n)
}

// Find the current block node that contains this literal-use node.
func (n *literalUseNode) CurrentBlock() Block {
	return searchBlock(n, CurrentBlock)
}

// The literal-use node always represents a constant value.
func (n *literalUseNode) IsConstant() bool {
	return true
}

// Value of the literal in the literal-use node.
func (n *literalUseNode) Value() any {
	return n.Value_
}
