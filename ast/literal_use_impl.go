// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"fmt"

	ts "github.com/petersen65/pl0/v3/typesystem"
)

// Format for the string representation of a literal-use node.
const literalUseFormat = "use(kind=%v,value=%v)"

// Represents a single use of a literal value.
type literalUseNode struct {
	expressionNode                   // embedded expression node
	Value_         any               `json:"literal_value"` // value of the literal
	Hint_          LiteralHint       `json:"literal_hint"`  // hint for the literal (e.g., character literal)
	DataType_      ts.TypeDescriptor `json:"data_type"`     // data type of the literal, might be nil if it was not or cannot be inferred
}

// Create a new literal-use node in the abstract syntax tree.
func newLiteralUse(value any, hint LiteralHint, index int) LiteralUse {
	return &literalUseNode{
		expressionNode: expressionNode{
			commonNode:       commonNode{NodeKind: KindLiteralUse},
			TokenStreamIndex: index,
		},
		Value_: value,
		Hint_:  hint,
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

// Hint for the literal in the literal-use node (e.g., character literal).
func (n *literalUseNode) Hint() LiteralHint {
	return n.Hint_
}

// Determine the data type of the literal used by this literal-use node.
// The used literal might not have a data type if it cannot be inferred. In that case, nil is returned.
func (n *literalUseNode) DataType() ts.TypeDescriptor {
	// if there is no value, there is no data type
	if n.Value_ == nil {
		return nil
	}

	// return the cached data type if available
	if n.DataType_ != nil {
		return n.DataType_
	}

	// character literals are always of type rune (which is only an alias for int32)
	if n.Hint_ == CharacterHint {
		n.DataType_ = ts.NewSimpleTypeDescriptor(ts.Character)
		return n.DataType_
	}

	// infer the data type from the value and cache it
	switch n.Value_.(type) {
	case int64:
		n.DataType_ = ts.NewSimpleTypeDescriptor(ts.Integer64)

	case int32:
		n.DataType_ = ts.NewSimpleTypeDescriptor(ts.Integer32)

	case int16:
		n.DataType_ = ts.NewSimpleTypeDescriptor(ts.Integer16)

	case int8:
		n.DataType_ = ts.NewSimpleTypeDescriptor(ts.Integer8)

	case float64:
		n.DataType_ = ts.NewSimpleTypeDescriptor(ts.Float64)

	case float32:
		n.DataType_ = ts.NewSimpleTypeDescriptor(ts.Float32)

	case uint64:
		n.DataType_ = ts.NewSimpleTypeDescriptor(ts.Unsigned64)

	case uint32:
		n.DataType_ = ts.NewSimpleTypeDescriptor(ts.Unsigned32)

	case uint16:
		n.DataType_ = ts.NewSimpleTypeDescriptor(ts.Unsigned16)

	case uint8:
		n.DataType_ = ts.NewSimpleTypeDescriptor(ts.Unsigned8)

	case bool:
		n.DataType_ = ts.NewSimpleTypeDescriptor(ts.Boolean)

	case string:
		n.DataType_ = ts.NewSimpleTypeDescriptor(ts.String)
	}

	// return the inferred data type, might be nil if it could not be inferred
	return n.DataType_
}
