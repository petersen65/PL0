// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"fmt"

	ts "github.com/petersen65/pl0/v3/typesystem"
)

// Format for the string representation of a literal-use node.
const literalUseFormat = "use(kind=%v,literal=%v)"

// Represents a single use of a literal value.
type literalUseNode struct {
	expressionNode                   // embedded expression node
	Literal_       string            `json:"literal"`       // the original literal string
	Hint_          LiteralHint       `json:"literal_hint"`  // hint for the literal (e.g., character literal or integer literal)
	Sign_          LiteralSign       `json:"literal_sign"`  // sign of the literal (e.g., unary plus or minus)
	Value_         any               `json:"literal_value"` // inferred value of the literal, might be nil if it was not or cannot be inferred
	DataType_      ts.TypeDescriptor `json:"data_type"`     // inferred data type of the literal, might be nil if it was not or cannot be inferred
}

// Create a new literal-use node in the abstract syntax tree.
func newLiteralUse(literal string, hint LiteralHint, sign LiteralSign, index int) LiteralUse {
	return &literalUseNode{
		expressionNode: expressionNode{
			commonNode:       commonNode{NodeKind: KindLiteralUse},
			TokenStreamIndex: index,
		},
		Literal_: literal,
		Hint_:    hint,
		Sign_:    sign,
	}
}

// Children nodes of the literal-use node.
func (n *literalUseNode) Children() []Node {
	return make([]Node, 0)
}

// String representation of the literal-use node.
func (n *literalUseNode) String() string {
	return fmt.Sprintf(literalUseFormat, n.Kind(), n.Literal_)
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

// Hint for the literal in the literal-use node (e.g., character literal).
func (n *literalUseNode) Hint() LiteralHint {
	return n.Hint_
}

// Sign of the literal in the literal-use node (e.g., unary plus or minus).
func (n *literalUseNode) Sign() LiteralSign {
	return n.Sign_
}

// Determine the value of the literal in this literal-use node.
// The literal might not have a value if it cannot be inferred. In that case, nil is returned.
func (n *literalUseNode) Value() any {
	return n.Value_
}

// Determine the data type of the literal in this literal-use node.
// The literal might not have a data type if it cannot be inferred. In that case, nil is returned.
func (n *literalUseNode) DataType() ts.TypeDescriptor {
	// if there is no value, there is no data type
	if n.Value_ == nil {
		return nil
	}

	// return the cached data type if available
	if n.DataType_ != nil {
		return n.DataType_
	}

	// find the current block node to look up built-in data types
	cb := n.CurrentBlock()

	// character literals are always of type rune (which is only an alias for int32)
	if n.Hint_ == CharacterHint {
		n.DataType_ = cb.BuiltInDataType(ts.Character.String())
		return n.DataType_
	}

	// infer the data type from the value and cache it
	switch n.Value_.(type) {
	case int64:
		n.DataType_ = cb.BuiltInDataType(ts.Integer64.String())

	case int32:
		n.DataType_ = cb.BuiltInDataType(ts.Integer32.String())

	case int16:
		n.DataType_ = cb.BuiltInDataType(ts.Integer16.String())

	case int8:
		n.DataType_ = cb.BuiltInDataType(ts.Integer8.String())

	case float64:
		n.DataType_ = cb.BuiltInDataType(ts.Float64.String())

	case float32:
		n.DataType_ = cb.BuiltInDataType(ts.Float32.String())

	case uint64:
		n.DataType_ = cb.BuiltInDataType(ts.Unsigned64.String())

	case uint32:
		n.DataType_ = cb.BuiltInDataType(ts.Unsigned32.String())

	case uint16:
		n.DataType_ = cb.BuiltInDataType(ts.Unsigned16.String())

	case uint8:
		n.DataType_ = cb.BuiltInDataType(ts.Unsigned8.String())

	case bool:
		n.DataType_ = cb.BuiltInDataType(ts.Boolean.String())

	case string:
		n.DataType_ = cb.BuiltInDataType(ts.String.String())
	}

	// return the inferred data type, might be nil if it could not be inferred
	return n.DataType_
}
