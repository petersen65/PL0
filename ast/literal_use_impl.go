// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"fmt"
	"math"
	"strconv"

	ts "github.com/petersen65/pl0/v3/typesystem"
)

// Number of bits of an integer or floating-point literal.
const literalBitSize = 64

// Format for the string representation of a literal-use node.
const literalUseFormat = "use(kind=%v,literal=%v)"

// Represents a single use of a literal value.
type literalUseNode struct {
	expressionNode                   // embedded expression node
	Tried_         bool              `json:"value_tried"`   // indicates whether the literal-use node has tried to infer the value
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
	// return the cached value if available
	if n.Value_ != nil {
		return n.Value_
	}

	// if there was no hint provided, try out several hints in order of preference
	if n.Hint_ == NoHint {
		// try integer inference first
		n.Hint_ = IntegerHint

		// return the value if integer inference succeeded
		if value := n.Value(); value != nil {
			return value
		}

		// if integer inference failed, try floating-point next
		n.Hint_ = FloatingPointHint

		// return the value if floating-point inference succeeded
		if value := n.Value(); value != nil {
			return value
		}

		// if floating-point inference also failed, check the length of the orignal literal string
		if runes := []rune(n.Literal_); len(runes) == 1 {
			// if the literal is a single rune, it must be a character literal
			n.Hint_ = CharacterHint
		}

		// return the value if character inference succeeded
		if value := n.Value(); value != nil {
			return value
		}

		// if all inference attempts failed, assume it is a string literal
		n.Hint_ = StringHint
		n.Value_ = n.Literal_
		return n.Value_
	}

	// based on the original literal string, a provided hint, and sign, try to infer the value
	switch n.Hint_ {
	case IntegerHint:
		// check sign to determine which parsing function to use
		switch n.Sign_ {
		case NegativeSign:
			// parse all signed integer literals to the data type 'int64'
			if literal, err := strconv.ParseInt(n.Literal_, 10, literalBitSize); err == nil {
				n.Value_ = -literal
			}

		case PositiveSign, NoSign:
			// parse all unsigned integer literals to the data type 'uint64'
			if literal, err := strconv.ParseUint(n.Literal_, 10, literalBitSize); err == nil {
				// if the parsed value fits into the type 'int64', that type is preferred
				if literal <= math.MaxInt64 {
					n.Value_ = int64(literal)
				} else {
					n.Value_ = literal
				}
			}
		}

	case FloatingPointHint:
		// parse all floating-point literals to the data type 'float64'
		if literal, err := strconv.ParseFloat(n.Literal_, literalBitSize); err == nil {
			// check literal sign to calculate the final value
			switch n.Sign_ {
			case NegativeSign:
				n.Value_ = -literal

			case PositiveSign, NoSign:
				n.Value_ = literal
			}
		}

	case StringHint:
		// string literals are stored as-is without parsing and can be empty
		n.Value_ = n.Literal_

	case CharacterHint:
		// parse character literals to rune
		if len(n.Literal_) > 0 {
			// convert original string to rune slice and take the first rune
			runes := []rune(n.Literal_)

			// only take the first rune, ignore the rest
			if len(runes) > 0 {
				n.Value_ = runes[0]
			}
		}
	}

	// mark that the literal-use node tried to infer the value
	n.Tried_ = true

	// return the inferred value, might be nil if it could not be inferred
	return n.Value_
}

// Determine the data type of the literal in this literal-use node.
// The literal might not have a data type if it cannot be inferred. In that case, nil is returned.
func (n *literalUseNode) DataType() ts.TypeDescriptor {
	// first ensure that the value has been inferred before inferring the data type
	if !n.Tried_ {
		n.Value()
	}

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

	// // character literals require a hint for type inference because 'rune' is an alias for 'int32' and cannot be distinguished at runtime
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
