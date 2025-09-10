// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// Hints for the literal use when the literal is created.
const (
	NoHint            LiteralHint = iota // no special hint provided when the literal was created
	IntegerHint                          // indicates that the literal is an integer literal
	FloatingPointHint                    // indicates that the literal is a floating-point literal
	StringHint                           // indicates that the literal is a string literal
	CharacterHint                        // indicates that the literal is a character literal
)

// Signs for the literal use when the literal is created.
const (
	NoSign       LiteralSign = iota // no sign provided when the literal was created
	PositiveSign                    // indicates that the literal has a plus sign (unary plus)
	NegativeSign                    // indicates that the literal has a minus sign (unary minus)
)

type (
	// Hint for the literal use to support type inference.
	LiteralHint int

	// Sign of the literal use to support unary plus and minus operators.
	LiteralSign int

	// A literal-use node in the abstract syntax tree.
	LiteralUse interface {
		Expression
		Value() any
		Hint() LiteralHint
		Sign() LiteralSign
	}
)

// Create a new literal-use node in the abstract syntax tree.
func NewLiteralUse(literal string, hint LiteralHint, sign LiteralSign, index int) LiteralUse {
	return newLiteralUse(literal, hint, sign, index)
}
