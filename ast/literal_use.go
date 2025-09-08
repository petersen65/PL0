// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// Hints for the literal use when the literal is created.
const (
	NoHint        LiteralHint = iota // no special hint provided when the literal was created
	CharacterHint                    // indicates that the literal is a character literal and not an integer literal
)

type (
	// Hint for the literal use to support type inference.
	LiteralHint int

	// A literal-use node in the abstract syntax tree.
	LiteralUse interface {
		Expression
		Value() any
		Hint() LiteralHint
	}
)

// Create a new literal-use node in the abstract syntax tree.
func NewLiteralUse(value any, hint LiteralHint, index int) LiteralUse {
	return newLiteralUse(value, hint, index)
}
