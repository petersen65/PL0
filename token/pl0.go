// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package token

// Tokens of the PL/0 programming language.
const (
	Unknown Token = iota
	Error
	Identifier
	Integer
	FloatingPoint
	String
	Character
	Plus
	Minus
	Times
	Divide
	Equal
	NotEqual
	Less
	LessEqual
	Greater
	GreaterEqual
	LeftParenthesis
	RightParenthesis
	Comma
	Colon
	Semicolon
	ProgramEnd
	Becomes
	Read
	Write
	OddWord
	BeginWord
	EndWord
	IfWord
	ThenWord
	WhileWord
	DoWord
	CallWord
	ConstWord
	VarWord
	ProcedureWord
)

// String representation of a PL/0 token.
func (t Token) String() string {
	return tokenNames[t]
}
