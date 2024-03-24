// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package scanner implements the PL/0 scanner that performs a lexical analysis of the source code.
package scanner

import cor "github.com/petersen65/PL0/core"

// Tokens that are supported by the PL/0 scanner.
const (
	Unknown = cor.Token(iota)
	Identifier
	Number
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

type (
	// The scanner interface provides methods for scanning binary UTF-8 encoded source code into a binary token stream.
	Scanner interface {
		Scan(content []byte) (cor.TokenStream, error)
	}
)

var (
	// Empty is an empty token set.
	Empty = cor.Tokens{}

	// TokenNames maps tokens to their string representation.
	TokenNames = map[cor.Token]string{
		Unknown:          "unknown",
		Identifier:       "identifier",
		Number:           "number",
		Plus:             "plus",
		Minus:            "minus",
		Times:            "times",
		Divide:           "divide",
		Equal:            "equal",
		NotEqual:         "notEqual",
		Less:             "less",
		LessEqual:        "lessEqual",
		Greater:          "greater",
		GreaterEqual:     "greaterEqual",
		LeftParenthesis:  "leftParenthesis",
		RightParenthesis: "rightParenthesis",
		Comma:            "comma",
		Colon:            "colon",
		Semicolon:        "semicolon",
		ProgramEnd:       "programEnd",
		Becomes:          "becomes",
		Read:             "read",
		Write:            "write",
		OddWord:          "odd",
		BeginWord:        "begin",
		EndWord:          "end",
		IfWord:           "if",
		ThenWord:         "then",
		WhileWord:        "while",
		DoWord:           "do",
		CallWord:         "call",
		ConstWord:        "const",
		VarWord:          "var",
		ProcedureWord:    "procedure",
	}
)

// Return the public interface of the private scanner implementation.
func NewScanner() Scanner {
	return newScanner()
}
