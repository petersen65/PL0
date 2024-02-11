// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

// Package scanner implements the PL/0 scanner that performs a lexical analysis of the source code.
package scanner

import "slices"

// Number of bits of a signed integer.
const IntegerBitSize = 64

// Tokens that are supported by the PL/0 scanner.
const (
	Null = Token(iota)
	Eof
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

// Token types for constants and variables.
const (
	None = TokenType(iota)
	Integer64
)

type (
	// Token is a type that represents a token in the source code.
	Token int

	// Data types for constants and variables in PL/0.
	TokenType int

	// Tokens represents a set of tokens.
	Tokens []Token

	// The concrete syntax table of token descriptions is the result of the lexical analysis of the source code. It is consumed by the parser.
	ConcreteSyntax []TokenDescription

	// Describes a token with its type, name, value, and position in the source code.
	TokenDescription struct {
		Token        Token
		TokenName    string
		TokenValue   any
		TokenType    TokenType
		Line, Column int
		CurrentLine  []byte
	}

	// TokenSet is an interface that is used for types that can be converted to the 'Tokens' type.
	TokenSet interface {
		ToTokens() Tokens
	}

	// Scanner is the public interface of the scanner implementation.
	Scanner interface {
		Scan(content []byte) (ConcreteSyntax, error)
	}
)

var (
	// Empty is an empty token set.
	Empty = Tokens{}

	// TokenNames maps tokens to their string representation.
	TokenNames = map[Token]string{
		Null:             "null",
		Eof:              "eof",
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

	// KeyWords is a token set of all keywords.
	KeyWords = Tokens{
		OddWord,
		BeginWord,
		EndWord,
		IfWord,
		ThenWord,
		WhileWord,
		DoWord,
		CallWord,
		ConstWord,
		VarWord,
		ProcedureWord,
	}

	// Operators is a token set of all operators.
	Operators = Tokens{
		OddWord,
		Plus,
		Minus,
		Times,
		Divide,
		Equal,
		NotEqual,
		Less,
		LessEqual,
		Greater,
		GreaterEqual,
	}

	// Declarations is a set of all declaration keywords.
	Declarations = Tokens{
		ConstWord,
		VarWord,
		ProcedureWord,
	}

	// Statements is a set of all statement introductions.
	Statements = Tokens{
		Read,
		Write,
		BeginWord,
		CallWord,
		IfWord,
		WhileWord,
	}

	// Factors is a set of all factors: an identifier, a number, or an expression surrounded by parentheses.
	Factors = Tokens{
		Identifier,
		Number,
		LeftParenthesis,
	}
)

// Return the public interface of the private scanner implementation.
func NewScanner() Scanner {
	return newScanner()
}

// Set returns a joined slice of all tokens within the given TokenSet interfaces. Redundant tokens are removed.
func Set(tss ...TokenSet) Tokens {
	set := Tokens{}

	for _, ts := range tss {
		set = append(set, ts.ToTokens()...)
	}

	slices.Sort(set)
	return slices.Compact(set)
}

// In returns true if the token is in the given tokens.
func (token Token) In(set Tokens) bool {
	return slices.Contains(set, token)
}

// Token.ToTokens concerts a token to a token set to satisfy the TokenSet interface.
func (t Token) ToTokens() Tokens {
	return Tokens{t}
}

// Tokens.ToTokens simply returns the token set that is passed in to satisfy the TokenSet interface.
func (t Tokens) ToTokens() Tokens {
	return t
}
