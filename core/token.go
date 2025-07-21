// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package core

import "slices"

// The eof token is used to indicate the end of the token stream and is used only internally by the token handler.
const eof Token = -1

// Tokens of the PL/0 programming language.
const (
	Unknown Token = iota
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
	// Token is a type that represents a token in the source code.
	Token int

	// Tokens represents a set of tokens.
	Tokens []Token

	// TokenSet is an interface that is used for types that can be converted to the 'Tokens' type.
	TokenSet interface {
		ToTokens() Tokens
	}

	// The token stream table of token descriptions is the result of the lexical analysis of the source code.
	TokenStream []TokenDescription

	// Describes a token with its kind, name, value, datatype, and position in the source code.
	TokenDescription struct {
		Token       Token  `json:"token"`       // token kind
		TokenName   string `json:"token_name"`  // token name
		TokenValue  string `json:"token_value"` // token value
		Line        int    `json:"line"`        // line position in the source code
		Column      int    `json:"column"`      // column position in the source code
		CurrentLine []byte `json:"-"`           // source code line
	}

	// TokenHandler is an interface that provides methods for handling tokens in the token stream.
	TokenHandler interface {
		NextTokenDescription() bool
		LastToken() Token
		LastTokenName() string
		LastTokenValue() string
		LastTokenIndex() int
		Recover(code Failure, expected, fallback Tokens) bool
		IsFullyParsed() bool
		SetFullyParsed()
		NewError(severity Severity, code Failure, value any) error
		NewErrorOnIndex(severity Severity, code Failure, value any, index int) error
		AppendError(err error) error
		ReplaceComponent(component Component, failureMap map[Failure]string)
	}
)

// Return the interface to a new token handler.
func NewTokenHandler(tokenStream TokenStream, errorHandler ErrorHandler, component Component, failureMap map[Failure]string) TokenHandler {
	return newTokenHandler(tokenStream, errorHandler, component, failureMap)
}

// String representation of a token.
func (t Token) String() string {
	return tokenNames[t]
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

// Token.ToTokens converts a token to a token set to satisfy the TokenSet interface.
func (t Token) ToTokens() Tokens {
	return Tokens{t}
}

// Tokens.ToTokens simply returns the token set that is passed in to satisfy the TokenSet interface.
func (t Tokens) ToTokens() Tokens {
	return t
}
