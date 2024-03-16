// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

// Package token provides the token type-system and an error handling mechanism for the PL/0 compiler. This combination enables the error handler to connect an error to a location in the token stream.
package token

import (
	"io"
	"slices"
)

// Packages of the compiler which can generate errors.
const (
	_ = Component(iota)
	Scanner
	Parser
	Analyzer
	Optimizer
	Generator
	Emitter
	Emulator
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
		Token        Token
		TokenName    string
		TokenValue   string
		Line, Column int
		CurrentLine  []byte
	}

	// TokenHandler is an interface that provides methods for handling tokens in the token stream.
	TokenHandler interface {
		NextTokenDescription() bool
		LastToken() Token
		LastTokenName() string
		LastTokenValue() string
		Rebase(code Failure, expected, fallback Tokens) bool
		IsFullyParsed() bool
		SetFullyParsed()
		AppendError(err error) error
	}

	// Failure is a type for error codes that can be mapped to error messages.
	Failure int

	// Component describes packages of the compiler which can generate errors.
	Component int

	// ErrorReport is a list of errors that occurred during the compilation process.
	ErrorReport []Error

	// Error is a single error that occurred during the compilation process.
	Error struct {
		Err              error // The error that occurred.
		TokenStreamIndex int   // The index of the token in the token stream where the error occurred.
	}

	// ErrorHandler is an interface that provides methods for error handling and printing.
	ErrorHandler interface {
		HasErrors() bool
		AppendError(err error, index int) Error
		GetTokenDescription(err *Error) TokenDescription
		PrintError(err error)
		PrintErrorReport()
	}
)

// Map compiler components to their corresponding names.
var ComponentMap = map[Component]string{
	Scanner:   "scanner",
	Parser:    "parser",
	Analyzer:  "analyzer",
	Optimizer: "optimizer",
	Generator: "generator",
	Emitter:   "emitter",
	Emulator:  "emulator",
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

// Return the public interface to a new token handler.
func NewTokenHandler(tokenStream TokenStream, errorHandler ErrorHandler, component Component, errorMap map[Failure]string) TokenHandler {
	return newTokenHandler(tokenStream, errorHandler, component, errorMap)
}

// Return the public interface to a new error handler.
func NewErrorHandler(tokenStream TokenStream, print io.Writer) ErrorHandler {
	return newErrorHandler(tokenStream, print)
}

// Create a new error by mapping the failure code to its corresponding error message.
func NewFailure(component Component, errorMap map[Failure]string, code Failure, value any, line, column int) error {
	return newFailure(component, errorMap, code, value, line, column)
}
