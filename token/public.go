// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

// Package token provides the token type-system and an error handling mechanism for the PL/0 compiler. This combination enables the error handler to connect an error to a location in the token stream.
package token

import (
	"fmt"
	"io"
	"slices"
	"strings"
)

// Suppress line number and column number in error messages.
const None = -1

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
		NewError(code Failure, value any) error
		AppendError(err error) error
	}

	// Failure is a type for error codes that can be mapped to error messages.
	Failure int

	// Component describes packages of the compiler which can generate errors.
	Component int

	// ErrorHandler is an interface that provides methods for error handling and printing.
	ErrorHandler interface {
		Count() int
		AppendError(err error, index int)
		PrintErrorReport(print io.Writer)
	}
)

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
func NewErrorHandler(tokenStream TokenStream) ErrorHandler {
	return newErrorHandler(tokenStream)
}

// Create a new error by mapping the failure code to its corresponding error message.
func NewFailure(component Component, errorMap map[Failure]string, code Failure, value any, line, column int) error {
	return newFailure(component, errorMap, code, value, line, column)
}

// Print one or several errors as summary to the writer.
func PrintErrorSummary(err error, print io.Writer) {
	if strings.Contains(err.Error(), "\n") {
		print.Write([]byte(fmt.Sprintf("Errors Summary:\n%v\n", err)))
	} else {
		print.Write([]byte(fmt.Sprintf("Error Summary: %v\n", err)))
	}
}
