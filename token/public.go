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

// Severity is an enumeration of different error levels.
const (
	Information Severity = iota
	Warning
	Error
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
		Token        Token  // token kind
		TokenName    string // token name
		TokenValue   string // token value
		Line, Column int    // position in the source code
		CurrentLine  []byte // source code line
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

	// Failure is a type for codes that can be mapped to messages.
	Failure int

	// Error levels that are used to categorize errors.
	Severity int

	// Component describes packages of the compiler which can generate errors.
	Component int

	// ErrorHandler is an interface that provides methods for error handling and printing.
	ErrorHandler interface {
		Count() int
		AppendError(err error)
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
func NewTokenHandler(tokenStream TokenStream, errorHandler ErrorHandler, component Component, failureMap map[Failure]string) TokenHandler {
	return newTokenHandler(tokenStream, errorHandler, component, failureMap)
}

// Return the public interface to a new error handler.
func NewErrorHandler(tokenStream TokenStream) ErrorHandler {
	return newErrorHandler(tokenStream)
}

// Create a new general error with a severity level.
func NewGeneralError(component Component, failureMap map[Failure]string, severity Severity, code Failure, value any) error {
	return newGeneralError(component, failureMap, severity, code, value)
}

// Create a new line-column error with a severity level and a line and column number.
func NewLineColumnError(component Component, failureMap map[Failure]string, severity Severity, code Failure, value any, line, column int) error {
	return newLineColumnError(component, failureMap, severity, code, value, line, column)
}

// Create a new source error with a severity level, a line and column number, and the source code where the error occurred.
func NewSourceError(component Component, failureMap map[Failure]string, severity Severity, code Failure, value any, line, column int, sourceCode []byte) error {
	return newSourceError(component, failureMap, severity, code, value, line, column, sourceCode)
}

// Create a new token error with a severity level and a token stream that is used to connect errors to a location in the source code.
func NewTokenError(component Component, failureMap map[Failure]string, severity Severity, code Failure, value any, tokenStream TokenStream, index int) error {
	return newTokenError(component, failureMap, severity, code, value, tokenStream, index)
}

// Print one or several errors as summary to the writer.
func PrintErrorSummary(err error, print io.Writer) {
	if strings.Contains(err.Error(), "\n") {
		print.Write([]byte(fmt.Sprintf("Errors Summary:\n%v\n", err)))
	} else {
		print.Write([]byte(fmt.Sprintf("Error Summary: %v\n", err)))
	}
}
