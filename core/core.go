// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package core provides foundation features, the token type-system, and the error handling mechanism for the PL/0 compiler. This combination enables the error handler to connect an error to a location in the token stream. Package core is required to depend on Go standard library packages only.
package core

import (
	"io"
	"slices"
)

// The eof token is used to indicate the end of the token stream and is used only internally by the token handler.
const eof Token = -1

// Tokens of the PL/0 programming language.
const (
	Unknown = Token(iota)
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

// Export formats for the compiler which can be used to export intermediate results.
const (
	Json = ExportFormat(iota)
	Text
	Binary
)

// Packages of the compiler which can generate errors as a bit-mask enumeration.
const (
	Core Component = 1 << iota
	Scanner
	Parser
	AbstractSyntaxTree
	Analyzer
	Generator
	Intermediate
	ControlFlowGraph
	Emitter
	Assembly
	AllComponents = Component(^uint64(0))
)

// Severity is a bit-mask enumeration of different error levels.
const (
	Remark Severity = 1 << iota
	Warning
	Error
	Fatal
	AllSeverities = Severity(^uint64(0))
)

type (
	// Export formats for the compiler.
	ExportFormat int

	// Exporter is an interface that provides methods for exporting intermediate results.
	Exporter interface {
		Print(print io.Writer, args ...any) error
		Export(format ExportFormat, print io.Writer) error
	}

	// Importer is an interface that provides methods for importing intermediate results.
	Importer interface {
		Import(format ExportFormat, scan io.Reader) error
	}

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

	// Failure is a type for codes that can be mapped to messages.
	Failure int

	// Error levels that are used to categorize errors (bit-mask).
	Severity uint64

	// Component describes packages of the compiler which can generate errors (bit-mask).
	Component uint64

	// ErrorHandler is an interface that provides methods for error handling and printing.
	ErrorHandler interface {
		AppendError(err error) error
		Count(severity Severity, component Component) int
		HasErrors() bool
		HasWarnings() bool
		HasRemarks() bool
		Print(print io.Writer, args ...any) error
		Export(format ExportFormat, print io.Writer) error
	}
)

var (
	// Empty is an empty token set.
	Empty = Tokens{}

	// TokenNames maps tokens to their string representation.
	TokenNames = map[Token]string{
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

// Return the interface to a new token handler.
func NewTokenHandler(tokenStream TokenStream, errorHandler ErrorHandler, component Component, failureMap map[Failure]string) TokenHandler {
	return newTokenHandler(tokenStream, errorHandler, component, failureMap)
}

// Return the interface to a new error handler.
func NewErrorHandler(tokenStream TokenStream) ErrorHandler {
	return newErrorHandler(tokenStream)
}

// Create a new general error with a severity level (a general error can wrap any other error).
func NewGeneralError(component Component, failureMap map[Failure]string, severity Severity, code Failure, value any, inner error) error {
	return newGeneralError(component, failureMap, severity, code, value, inner)
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
