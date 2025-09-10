// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package errors provides the error handling mechanism for the compiler.
package errors

import exp "github.com/petersen65/pl0/v3/export"

// Severity is a bit-mask of different error levels.
const (
	Remark Severity = 1 << iota
	Warning
	Error
	Fatal
	AllSeverities = Severity(^uint64(0))
)

// Components of the compiler that can generate errors.
const (
	Errors Component = 1 << iota
	Debugging
	TypeSystem
	Token
	Scanner
	Parser
	AbstractSyntaxTree
	Analyzer
	Generator
	Intermediate
	ControlFlowGraph
	Emitter
	Intel
	ExecutableLinkableFormat
	AllComponents = Component(^uint64(0))
)

type (
	// Failure is a type for codes that can be mapped to messages.
	Failure int

	// Error levels that are used to categorize errors (bit-mask).
	Severity uint64

	// Component describes packages of the compiler which can generate errors (bit-mask).
	Component uint64

	// A component error is an error that provides additional context about the error, such as its severity and the component it originated from.
	ComponentError interface {
		HasSeverity(severity Severity) bool
		FromComponent(component Component) bool
	}

	// ErrorHandler is an interface that provides methods for error handling and printing.
	ErrorHandler interface {
		exp.Exporter
		AppendError(err error) error
		Count(severity Severity, component Component) int
		HasErrors() bool
		HasWarnings() bool
		HasRemarks() bool
	}
)

// Return the interface to a new error handler.
func NewErrorHandler() ErrorHandler {
	return newErrorHandler()
}

// Create a new Go error with an optional value that can be used to format the error message based on failure code and map.
func NewGoError(failureMap map[Failure]string, code Failure, values ...any) error {
	return newGoError(failureMap, code, values...)
}

// Create a new general error with a severity level (a general error can wrap any other error).
func NewGeneralError(component Component, failureMap map[Failure]string, severity Severity, code Failure, inner error, values ...any) error {
	return newGeneralError(component, failureMap, severity, code, inner, values...)
}

// Create a new line-column error with a severity level and a line and column number.
func NewLineColumnError(component Component, failureMap map[Failure]string, severity Severity, code Failure, line, column int, values ...any) error {
	return newLineColumnError(component, failureMap, severity, code, line, column, values...)
}

// Create a new source error with a severity level, a line and column number, and the source code where the error occurred.
func NewSourceError(component Component, failureMap map[Failure]string, severity Severity, code Failure, line, column int, sourceCode []byte, values ...any) error {
	return newSourceError(component, failureMap, severity, code, line, column, sourceCode, values...)
}
