// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package core

import "io"

// Severity is a bit-mask enumeration of different error levels.
const (
	Remark Severity = 1 << iota
	Warning
	Error
	Fatal
	AllSeverities = Severity(^uint64(0))
)

type (
	// Failure is a type for codes that can be mapped to messages.
	Failure int

	// Error levels that are used to categorize errors (bit-mask).
	Severity uint64

	// A component error is an error that provides additional context about the error, such as its severity and the component it originated from.
	ComponentError interface {
		HasSeverity(severity Severity) bool
		FromComponent(component Component) bool
	}

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

// Return the interface to a new error handler.
func NewErrorHandler() ErrorHandler {
	return newErrorHandler()
}

// Create a new Go error with an optional value that can be used to format the error message based on failure code and map.
func NewGoError(failureMap map[Failure]string, code Failure, value any) error {
	return newGoError(failureMap, code, value)
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
