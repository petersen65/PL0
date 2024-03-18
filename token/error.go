// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package token

import (
	"errors"
	"fmt"
	"io"
	"strings"
)

type (
	// ErrorReport is a list of errors that occurred during the compilation process.
	errorReport []error

	// A general error with a severity level.
	generalError struct {
		err       error     // error message
		code      Failure   // failure code of the error
		component Component // component that generated the error
		severity  Severity  // severity of the error
	}

	// An error with a severity level and a line and column number.
	lineColumnError struct {
		err          error     // error message
		code         Failure   // failure code of the error
		component    Component // component that generated the error
		severity     Severity  // severity of the error
		line, column int       // line and column where the error occurred
	}

	// An self-contained error that can stringify itself to a fully formatted multi-line text pointing to the source code where the error occurred.
	sourceError struct {
		err          error     // error message
		code         Failure   // failure code of the error
		component    Component // component that generated the error
		severity     Severity  // severity of the error
		line, column int       // line and column where the error occurred
		sourceCode   []byte    // source code where the error occurred
	}

	// An self-contained error that can stringify itself to a fully formatted multi-line text pointing to the source code where the error occurred.
	tokenError struct {
		err              error       // error message
		code             Failure     // failure code of the error
		component        Component   // component that generated the error
		severity         Severity    // severity of the error
		tokenStreamIndex int         // index of the token in the token stream where the error occurred
		tokenStream      TokenStream // token stream that is used to connect errors to a location in the source code
	}

	// Private implementation of the error handler.
	errorHandler struct {
		errorReport errorReport // list of errors that occurred during the compilation process
		tokenStream TokenStream // token stream that is used to connect errors to a location in the source code
	}
)

var (
	// Map severity levels to their corresponding names.
	severityMap = map[Severity]string{
		Information: "information",
		Warning:     "warning",
		Error:       "error",
	}

	// Map compiler components to their corresponding names.
	componentMap = map[Component]string{
		Scanner:   "scanner",
		Parser:    "parser",
		Analyzer:  "analyzer",
		Optimizer: "optimizer",
		Generator: "generator",
		Emitter:   "emitter",
		Emulator:  "emulator",
	}
)

// Create a new error handler and initialize it with an empty error report.
func newErrorHandler(tokenStream TokenStream) ErrorHandler {
	return &errorHandler{errorReport: make(errorReport, 0), tokenStream: tokenStream}
}

// Create a new Go error with an optional value that can be used to format the error message based on failure code and map.
func newGoError(failureMap map[Failure]string, code Failure, value any) error {
	var message string

	if value != nil {
		message = fmt.Sprintf(failureMap[code], value)
	} else {
		message = failureMap[code]
	}

	return errors.New(message)
}

// Create a new general error with a severity level.
func newGeneralError(component Component, failureMap map[Failure]string, severity Severity, code Failure, value any) error {
	err := newGoError(failureMap, code, value)
	return &generalError{err: err, code: code, component: component, severity: severity}
}

// Create a new line-column error with a severity level and a line and column number.
func newLineColumnError(component Component, failureMap map[Failure]string, severity Severity, code Failure, value any, line, column int) error {
	err := newGoError(failureMap, code, value)
	return &lineColumnError{err: err, code: code, component: component, severity: severity, line: line, column: column}
}

// Create a new source error with a severity level, a line and column number, and the source code where the error occurred.
func newSourceError(component Component, failureMap map[Failure]string, severity Severity, code Failure, value any, line, column int, sourceCode []byte) error {
	err := newGoError(failureMap, code, value)
	return &sourceError{err: err, code: code, component: component, severity: severity, line: line, column: column, sourceCode: sourceCode}
}

// Create a new token error with a severity level and a token stream that is used to connect errors to a location in the source code.
func newTokenError(component Component, failureMap map[Failure]string, severity Severity, code Failure, value any, tokenStream TokenStream, index int) error {
	err := newGoError(failureMap, code, value)
	return &tokenError{err: err, code: code, component: component, severity: severity, tokenStreamIndex: index, tokenStream: tokenStream}
}

// Implement the error interface for the general error so that it can be used like a native Go error.
func (e *generalError) Error() string {
	message := e.err.Error()
	return fmt.Sprintf("%v %v %v: %v", componentMap[e.component], severityMap[e.severity], e.code, message)
}

// Implement the error interface for the line-column error so that it can be used like a native Go error.
func (e *lineColumnError) Error() string {
	message := e.err.Error()
	return fmt.Sprintf("%v %v %v [%v,%v]: %v", componentMap[e.component], severityMap[e.severity], e.code, e.line, e.column, message)
}

// Implement the error interface for the source error so that it can be used like a native Go error.
func (e *sourceError) Error() string {
	message := e.err.Error()
	message = fmt.Sprintf("%v %v %v [%v,%v]: %v", componentMap[e.component], severityMap[e.severity], e.code, e.line, e.column, message)

	linePrefix := fmt.Sprintf("%5v: ", e.line)
	trimmedLine := strings.TrimLeft(string(e.sourceCode), " \t\n\r")
	trimmedLen := len(string(e.sourceCode)) - len(trimmedLine)
	sourceLine := fmt.Sprintf("\n%v%v\n", linePrefix, trimmedLine)
	errorLine := fmt.Sprintf("%v^ %v\n", strings.Repeat(" ", e.column+len(linePrefix)-trimmedLen-1), message)

	return sourceLine + errorLine
}

// Implement the error interface for the token error so that it can be used like a native Go error.
func (e *tokenError) Error() string {
	td := e.tokenStream[e.tokenStreamIndex]
	message := e.err.Error()
	message = fmt.Sprintf("%v %v %v [%v,%v]: %v", componentMap[e.component], severityMap[e.severity], e.code, td.Line, td.Column, message)

	linePrefix := fmt.Sprintf("%5v: ", td.Line)
	trimmedLine := strings.TrimLeft(string(td.CurrentLine), " \t\n\r")
	trimmedLen := len(string(td.CurrentLine)) - len(trimmedLine)
	sourceLine := fmt.Sprintf("\n%v%v\n", linePrefix, trimmedLine)
	errorLine := fmt.Sprintf("%v^ %v\n", strings.Repeat(" ", td.Column+len(linePrefix)-trimmedLen-1), message)

	return sourceLine + errorLine
}

// Return the number of all errors entries in the error report of the error handler.
func (e *errorHandler) Count() int {
	return len(e.errorReport)
}

// Return the number of errors in the error report of the error handler by severity level.
func (e *errorHandler) CountBySeverity(severity Severity) int {
	var count int

	for _, err := range e.errorReport {
		switch err := err.(type) {
		case *generalError:
			if err.severity == severity {
				count++
			}

		case *lineColumnError:
			if err.severity == severity {
				count++
			}

		case *sourceError:
			if err.severity == severity {
				count++
			}

		case *tokenError:
			if err.severity == severity {
				count++
			}

		default:
			count++
		}
	}

	return count
}

// Append a new error to the error report of the error handler.
func (e *errorHandler) AppendError(err error) {
	e.errorReport = append(e.errorReport, err)
}

// Print the error report to the writer of the error handler.
func (e *errorHandler) PrintErrorReport(print io.Writer) {
	print.Write([]byte("Error Report:"))

	if len(e.errorReport) == 0 {
		print.Write([]byte("\n"))
		return
	}

	for _, err := range e.errorReport {
		print.Write([]byte(err.Error()))
	}
}
