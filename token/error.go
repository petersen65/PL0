// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package token

import (
	"fmt"
	"io"
	"strings"
)

type (
	// ErrorReport is a list of errors that occurred during the compilation process.
	errorReport []componentError

	// ComponentError is a single error that occurred during the compilation process in a compiler component.
	componentError struct {
		err              error // The error that occurred.
		tokenStreamIndex int   // The index of the token in the token stream where the error occurred.
	}

	// Private implementation of the error handler.
	errorHandler struct {
		errorReport errorReport // list of errors that occurred during the compilation process
		tokenStream TokenStream // token stream that is used to connect errors to a location in the source code
	}
)

// Map compiler components to their corresponding names.
var componentMap = map[Component]string{
	Scanner:   "scanner",
	Parser:    "parser",
	Analyzer:  "analyzer",
	Optimizer: "optimizer",
	Generator: "generator",
	Emitter:   "emitter",
	Emulator:  "emulator",
}

// Create a new error handler and initialize it with an empty error report.
func newErrorHandler(tokenStream TokenStream) ErrorHandler {
	return &errorHandler{errorReport: make(errorReport, 0), tokenStream: tokenStream}
}

// Create a new error by mapping the failure code to its corresponding error message.
func newFailure(component Component, errorMap map[Failure]string, code Failure, value any, line, column int) error {
	var message string

	if value != nil {
		message = fmt.Sprintf(errorMap[code], value)
	} else {
		message = errorMap[code]
	}

	if line == None || column == None {
		return fmt.Errorf("%v error %v: %v", componentMap[component], code, message)
	}

	return fmt.Errorf("%v error %v [%v,%v]: %v", componentMap[component], code, line, column, message)
}

// Return the number of errors in the error report of the error handler.
func (e *errorHandler) Count() int {
	return len(e.errorReport)
}

// Append a new error to the error report of the error handler.
func (e *errorHandler) AppendError(err error, index int) {
	e.errorReport = append(e.errorReport, componentError{err: err, tokenStreamIndex: index})
}

// Print the error report to the writer of the error handler.
func (e *errorHandler) PrintErrorReport(print io.Writer) {
	print.Write([]byte("Error Report:"))

	if len(e.errorReport) == 0 {
		print.Write([]byte("\n"))
		return
	}

	for _, err := range e.errorReport {
		td := e.tokenStream[err.tokenStreamIndex]

		linePrefix := fmt.Sprintf("%5v: ", td.Line)
		trimmedLine := strings.TrimLeft(string(td.CurrentLine), " \t\n\r")
		trimmedSpaces := len(string(td.CurrentLine)) - len(trimmedLine)

		print.Write([]byte(fmt.Sprintf("\n%v%v\n", linePrefix, trimmedLine)))
		print.Write([]byte(fmt.Sprintf("%v^ %v\n", strings.Repeat(" ", td.Column+len(linePrefix)-trimmedSpaces-1), err.err)))
	}
}
