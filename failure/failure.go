// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package failure

import (
	"fmt"
	"io"
	"strings"
)

// Private implementation of the error handler.
type errorHandler struct {
	errorReport ErrorReport
	tokenStream TokenStream
	print       io.Writer
}

// Create a new error handler and initialize it with an empty error report.
func newErrorHandler(tokenStream TokenStream, print io.Writer) ErrorHandler {
	return &errorHandler{errorReport: make(ErrorReport, 0), tokenStream: tokenStream, print: print}
}

// Create a new error by mapping the failure code to its corresponding error message.
func newFailure(component Component, errorMap map[Failure]string, code Failure, value any, line, column int) error {
	var message string

	if value != nil {
		message = fmt.Sprintf(errorMap[code], value)
	} else {
		message = errorMap[code]
	}

	return fmt.Errorf("%v error %v [%v,%v]: %v", ComponentMap[component], code, line, column, message)
}

// Check if the error handler has errors in its error report.
func (e *errorHandler) HasErrors() bool {
	return len(e.errorReport) > 0
}

// Append a new error to the error report of the error handler.
func (e *errorHandler) AppendError(err error, index int) Error {
	e.errorReport = append(e.errorReport, Error{Err: err, TokenStreamIndex: index})
	return e.errorReport[len(e.errorReport)-1]
}

// Get the token description of an error in the error report.
func (e *errorHandler) GetTokenDescription(err *Error) TokenDescription {
	return e.tokenStream[err.TokenStreamIndex]
}

// Print one or several errors to the writer of the error handler.
func (e *errorHandler) PrintError(err error) {
	if strings.Contains(err.Error(), "\n") {
		e.print.Write([]byte(fmt.Sprintf("Errors Summary:\n%v\n", err)))
	} else {
		e.print.Write([]byte(fmt.Sprintf("Error Summary: %v\n", err)))
	}
}

// Print the error report to the writer of the error handler.
func (e *errorHandler) PrintErrorReport() {
	e.print.Write([]byte("Error Report:"))

	if len(e.errorReport) == 0 {
		e.print.Write([]byte("\n"))
		return
	}

	for _, err := range e.errorReport {
		td := e.GetTokenDescription(&err)

		linePrefix := fmt.Sprintf("%5v: ", td.Line)
		trimmedLine := strings.TrimSpace(string(td.CurrentLine))
		trimmedSpaces := len(string(td.CurrentLine)) - len(trimmedLine)

		e.print.Write([]byte(fmt.Sprintf("\n%v%v\n", linePrefix, trimmedLine)))
		e.print.Write([]byte(fmt.Sprintf("%v^ %v\n", strings.Repeat(" ", td.Column+len(linePrefix)-trimmedSpaces-1), err.Err)))
	}
}
