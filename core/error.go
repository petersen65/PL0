// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package core

import (
	"errors"
	"fmt"
	"io"
	"reflect"
	"strings"
)

// Failure codes for core components of the PL/0 compiler.
const (
	_ = Failure(iota + 100)
	errorKindNotSupported
	unknownExportFormat
	errorReportExportFailed
	tokenStreamExportFailed
)

// Map failure codes to error messages.
var failureMap = map[Failure]string{
	errorKindNotSupported:   "error kind not supported: %v",
	unknownExportFormat:     "unknown export format: %v",
	errorReportExportFailed: "failed to export the error report",
	tokenStreamExportFailed: "failed to export the token stream",
}

type (
	// ErrorReport is a list of errors that occurred during the compilation process.
	errorReport []error

	// A general error with a severity level and an optional inner error.
	generalError struct {
		err       error     // error message
		code      Failure   // failure code of the error
		component Component // component that generated the error
		severity  Severity  // severity of the error
		inner     error     // inner error wrapped by the general error
	}

	// An error with a severity level and a line and column number.
	lineColumnError struct {
		err          error     // error message
		code         Failure   // failure code of the error
		component    Component // component that generated the error
		severity     Severity  // severity of the error
		line, column int32     // line and column where the error occurred
	}

	// An self-contained error that can stringify itself to a fully formatted multi-line text pointing to the source code where the error occurred.
	sourceError struct {
		err          error     // error message
		code         Failure   // failure code of the error
		component    Component // component that generated the error
		severity     Severity  // severity of the error
		line, column int32     // line and column where the error occurred
		sourceCode   []byte    // source code where the error occurred
	}

	// An self-contained error that can stringify itself to a fully formatted multi-line text pointing to the source code where the error occurred.
	tokenError struct {
		err              error       // error message
		code             Failure     // failure code of the error
		component        Component   // component that generated the error
		severity         Severity    // severity of the error
		tokenStreamIndex int64       // index of the token in the token stream where the error occurred
		tokenStream      TokenStream // token stream that is used to connect errors to a location in the source code
	}

	// Private implementation of the error handler.
	errorHandler struct {
		errorReport errorReport // list of errors that occurred during the compilation process
		tokenStream TokenStream // token stream that is used to connect errors to a location in the source code
	}
)

var (
	// Text messages for printing an error report.
	textErrorReport = []byte("Error Report:\n")
	textErrors      = []byte("Errors:")
	textWarnings    = []byte("Warnings:")
	textRemarks     = []byte("Remarks:")

	// Map severity levels to their corresponding names.
	severityMap = map[Severity]string{
		Remark:  "remark",
		Warning: "warning",
		Error:   "error",
		Fatal:   "fatal",
	}

	// Map compiler components to their corresponding names.
	componentMap = map[Component]string{
		Core:               "core",
		Scanner:            "scanner",
		Parser:             "parser",
		AbstractSyntaxTree: "ast",
		Analyzer:           "analyzer",
		Optimizer:          "optimizer",
		Generator:          "generator",
		Emitter:            "emitter",
		Emulator:           "emulator",
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
func newGeneralError(component Component, failureMap map[Failure]string, severity Severity, code Failure, value any, inner error) error {
	err := newGoError(failureMap, code, value)
	return &generalError{err: err, code: code, component: component, severity: severity, inner: inner}
}

// Create a new line-column error with a severity level and a line and column number.
func newLineColumnError(component Component, failureMap map[Failure]string, severity Severity, code Failure, value any, line, column int32) error {
	err := newGoError(failureMap, code, value)
	return &lineColumnError{err: err, code: code, component: component, severity: severity, line: line, column: column}
}

// Create a new source error with a severity level, a line and column number, and the source code where the error occurred.
func newSourceError(component Component, failureMap map[Failure]string, severity Severity, code Failure, value any, line, column int32, sourceCode []byte) error {
	err := newGoError(failureMap, code, value)
	return &sourceError{err: err, code: code, component: component, severity: severity, line: line, column: column, sourceCode: sourceCode}
}

// Create a new token error with a severity level and a token stream that is used to connect errors to a location in the source code.
func newTokenError(component Component, failureMap map[Failure]string, severity Severity, code Failure, value any, tokenStream TokenStream, index int) error {
	err := newGoError(failureMap, code, value)
	return &tokenError{err: err, code: code, component: component, severity: severity, tokenStreamIndex: int64(index), tokenStream: tokenStream}
}

// Implement the error interface for the general error so that it can be used like a native Go error.
func (e *generalError) Error() string {
	message := e.err.Error()
	return fmt.Sprintf("%v %v %v: %v", componentMap[e.component], severityMap[e.severity], e.code, message)
}

// Implement the Unwrap method for the general error so that it can be used to unwrap the inner error.
func (e *generalError) Unwrap() error {
	return e.inner
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
	errorLine := fmt.Sprintf("%v^ %v\n", strings.Repeat(" ", int(e.column)+len(linePrefix)-trimmedLen-1), message)

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
	errorLine := fmt.Sprintf("%v^ %v\n", strings.Repeat(" ", int(td.Column)+len(linePrefix)-trimmedLen-1), message)

	return sourceLine + errorLine
}

// Append a new error to the error report of the error handler only if the error is not nil.
func (e *errorHandler) AppendError(err error) error {
	if err != nil {
		e.errorReport = append(e.errorReport, err)
	}

	return err
}

// Return the number of all errors entries in the error report of the error handler.
func (e *errorHandler) Count(severity Severity, component Component) int {
	var count int

	for range e.iterate(severity, component) {
		count++
	}

	return count
}

// Print the error report to the writer of the error handler.
func (e *errorHandler) Print(print io.Writer, args ...any) error {
	if len(e.errorReport) == 0 {
		return nil
	}

	// print the title text message for the error report
	if _, err := print.Write(textErrorReport); err != nil {
		return newGeneralError(Core, failureMap, Error, errorReportExportFailed, nil, err)
	}

	// print errors in the error report
	if e.Count(Fatal|Error, AllComponents) > 0 {
		if _, err := print.Write(textErrors); err != nil {
			return newGeneralError(Core, failureMap, Error, errorReportExportFailed, nil, err)
		}

		if err := printErrors(e.iterate(Fatal|Error, AllComponents), print); err != nil {
			return newGeneralError(Core, failureMap, Error, errorReportExportFailed, nil, err)
		}
	}

	// print warnings in the error report
	if e.Count(Warning, AllComponents) > 0 {
		if _, err := print.Write(textWarnings); err != nil {
			return newGeneralError(Core, failureMap, Error, errorReportExportFailed, nil, err)
		}

		if err := printErrors(e.iterate(Warning, AllComponents), print); err != nil {
			return newGeneralError(Core, failureMap, Error, errorReportExportFailed, nil, err)
		}
	}

	// print remarks in the error report
	if e.Count(Remark, AllComponents) > 0 {
		if _, err := print.Write(textRemarks); err != nil {
			return newGeneralError(Core, failureMap, Error, errorReportExportFailed, nil, err)
		}

		if err := printErrors(e.iterate(Remark, AllComponents), print); err != nil {
			return newGeneralError(Core, failureMap, Error, errorReportExportFailed, nil, err)
		}
	}

	return nil
}

// Export the error report to the writer of the error handler.
func (e *errorHandler) Export(format ExportFormat, print io.Writer) error {
	switch format {
	case Json:

	case String:
		// print is a convenience function to export the error report as a string to the print writer
		return e.Print(print)

	case Binary:

	default:
		panic(newGeneralError(Core, failureMap, Fatal, unknownExportFormat, format, nil))
	}

	return nil
}

// Iterate over all errors in the error report of the error handler and return a channel of errors that match the severity and component.
func (e *errorHandler) iterate(severity Severity, component Component) <-chan error {
	errors := make(chan error)

	go func() {
		for _, err := range e.errorReport {
			switch err := err.(type) {
			case *generalError:
				if err.severity&severity != 0 && err.component&component != 0 {
					errors <- err
				}

			case *lineColumnError:
				if err.severity&severity != 0 && err.component&component != 0 {
					errors <- err
				}

			case *sourceError:
				if err.severity&severity != 0 && err.component&component != 0 {
					errors <- err
				}

			case *tokenError:
				if err.severity&severity != 0 && err.component&component != 0 {
					errors <- err
				}

			default:
				panic(newGeneralError(Core, failureMap, Fatal, errorKindNotSupported, reflect.TypeOf(err).Name(), err))
			}
		}

		close(errors)
	}()

	return errors
}

// Print all errors in the errors channel to the given writer.
func printErrors(errors <-chan error, print io.Writer) error {
	for e := range errors {
		if _, err := print.Write([]byte(e.Error())); err != nil {
			return err
		}
	}

	return nil
}
