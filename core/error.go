// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package core

import (
	"encoding/json"
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

// Text messages for printing an error report.
const (
	textErrorReport = "Error Report:"
	textErrors      = "Errors:"
	textWarnings    = "Warnings:"
	textRemarks     = "Remarks:"
)

type (
	// ErrorReport is a list of errors that occurred during the compilation process.
	errorReport []error

	// A general error with a severity level and an optional inner error.
	generalError struct {
		Err       error     `json:"-"`         // error message
		Code      Failure   `json:"code"`      // failure code of the error
		Component Component `json:"component"` // component that generated the error
		Severity  Severity  `json:"severity"`  // severity of the error
		Inner     error     `json:"-"`         // inner error wrapped by the general error
		Indent    int32     `json:"-"`         // indentation level of the error message
	}

	// An error with a severity level and a line and column number.
	lineColumnError struct {
		Err       error     `json:"-"`         // error message
		Code      Failure   `json:"code"`      // failure code of the error
		Component Component `json:"component"` // component that generated the error
		Severity  Severity  `json:"severity"`  // severity of the error
		Line      int32     `json:"line"`      // line where the error occurred
		Column    int32     `json:"column"`    // column where the error occurred
	}

	// An self-contained error that can stringify itself to a fully formatted multi-line text pointing to the source code where the error occurred.
	sourceError struct {
		Err        error     `json:"-"`         // error message
		Code       Failure   `json:"code"`      // failure code of the error
		Component  Component `json:"component"` // component that generated the error
		Severity   Severity  `json:"severity"`  // severity of the error
		Line       int32     `json:"line"`      // line where the error occurred
		Column     int32     `json:"column"`    // column where the error occurred
		SourceCode []byte    `json:"-"`         // source code where the error occurred
	}

	// An self-contained error that can stringify itself to a fully formatted multi-line text pointing to the source code where the error occurred.
	tokenError struct {
		Err              error       `json:"-"`                  // error message
		Code             Failure     `json:"code"`               // failure code of the error
		Component        Component   `json:"component"`          // component that generated the error
		Severity         Severity    `json:"severity"`           // severity of the error
		TokenStreamIndex int64       `json:"token_stream_index"` // index of the token in the token stream where the error occurred
		TokenStream      TokenStream `json:"-"`                  // token stream that is used to connect errors to a location in the source code
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
		Generator:          "generator",
		ControlFlowGraph:   "cfg",
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
	return &generalError{Err: err, Code: code, Component: component, Severity: severity, Inner: inner}
}

// Create a new line-column error with a severity level and a line and column number.
func newLineColumnError(component Component, failureMap map[Failure]string, severity Severity, code Failure, value any, line, column int32) error {
	err := newGoError(failureMap, code, value)
	return &lineColumnError{Err: err, Code: code, Component: component, Severity: severity, Line: line, Column: column}
}

// Create a new source error with a severity level, a line and column number, and the source code where the error occurred.
func newSourceError(component Component, failureMap map[Failure]string, severity Severity, code Failure, value any, line, column int32, sourceCode []byte) error {
	err := newGoError(failureMap, code, value)
	return &sourceError{Err: err, Code: code, Component: component, Severity: severity, Line: line, Column: column, SourceCode: sourceCode}
}

// Create a new token error with a severity level and a token stream that is used to connect errors to a location in the source code.
func newTokenError(component Component, failureMap map[Failure]string, severity Severity, code Failure, value any, tokenStream TokenStream, index int) error {
	err := newGoError(failureMap, code, value)
	return &tokenError{Err: err, Code: code, Component: component, Severity: severity, TokenStreamIndex: int64(index), TokenStream: tokenStream}
}

// Implement the error interface for the general error so that it can be used like a native Go error.
func (e *generalError) Error() string {
	var lineFeed string
	message := e.Err.Error()

	// if the general error is used as a part of an error report, the error message must have an indentation level and a line feed
	if e.Indent > 0 {
		lineFeed = "\n"
	}

	return fmt.Sprintf("%v%v %v %v: %v%v", strings.Repeat(" ", int(e.Indent)), componentMap[e.Component], severityMap[e.Severity], e.Code, message, lineFeed)
}

// Implement the Unwrap method for the general error so that it can be used to unwrap the inner error.
func (e *generalError) Unwrap() error {
	return e.Inner
}

// Marshal the general error to a JSON object.
func (e *generalError) MarshalJSON() ([]byte, error) {
	type Embedded generalError

	// replace the error interfaces with error message strings
	ej := &struct {
		ErrorMessage string `json:"error"`
		*Embedded
		InnerMessage string `json:"inner_error"`
	}{
		ErrorMessage: e.Err.Error(),
		Embedded:     (*Embedded)(e),
		InnerMessage: e.Inner.Error(),
	}

	return json.Marshal(ej)
}

// Unmarshal the general error from a JSON object.
func (e *generalError) UnmarshalJSON(raw []byte) error {
	type Embedded generalError

	// struct to unmarshal the JSON object to
	ej := &struct {
		ErrorMessage string `json:"error"`
		*Embedded
		InnerMessage string `json:"inner_error"`
	}{
		Embedded: (*Embedded)(e),
	}

	if err := json.Unmarshal(raw, ej); err != nil {
		return err
	}

	// replace the error message strings with error interfaces
	e.Err = errors.New(ej.ErrorMessage)
	e.Code = ej.Code
	e.Component = ej.Component
	e.Severity = ej.Severity
	e.Inner = errors.New(ej.InnerMessage)

	return nil
}

// Implement the error interface for the line-column error so that it can be used like a native Go error.
func (e *lineColumnError) Error() string {
	message := e.Err.Error()
	return fmt.Sprintf("%5v: %v %v %v [%v,%v]: %v\n", e.Line, componentMap[e.Component], severityMap[e.Severity], e.Code, e.Line, e.Column, message)
}

// Marshal the line column error to a JSON object.
func (e *lineColumnError) MarshalJSON() ([]byte, error) {
	type Embedded lineColumnError

	// replace the error interface with an error message string
	ej := &struct {
		ErrorMessage string `json:"error"`
		*Embedded
	}{
		ErrorMessage: e.Err.Error(),
		Embedded:     (*Embedded)(e),
	}

	return json.Marshal(ej)
}

// Unmarshal the line column error from a JSON object.
func (e *lineColumnError) UnmarshalJSON(raw []byte) error {
	type Embedded lineColumnError

	// struct to unmarshal the JSON object to
	ej := &struct {
		ErrorMessage string `json:"error"`
		*Embedded
	}{
		Embedded: (*Embedded)(e),
	}

	if err := json.Unmarshal(raw, ej); err != nil {
		return err
	}

	// replace the error message string with an error interface
	e.Err = errors.New(ej.ErrorMessage)
	e.Code = ej.Code
	e.Component = ej.Component
	e.Severity = ej.Severity
	e.Line = ej.Line
	e.Column = ej.Column

	return nil
}

// Implement the error interface for the source error so that it can be used like a native Go error.
func (e *sourceError) Error() string {
	message := e.Err.Error()
	message = fmt.Sprintf("%v %v %v [%v,%v]: %v", componentMap[e.Component], severityMap[e.Severity], e.Code, e.Line, e.Column, message)

	linePrefix := fmt.Sprintf("%5v: ", e.Line)
	trimmedLine := strings.TrimLeft(string(e.SourceCode), " \t\n\r")
	trimmedLen := len(string(e.SourceCode)) - len(trimmedLine)
	indentionLen := len(linePrefix) + int(e.Column) - trimmedLen - 1 // valid column numbers are greater than 'trimmedLen'

	// corner cases for errors that have a non-valid column number
	if indentionLen < 0 {
		indentionLen = len(linePrefix)
	}

	sourceLine := fmt.Sprintf("%v%v\n", linePrefix, trimmedLine)
	errorLine := fmt.Sprintf("%v^ %v\n", strings.Repeat(" ", indentionLen), message)
	return sourceLine + errorLine
}

// Marshal the source error to a JSON object.
func (e *sourceError) MarshalJSON() ([]byte, error) {
	type Embedded sourceError

	// replace the error interface with an error message string and the source code byte slice with a string of the source code
	ej := &struct {
		ErrorMessage string `json:"error"`
		*Embedded
		SourceCode string `json:"source_code"`
	}{
		ErrorMessage: e.Err.Error(),
		Embedded:     (*Embedded)(e),
		SourceCode:   string(e.SourceCode),
	}

	return json.Marshal(ej)
}

// Unmarshal the source error from a JSON object.
func (e *sourceError) UnmarshalJSON(raw []byte) error {
	type Embedded sourceError

	// struct to unmarshal the JSON object to
	ej := &struct {
		ErrorMessage string `json:"error"`
		*Embedded
		SourceCode string `json:"source_code"`
	}{
		Embedded: (*Embedded)(e),
	}

	if err := json.Unmarshal(raw, ej); err != nil {
		return err
	}

	// replace the error message string with an error interface and the string of the source code with a byte slice
	e.Err = errors.New(ej.ErrorMessage)
	e.Code = ej.Code
	e.Component = ej.Component
	e.Severity = ej.Severity
	e.Line = ej.Line
	e.Column = ej.Column
	e.SourceCode = []byte(ej.SourceCode)

	return nil
}

// Implement the error interface for the token error so that it can be used like a native Go error.
func (e *tokenError) Error() string {
	td := e.TokenStream[e.TokenStreamIndex]
	message := e.Err.Error()
	message = fmt.Sprintf("%v %v %v [%v,%v]: %v", componentMap[e.Component], severityMap[e.Severity], e.Code, td.Line, td.Column, message)

	linePrefix := fmt.Sprintf("%5v: ", td.Line)
	trimmedLine := strings.TrimLeft(string(td.CurrentLine), " \t\n\r")
	trimmedLen := len(string(td.CurrentLine)) - len(trimmedLine)
	indentionLen := len(linePrefix) + int(td.Column) - trimmedLen - 1 // valid column numbers are greater than 'trimmedLen'

	// corner cases for errors that have a non-valid column number
	if indentionLen < 0 {
		indentionLen = len(linePrefix)
	}

	sourceLine := fmt.Sprintf("%v%v\n", linePrefix, trimmedLine)
	errorLine := fmt.Sprintf("%v^ %v\n", strings.Repeat(" ", indentionLen), message)
	return sourceLine + errorLine
}

// Marshal the token error to a JSON object.
func (e *tokenError) MarshalJSON() ([]byte, error) {
	type Embedded tokenError

	// replace the error interface with an error message string
	ej := &struct {
		ErrorMessage string `json:"error"`
		*Embedded
		TokenDescription TokenDescription `json:"token_description"`
	}{
		ErrorMessage:     e.Err.Error(),
		Embedded:         (*Embedded)(e),
		TokenDescription: e.TokenStream[e.TokenStreamIndex],
	}

	return json.Marshal(ej)
}

// Unmarshal the token error from a JSON object.
func (e *tokenError) UnmarshalJSON(raw []byte) error {
	type Embedded tokenError

	// struct to unmarshal the JSON object to
	ej := &struct {
		ErrorMessage string `json:"error"`
		*Embedded
		TokenDescription TokenDescription `json:"token_description"`
	}{
		Embedded: (*Embedded)(e),
	}

	if err := json.Unmarshal(raw, ej); err != nil {
		return err
	}

	// replace the error message string with an error interface
	e.Err = errors.New(ej.ErrorMessage)
	e.Code = ej.Code
	e.Component = ej.Component
	e.Severity = ej.Severity
	e.TokenStreamIndex = ej.TokenStreamIndex
	e.TokenStream = []TokenDescription{ej.TokenDescription}

	return nil
}

// Append a new error to the error report of the error handler only if the error is not nil.
func (e *errorHandler) AppendError(err error) error {
	if err != nil {
		// the general error needs an explicit indentation level to format the error message correctly with other errors in the error report
		if ge, ok := err.(*generalError); ok {
			ge.Indent = 7 // equals to the length of the error line prefix which is formatted with "%5v: "
		}

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

// Return whether the error handler has fatal or error entries in the error report.
func (e *errorHandler) HasErrors() bool {
	return e.Count(Fatal|Error, AllComponents) > 0
}

// Return whether the error handler has warning entries in the error report.
func (e *errorHandler) HasWarnings() bool {
	return e.Count(Warning, AllComponents) > 0
}

// Return whether the error handler has remark entries in the error report.
func (e *errorHandler) HasRemarks() bool {
	return e.Count(Remark, AllComponents) > 0
}

// Print the error report to the writer of the error handler.
func (e *errorHandler) Print(print io.Writer, args ...any) error {
	if len(e.errorReport) == 0 {
		return nil
	}

	// print the title text message for the error report
	if _, err := fmt.Fprintln(print, textErrorReport); err != nil {
		return newGeneralError(Core, failureMap, Error, errorReportExportFailed, nil, err)
	}

	// print errors in the error report
	if e.Count(Fatal|Error, AllComponents) > 0 {
		if _, err := fmt.Fprintln(print, textErrors); err != nil {
			return newGeneralError(Core, failureMap, Error, errorReportExportFailed, nil, err)
		}

		if err := printErrors(e.iterate(Fatal|Error, AllComponents), print); err != nil {
			return newGeneralError(Core, failureMap, Error, errorReportExportFailed, nil, err)
		}
	}

	// print warnings in the error report
	if e.Count(Warning, AllComponents) > 0 {
		if _, err := fmt.Fprintln(print, textWarnings); err != nil {
			return newGeneralError(Core, failureMap, Error, errorReportExportFailed, nil, err)
		}

		if err := printErrors(e.iterate(Warning, AllComponents), print); err != nil {
			return newGeneralError(Core, failureMap, Error, errorReportExportFailed, nil, err)
		}
	}

	// print remarks in the error report
	if e.Count(Remark, AllComponents) > 0 {
		if _, err := fmt.Fprintln(print, textRemarks); err != nil {
			return newGeneralError(Core, failureMap, Error, errorReportExportFailed, nil, err)
		}

		if err := printErrors(e.iterate(Remark, AllComponents), print); err != nil {
			return newGeneralError(Core, failureMap, Error, errorReportExportFailed, nil, err)
		}
	}

	return nil
}

// Export the error report of the error handler (only Json and Text formats are supported).
func (e *errorHandler) Export(format ExportFormat, print io.Writer) error {
	switch format {
	case Json:
		if len(e.errorReport) == 0 {
			return nil
		}

		// export the error report as a JSON object and wrap it in a struct to provide a field name for the error report
		if raw, err := json.MarshalIndent(struct {
			Report errorReport `json:"error_report"`
		}{Report: e.errorReport}, "", "  "); err != nil {
			return newGeneralError(Core, failureMap, Error, errorReportExportFailed, nil, err)
		} else {
			_, err = print.Write(raw)

			if err != nil {
				err = newGeneralError(Core, failureMap, Error, errorReportExportFailed, nil, err)
			}

			return err
		}

	case Text:
		// print is a convenience function to export the error report as a string to the print writer
		return e.Print(print)

	default:
		panic(newGeneralError(Core, failureMap, Fatal, unknownExportFormat, format, nil))
	}
}

// Iterate over all errors in the error report of the error handler and return a channel of errors that match the severity and component.
func (e *errorHandler) iterate(severity Severity, component Component) <-chan error {
	errors := make(chan error)

	go func() {
		for _, err := range e.errorReport {
			switch err := err.(type) {
			case *generalError:
				if err.Severity&severity != 0 && err.Component&component != 0 {
					errors <- err
				}

			case *lineColumnError:
				if err.Severity&severity != 0 && err.Component&component != 0 {
					errors <- err
				}

			case *sourceError:
				if err.Severity&severity != 0 && err.Component&component != 0 {
					errors <- err
				}

			case *tokenError:
				if err.Severity&severity != 0 && err.Component&component != 0 {
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
		if _, err := fmt.Fprint(print, e.Error()); err != nil {
			return err
		}
	}

	return nil
}
