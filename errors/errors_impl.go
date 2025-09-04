// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package errors

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"reflect"
	"strings"

	exp "github.com/petersen65/pl0/v3/export"
)

// Separator for the string representation of a bit-mask.
const bitMaskSeparator = "|"

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

	// A component error provides a foundation for all errors that are associated with a specific component.
	componentError struct {
		Err       error     `json:"-"`         // error message
		Code      Failure   `json:"code"`      // failure code of the error
		Component Component `json:"component"` // component that generated the error
		Severity  Severity  `json:"severity"`  // severity of the error
	}

	// A general error with a severity level and an optional inner error.
	generalError struct {
		componentError       // embedded component error for general errors
		Inner          error `json:"-"` // inner error wrapped by the general error
		Indent         int32 `json:"-"` // indentation level of the error message
	}

	// An error with a severity level and a line and column number.
	lineColumnError struct {
		componentError     // embedded component error for line-column errors
		Line           int `json:"line"`   // line where the error occurred
		Column         int `json:"column"` // column where the error occurred
	}

	// An self-contained error that can stringify itself to a fully formatted multi-line text pointing to the source code where the error occurred.
	sourceError struct {
		componentError        // embedded component error for source errors
		Line           int    `json:"line"`   // line where the error occurred
		Column         int    `json:"column"` // column where the error occurred
		SourceCode     []byte `json:"-"`      // source code where the error occurred
	}

	// Implementation of the error handler.
	errorHandler struct {
		errorReport errorReport // list of errors that occurred during the compilation process
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
		Errors:                   "errors",
		Debugging:                "debugging",
		TypeSystem:               "typesystem",
		Token:                    "token",
		Scanner:                  "scanner",
		Parser:                   "parser",
		AbstractSyntaxTree:       "ast",
		Analyzer:                 "analyzer",
		Generator:                "generator",
		Intermediate:             "intermediate",
		ControlFlowGraph:         "cfg",
		Emitter:                  "emitter",
		Intel:                    "x86_64",
		ExecutableLinkableFormat: "elf",
	}
)

// Create a new error handler and initialize it with an empty error report.
func newErrorHandler() ErrorHandler {
	return &errorHandler{errorReport: make(errorReport, 0)}
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

// Create a new general error with a severity level (a general error can wrap any other error).
func newGeneralError(component Component, failureMap map[Failure]string, severity Severity, code Failure, value any, inner error) error {
	err := newGoError(failureMap, code, value)
	return &generalError{
		componentError: componentError{
			Err:       err,
			Code:      code,
			Component: component,
			Severity:  severity,
		},
		Inner: inner,
	}
}

// Create a new line-column error with a severity level and a line and column number.
func newLineColumnError(component Component, failureMap map[Failure]string, severity Severity, code Failure, value any, line, column int) error {
	err := newGoError(failureMap, code, value)
	return &lineColumnError{
		componentError: componentError{
			Err:       err,
			Code:      code,
			Component: component,
			Severity:  severity,
		},
		Line:   line,
		Column: column,
	}
}

// Create a new source error with a severity level, a line and column number, and the source code where the error occurred.
func newSourceError(component Component, failureMap map[Failure]string, severity Severity, code Failure, value any, line, column int, sourceCode []byte) error {
	err := newGoError(failureMap, code, value)
	return &sourceError{
		componentError: componentError{
			Err:       err,
			Code:      code,
			Component: component,
			Severity:  severity,
		},
		Line:       line,
		Column:     column,
		SourceCode: sourceCode,
	}
}

// String representation of a severity level bit-mask.
func (s Severity) String() string {
	var parts []string

	for severity, name := range severityMap {
		if s&severity != 0 {
			parts = append(parts, name)
		}
	}

	return strings.Join(parts, bitMaskSeparator)
}

// String representation of a component bit-mask.
func (c Component) String() string {
	var parts []string

	for component, name := range componentMap {
		if c&component != 0 {
			parts = append(parts, name)
		}
	}

	return strings.Join(parts, bitMaskSeparator)
}

// Check if the error has a specific severity level.
func (e *componentError) HasSeverity(severity Severity) bool {
	return e.Severity&severity != 0
}

// Check if the error comes from the specified component.
func (e *componentError) FromComponent(component Component) bool {
	return e.Component&component != 0
}

// Implement the error interface for the general error so that it can be used like a native Go error.
func (e *generalError) Error() string {
	var lineFeed string
	message := e.Err.Error()

	// if the general error is used as a part of an error report, the error message must have an indentation level and a line feed
	if e.Indent > 0 {
		lineFeed = "\n"
	}

	return fmt.Sprintf("%v%v %v %v: %v%v", strings.Repeat(" ", int(e.Indent)), e.Component, e.Severity, e.Code, message, lineFeed)
}

// Implement the Unwrap method for the general error so that it can be used to unwrap the inner error.
func (e *generalError) Unwrap() error {
	return e.Inner
}

// Marshal the general error to a JSON object because the JSON encoder does not support error interfaces directly.
func (e *generalError) MarshalJSON() ([]byte, error) {
	type embedded generalError

	// create a JSON-compliant general error structure that embeds the original error
	// note: replace the error interfaces with error message strings
	jsonCompliantError := &struct {
		ErrorMessage string `json:"error"`
		*embedded
		InnerMessage string `json:"inner_error"`
	}{
		ErrorMessage: e.Err.Error(),
		embedded:     (*embedded)(e),
		InnerMessage: e.Inner.Error(),
	}

	// marshal the general error as data type "embedded" to prevent recursion
	return json.Marshal(jsonCompliantError)
}

// Unmarshal the general error from a JSON object because the JSON decoder does not support error interfaces directly.
func (e *generalError) UnmarshalJSON(raw []byte) error {
	type embedded generalError

	// unmarshal the JSON object into a JSON-compliant general error structure
	jsonCompliantError := &struct {
		ErrorMessage string `json:"error"`
		*embedded
		InnerMessage string `json:"inner_error"`
	}{
		embedded: (*embedded)(e),
	}

	// unmarshal the genereal error as data type "embedded" to prevent recursion
	if err := json.Unmarshal(raw, jsonCompliantError); err != nil {
		return err
	}

	// replace the error message strings with error interfaces
	e.Err = errors.New(jsonCompliantError.ErrorMessage)
	e.Inner = errors.New(jsonCompliantError.InnerMessage)

	return nil
}

// Implement the error interface for the line-column error so that it can be used like a native Go error.
func (e *lineColumnError) Error() string {
	message := e.Err.Error()
	return fmt.Sprintf("%5v: %v %v %v [%v,%v]: %v\n", e.Line, e.Component, e.Severity, e.Code, e.Line, e.Column, message)
}

// Marshal the line column error to a JSON object because the JSON encoder does not support error interfaces directly.
func (e *lineColumnError) MarshalJSON() ([]byte, error) {
	type embedded lineColumnError

	// create a JSON-compliant line-column error structure that embeds the original error
	// note: replace the error interface with an error message string
	jsonCompliantError := &struct {
		ErrorMessage string `json:"error"`
		*embedded
	}{
		ErrorMessage: e.Err.Error(),
		embedded:     (*embedded)(e),
	}

	// marshal the line-column error as data type "embedded" to prevent recursion
	return json.Marshal(jsonCompliantError)
}

// Unmarshal the line column error from a JSON object because the JSON decoder does not support error interfaces directly.
func (e *lineColumnError) UnmarshalJSON(raw []byte) error {
	type embedded lineColumnError

	// unmarshal the JSON object into a JSON-compliant line-column error structure
	jsonCompliantError := &struct {
		ErrorMessage string `json:"error"`
		*embedded
	}{
		embedded: (*embedded)(e),
	}

	// unmarshal the line-column error as data type "embedded" to prevent recursion
	if err := json.Unmarshal(raw, jsonCompliantError); err != nil {
		return err
	}

	// replace the error message string with an error interface
	e.Err = errors.New(jsonCompliantError.ErrorMessage)

	return nil
}

// Implement the error interface for the source error so that it can be used like a native Go error.
func (e *sourceError) Error() string {
	message := e.Err.Error()
	message = fmt.Sprintf("%v %v %v [%v,%v]: %v", e.Component, e.Severity, e.Code, e.Line, e.Column, message)

	linePrefix := fmt.Sprintf("%5v: ", e.Line)
	trimmedLine := strings.TrimLeft(string(e.SourceCode), " \n\r")
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

// Marshal the source error to a JSON object because the JSON encoder does not support error interfaces and the "[]byte" type directly.
func (e *sourceError) MarshalJSON() ([]byte, error) {
	type embedded sourceError

	// create a JSON-compliant source error structure that embeds the original error
	// note: replace the error interface with an error message string and the byte slice of the source code with a string
	jsonCompliantError := &struct {
		ErrorMessage string `json:"error"`
		*embedded
		SourceCode string `json:"source_code"`
	}{
		ErrorMessage: e.Err.Error(),
		embedded:     (*embedded)(e),
		SourceCode:   string(e.SourceCode),
	}

	// marshal the source error as data type "embedded" to prevent recursion
	return json.Marshal(jsonCompliantError)
}

// Unmarshal the source error from a JSON object because the JSON decoder does not support error interfaces and the "[]byte" type directly.
func (e *sourceError) UnmarshalJSON(raw []byte) error {
	type embedded sourceError

	// unmarshal the JSON object into a JSON-compliant source error structure
	jsonCompliantError := &struct {
		ErrorMessage string `json:"error"`
		*embedded
		SourceCode string `json:"source_code"`
	}{
		embedded: (*embedded)(e),
	}

	// unmarshal the source error as data type "embedded" to prevent recursion
	if err := json.Unmarshal(raw, jsonCompliantError); err != nil {
		return err
	}

	// replace the error message string with an error interface and the string of the source code with a byte slice
	e.Err = errors.New(jsonCompliantError.ErrorMessage)
	e.SourceCode = []byte(jsonCompliantError.SourceCode)

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
		return newGeneralError(Errors, failureMap, Error, errorReportExportFailed, nil, err)
	}

	// print errors in the error report
	if e.Count(Fatal|Error, AllComponents) > 0 {
		if _, err := fmt.Fprintln(print, textErrors); err != nil {
			return newGeneralError(Errors, failureMap, Error, errorReportExportFailed, nil, err)
		}

		if err := printErrors(e.iterate(Fatal|Error, AllComponents), print); err != nil {
			return newGeneralError(Errors, failureMap, Error, errorReportExportFailed, nil, err)
		}
	}

	// print warnings in the error report
	if e.Count(Warning, AllComponents) > 0 {
		if _, err := fmt.Fprintln(print, textWarnings); err != nil {
			return newGeneralError(Errors, failureMap, Error, errorReportExportFailed, nil, err)
		}

		if err := printErrors(e.iterate(Warning, AllComponents), print); err != nil {
			return newGeneralError(Errors, failureMap, Error, errorReportExportFailed, nil, err)
		}
	}

	// print remarks in the error report
	if e.Count(Remark, AllComponents) > 0 {
		if _, err := fmt.Fprintln(print, textRemarks); err != nil {
			return newGeneralError(Errors, failureMap, Error, errorReportExportFailed, nil, err)
		}

		if err := printErrors(e.iterate(Remark, AllComponents), print); err != nil {
			return newGeneralError(Errors, failureMap, Error, errorReportExportFailed, nil, err)
		}
	}

	return nil
}

// Export the error report of the error handler (only Json and Text formats are supported).
func (e *errorHandler) Export(format exp.ExportFormat, print io.Writer) error {
	switch format {
	case exp.Json:
		if len(e.errorReport) == 0 {
			return nil
		}

		// export the error report as a JSON object and wrap it in a struct to provide a field name for the error report
		if raw, err := json.MarshalIndent(struct {
			Report errorReport `json:"error_report"`
		}{Report: e.errorReport}, "", "  "); err != nil {
			return newGeneralError(Errors, failureMap, Error, errorReportExportFailed, nil, err)
		} else {
			_, err = print.Write(raw)

			if err != nil {
				err = newGeneralError(Errors, failureMap, Error, errorReportExportFailed, nil, err)
			}

			return err
		}

	case exp.Text:
		// print is a convenience function to export the error report as a string to the print writer
		return e.Print(print)

	default:
		panic(newGeneralError(Errors, failureMap, Fatal, unknownExportFormat, format, nil))
	}
}

// Iterate over all errors in the error report of the error handler and return a channel of errors that match the severity and component.
func (e *errorHandler) iterate(severity Severity, component Component) <-chan error {
	errors := make(chan error)

	// anonymous goroutine to iterate over the error report and send matching errors to the channel
	go func() {
		for _, err := range e.errorReport {
			// only send errors that match the severity and component
			if err == nil {
				panic(newGeneralError(Errors, failureMap, Fatal, errorKindNotSupported, reflect.TypeOf(err).Name(), err))
			} else if ce, ok := err.(ComponentError); !ok {
				panic(newGeneralError(Errors, failureMap, Fatal, errorKindNotSupported, reflect.TypeOf(err).Name(), err))
			} else if ce.HasSeverity(severity) && ce.FromComponent(component) {
				errors <- err
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
