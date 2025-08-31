// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package token

import (
	"bytes"
	"encoding/gob"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"strings"

	eh "github.com/petersen65/pl0/v3/errors"
	exp "github.com/petersen65/pl0/v3/export"
)

// The eof token is used to indicate the end of the token stream.
const eof Token = -1

// The eof token description is used to indicate a token that is out of bounds in the token stream.
var eofTokenDescription = TokenDescription{
	Token:       eof,
	TokenName:   "eof",
	CurrentLine: make([]byte, 0),
}

type (
	// Token handler manages the current and next token in the token stream.
	tokenHandler struct {
		tokenStreamIndex     int                   // index of the current token in the token stream table
		tokenStream          TokenStream           // token stream to parse
		lastTokenDescription TokenDescription      // description of the last token that was read
		component            eh.Component          // component of the compiler that is using the token handler
		failureMap           map[eh.Failure]string // map failure codes to error messages
		errorHandler         eh.ErrorHandler       // error handler that is used to handle errors that occured during parsing
	}

	// An self-contained error that can stringify itself to a fully formatted multi-line text pointing to the source code where the error occurred.
	tokenError struct {
		Err              error        `json:"-"`                  // error message
		Code             eh.Failure   `json:"code"`               // failure code of the error
		Component        eh.Component `json:"component"`          // component that generated the error
		Severity         eh.Severity  `json:"severity"`           // severity of the error
		TokenStreamIndex int64        `json:"token_stream_index"` // index of the token in the token stream where the error occurred
		TokenStream      TokenStream  `json:"-"`                  // token stream that is used to connect errors to a location in the source code
	}
)

// Create a new token handler for the compiler.
func newTokenHandler(tokenStream TokenStream, errorHandler eh.ErrorHandler, component eh.Component, failureMap map[eh.Failure]string) TokenHandler {
	return &tokenHandler{
		tokenStream:  tokenStream,
		component:    component,
		failureMap:   failureMap,
		errorHandler: errorHandler,
	}
}

// Create a new token error with a severity level and a token stream that is used to connect errors to a location in the source code.
func newTokenError(component eh.Component, failureMap map[eh.Failure]string, severity eh.Severity, code eh.Failure, value any, tokenStream TokenStream, index int) error {
	err := eh.NewGoError(failureMap, code, value)
	return &tokenError{Err: err, Code: code, Component: component, Severity: severity, TokenStreamIndex: int64(index), TokenStream: tokenStream}
}

// Marshal the token description to a JSON object because the JSON encoder does not support the "[]byte" type directly.
func (td *TokenDescription) MarshalJSON() ([]byte, error) {
	type embedded TokenDescription

	// create a JSON-compliant token description structure that embeds the original description
	// note: replace the byte slice of the current line with an UTF-8 encoded string
	jsonCompliantTokenDescription := &struct {
		*embedded
		CurrentLine string `json:"current_line"`
	}{
		embedded:    (*embedded)(td),
		CurrentLine: string(td.CurrentLine),
	}

	// marshal the token description as data type "embedded" to prevent recursion
	return json.Marshal(jsonCompliantTokenDescription)
}

// Unmarshal the token description from a JSON object because the JSON decoder does not support the "[]byte" type directly.
func (td *TokenDescription) UnmarshalJSON(raw []byte) error {
	type embedded TokenDescription

	// unmarshal the JSON object into a JSON-compliant token description structure
	jsonCompliantTokenDescription := &struct {
		*embedded
		CurrentLine string `json:"current_line"`
	}{
		embedded: (*embedded)(td),
	}

	// unmarshal the token description as data type "embedded" to prevent recursion
	if err := json.Unmarshal(raw, jsonCompliantTokenDescription); err != nil {
		return err
	}

	// replace the UTF-8 encoded string with a byte slice of the current line
	td.CurrentLine = []byte(jsonCompliantTokenDescription.CurrentLine)

	return nil
}

// Print the token stream to a writer and print from the bottom of the token stream if the bottom flag is set.
func (ts TokenStream) Print(print io.Writer, args ...any) error {
	if len(ts) == 0 {
		return nil
	}

	// calculate the start index of the token stream depending on the bottom flag
	var bottom bool
	var start int
	var previousLine int

	// check if the bottom flag is set
	if len(args) == 1 {
		if b, ok := args[0].(bool); ok {
			bottom = b
		}
	}

	// print from the bottom of the token stream if the bottom flag is set
	if bottom {
		lastLine := ts[len(ts)-1].Line

		for start = len(ts) - 1; start >= 0 && ts[start].Line == lastLine; start-- {
		}

		start++
	}

	// print from the start of the token stream depending on the bottom flag and the start index
	for i := start; i < len(ts); i++ {
		td := ts[i]

		// print line number and line content if the line number is a new line
		if td.Line != previousLine {
			if previousLine != 0 {
				if _, err := fmt.Fprintln(print); err != nil {
					return eh.NewGeneralError(eh.Token, failureMap, eh.Error, tokenStreamExportFailed, nil, err)
				}
			}

			if _, err := fmt.Fprintf(print, "%v: %v\n", td.Line, strings.TrimLeft(string(td.CurrentLine), " \n\r")); err != nil {
				return eh.NewGeneralError(eh.Token, failureMap, eh.Error, tokenStreamExportFailed, nil, err)
			}

			previousLine = td.Line
		}

		// print token description below the line number and line content
		if _, err := fmt.Fprintf(print, "%v,%-5v %v %v\n", td.Line, td.Column, td.TokenName, td.TokenValue); err != nil {
			return eh.NewGeneralError(eh.Token, failureMap, eh.Error, tokenStreamExportFailed, nil, err)
		}
	}

	return nil
}

// Export the token stream to a writer in the specified format.
func (ts TokenStream) Export(format exp.ExportFormat, print io.Writer) error {
	switch format {
	case exp.Json:
		// export the token stream as a JSON object and wrap it in a struct to provide a field name for the token stream
		if raw, err := json.MarshalIndent(struct {
			Stream TokenStream `json:"token_stream"`
		}{Stream: ts}, "", "  "); err != nil {
			return eh.NewGeneralError(eh.Token, failureMap, eh.Error, tokenStreamExportFailed, nil, err)
		} else {
			_, err = print.Write(raw)

			if err != nil {
				err = eh.NewGeneralError(eh.Token, failureMap, eh.Error, tokenStreamExportFailed, nil, err)
			}

			return err
		}

	case exp.Text:
		// print is a convenience function to export the token stream as a string to the print writer
		return ts.Print(print)

	case exp.Binary:
		var buffer bytes.Buffer

		// encode the raw bytes of the token stream into a binary buffer
		if err := gob.NewEncoder(&buffer).Encode(ts); err != nil {
			return eh.NewGeneralError(eh.Token, failureMap, eh.Error, tokenStreamExportFailed, nil, err)
		}

		// transfer the binary buffer to the print writer
		if _, err := buffer.WriteTo(print); err != nil {
			return eh.NewGeneralError(eh.Token, failureMap, eh.Error, tokenStreamExportFailed, nil, err)
		}

	default:
		panic(eh.NewGeneralError(eh.Token, failureMap, eh.Fatal, unknownExportFormat, format, nil))
	}

	return nil
}

// Set next token description in the token stream or an eof description.
func (t *tokenHandler) NextTokenDescription() bool {
	if t.tokenStreamIndex >= len(t.tokenStream) {
		if t.lastTokenDescription.Token != eof {
			t.lastTokenDescription = TokenDescription{
				Token:       eof,
				TokenName:   "eof",
				Line:        t.lastTokenDescription.Line,
				Column:      t.lastTokenDescription.Column,
				CurrentLine: t.lastTokenDescription.CurrentLine,
			}
		}

		return false
	}

	t.lastTokenDescription = t.tokenStream[t.tokenStreamIndex]
	t.tokenStreamIndex++
	return true
}

// Get token from the last token description.
func (t *tokenHandler) LastToken() Token {
	return t.lastTokenDescription.Token
}

// Get token name from the last token description.
func (t *tokenHandler) LastTokenName() string {
	return t.lastTokenDescription.TokenName
}

// Get token value from the last token description.
func (t *tokenHandler) LastTokenValue() string {
	return t.lastTokenDescription.TokenValue
}

// Get index of the last token in the token stream.
func (t *tokenHandler) LastTokenIndex() int {
	return t.tokenStreamIndex - 1
}

// Check if the last token is an expected token and forward to an fallback set of tokens in the case of a syntax error.
func (t *tokenHandler) Recover(code eh.Failure, expected, fallback Tokens) bool {
	var hasError bool

	if !t.LastToken().In(expected) {
		hasError = true
		t.AppendError(t.NewError(eh.Error, code, t.LastTokenName()))

		for next := Set(expected, fallback, eof); !t.LastToken().In(next); {
			t.NextTokenDescription()
		}
	}

	// the caller can check whether an error was appended to the error report
	return hasError
}

// The token stream is fully parsed if the index of the current token is equal to the length of the token stream table.
func (t *tokenHandler) IsFullyParsed() bool {
	return t.tokenStreamIndex == len(t.tokenStream)
}

// Set index of the current token to the last entry of the token stream table and update next token description.
func (t *tokenHandler) SetFullyParsed() {
	t.tokenStreamIndex = len(t.tokenStream) - 1
	t.lastTokenDescription = t.tokenStream[t.tokenStreamIndex]
	t.tokenStreamIndex++
}

// Get the token description for a specific index in the token stream.
func (t *tokenHandler) GetTokenDescription(tokenStreamIndex int) (TokenDescription, bool) {
	if tokenStreamIndex < 0 || tokenStreamIndex >= len(t.tokenStream) {
		return eofTokenDescription, false
	}

	return t.tokenStream[tokenStreamIndex], true
}

// Create a new error by mapping the error code to its corresponding error message.
func (t *tokenHandler) NewError(severity eh.Severity, code eh.Failure, value any) error {
	return newTokenError(t.component, t.failureMap, severity, code, value, t.tokenStream, t.tokenStreamIndex-1)
}

// Create a new error by mapping the error code to its corresponding error message and provide a token stream index for the error location.
func (t *tokenHandler) NewErrorOnIndex(severity eh.Severity, code eh.Failure, value any, index int) error {
	return newTokenError(t.component, t.failureMap, severity, code, value, t.tokenStream, index)
}

// Append an error to the error report of the underlying error handler which is used to store all errors that occured during parsing.
func (t *tokenHandler) AppendError(err error) error {
	// nil errors are not appended to the error report
	return t.errorHandler.AppendError(err)
}

// Replace the component and the failure map with new values to enable a chain of components that can append errors.
func (t *tokenHandler) ReplaceComponent(component eh.Component, failureMap map[eh.Failure]string) {
	t.component = component
	t.failureMap = failureMap
}

// Implement the error interface for the token error so that it can be used like a native Go error.
func (e *tokenError) Error() string {
	td := e.TokenStream[e.TokenStreamIndex]
	message := e.Err.Error()
	message = fmt.Sprintf("%v %v %v [%v,%v]: %v", e.Component, e.Severity, e.Code, td.Line, td.Column, message)

	linePrefix := fmt.Sprintf("%5v: ", td.Line)
	trimmedLine := strings.TrimLeft(string(td.CurrentLine), " \n\r")
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

// Check if the token error has a specific severity level.
func (e *tokenError) HasSeverity(severity eh.Severity) bool {
	return e.Severity&severity != 0
}

// Check if the token error comes from a specific component.
func (e *tokenError) FromComponent(component eh.Component) bool {
	return e.Component&component != 0
}
