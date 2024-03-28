// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package core

import (
	"bytes"
	"encoding/gob"
	"encoding/json"
	"fmt"
	"io"
	"strings"
)

// The eof token is used to indicate the end of the token stream and is used only internally by the token handler.
const eof Token = -1

// Token handler manages the current and next token in the token stream.
type tokenHandler struct {
	tokenStreamIndex     int                // index of the current token in the token stream table
	tokenStream          TokenStream        // token stream to parse
	lastTokenDescription TokenDescription   // description of the last token that was read
	component            Component          // component of the compiler that is using the token handler
	failureMap           map[Failure]string // map failure codes to error messages
	errorHandler         ErrorHandler       // error handler that is used to handle errors that occured during parsing
}

// Text messages for printing the token stream.
var textTokenStream = []byte("Token Stream:")

// Create a new token handler for the PL/0 parser.
func newTokenHandler(tokenStream TokenStream, errorHandler ErrorHandler, component Component, failureMap map[Failure]string) TokenHandler {
	return &tokenHandler{
		tokenStream:  tokenStream,
		component:    component,
		failureMap:   failureMap,
		errorHandler: errorHandler,
	}
}

// Marshal the token description to a JSON object.
func (td *TokenDescription) MarshalJSON() ([]byte, error) {
	type Embedded TokenDescription

	// replace the current line byte slice with a string of the current line
	tdj := &struct {
		Embedded
		CurrentLine string `json:"current_line"`
	}{
		Embedded:    (Embedded)(*td),
		CurrentLine: string(td.CurrentLine),
	}

	return json.Marshal(tdj)
}

// Unmarshal the token description from a JSON object.
func (td *TokenDescription) UnmarshalJSON(raw []byte) error {
	type Embedded TokenDescription

	// target struct to unmarshal the JSON object to
	tdj := &struct {
		Embedded
		CurrentLine string `json:"current_line"`
	}{
		Embedded: (Embedded)(*td),
	}

	if err := json.Unmarshal(raw, tdj); err != nil {
		return err
	}

	// replace the string of the current line with a byte slice
	td.Token = tdj.Token
	td.TokenName = tdj.TokenName
	td.TokenValue = tdj.TokenValue
	td.Line = tdj.Line
	td.Column = tdj.Column
	td.CurrentLine = []byte(tdj.CurrentLine)

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
	var previousLine int32

	// print the title text message for the token stream
	if _, err := print.Write(textTokenStream); err != nil {
		return newGeneralError(Core, failureMap, Error, tokenStreamExportFailed, nil, err)
	}

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
			if _, err := print.Write([]byte(fmt.Sprintf("\n%v: %v\n", td.Line, strings.TrimLeft(string(td.CurrentLine), " \t\n\r")))); err != nil {
				return newGeneralError(Core, failureMap, Error, tokenStreamExportFailed, nil, err)
			}

			previousLine = td.Line
		}

		// print token description below the line number and line content
		if _, err := print.Write([]byte(fmt.Sprintf("%v,%-5v %v %v\n", td.Line, td.Column, td.TokenName, td.TokenValue))); err != nil {
			return newGeneralError(Core, failureMap, Error, tokenStreamExportFailed, nil, err)
		}
	}

	return nil
}

// Export the token stream to a writer in the specified format.
func (ts TokenStream) Export(format ExportFormat, print io.Writer) error {
	switch format {
	case Json:
		// export the token stream as a JSON object and wrap it in a struct to provide a field name for the token stream
		if raw, err := json.MarshalIndent(struct {
			Stream TokenStream `json:"token_stream"`
		}{Stream: ts}, "", "  "); err != nil {
			return newGeneralError(Core, failureMap, Error, tokenStreamExportFailed, nil, err)
		} else {
			_, err = print.Write(raw)

			if err != nil {
				err = newGeneralError(Core, failureMap, Error, tokenStreamExportFailed, nil, err)
			}

			return err
		}

	case Text:
		// print is a convenience function to export the token stream as a string to the print writer
		return ts.Print(print)

	case Binary:
		var buffer bytes.Buffer

		// encode the raw bytes of the token stream into a binary buffer
		if err := gob.NewEncoder(&buffer).Encode(ts); err != nil {
			return newGeneralError(Core, failureMap, Error, tokenStreamExportFailed, nil, err)
		}

		// transfer the binary buffer to the print writer
		if _, err := buffer.WriteTo(print); err != nil {
			return newGeneralError(Core, failureMap, Error, tokenStreamExportFailed, nil, err)
		}

	default:
		panic(newGeneralError(Core, failureMap, Fatal, unknownExportFormat, format, nil))
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
func (t *tokenHandler) Rebase(code Failure, expected, fallback Tokens) bool {
	var hasError bool

	if !t.LastToken().In(expected) {
		hasError = true
		t.AppendError(t.NewError(Error, code, t.LastTokenName()))

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

// Create a new error by mapping the error code to its corresponding error message.
func (t *tokenHandler) NewError(severity Severity, code Failure, value any) error {
	return NewTokenError(t.component, t.failureMap, severity, code, value, t.tokenStream, t.tokenStreamIndex-1)
}

// Create a new error by mapping the error code to its corresponding error message and provide a token stream index for the error location.
func (t *tokenHandler) NewErrorOnIndex(severity Severity, code Failure, value any, index int) error {
	return NewTokenError(t.component, t.failureMap, severity, code, value, t.tokenStream, index)
}

// Append an error to the error report of the underlying error handler which is used to store all errors that occured during parsing.
func (t *tokenHandler) AppendError(err error) error {
	// nil errors are not appended to the error report
	return t.errorHandler.AppendError(err)
}

// Replace the component and the failure map with new values to enable a chain of components that can append errors.
func (t *tokenHandler) ReplaceComponent(component Component, failureMap map[Failure]string) {
	t.component = component
	t.failureMap = failureMap
}
