// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package token

import (
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

// Create a new token handler for the PL/0 parser.
func newTokenHandler(tokenStream TokenStream, errorHandler ErrorHandler, component Component, failureMap map[Failure]string) TokenHandler {
	return &tokenHandler{
		tokenStream:  tokenStream,
		component:    component,
		failureMap:   failureMap,
		errorHandler: errorHandler,
	}
}

// Print the token stream to the specified writer.
func (ts TokenStream) Print(print io.Writer, bottom bool) {
	if len(ts) == 0 {
		return
	}

	var start, previousLine int
	print.Write([]byte("Token Stream:"))

	if bottom {
		lastLine := ts[len(ts)-1].Line

		for start = len(ts) - 1; start >= 0 && ts[start].Line == lastLine; start-- {
		}

		start++
	} else {
		start = 0
	}

	for i := start; i < len(ts); i++ {
		td := ts[i]

		if td.Line != previousLine {
			print.Write([]byte(fmt.Sprintf("\n%v: %v\n", td.Line, strings.TrimLeft(string(td.CurrentLine), " \t\n\r"))))
			previousLine = td.Line
		}

		print.Write([]byte(fmt.Sprintf("%v,%-5v %v %v\n", td.Line, td.Column, td.TokenName, td.TokenValue)))
	}
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
