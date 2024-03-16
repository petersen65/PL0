// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package token

// The eof token is used to indicate the end of the token stream and is used only internally by the token handler.
const eof Token = -1

// Token handler manages the current and next token in the token stream.
type tokenHandler struct {
	tokenStreamIndex     int                // index of the current token in the token stream table
	tokenStream          TokenStream        // token stream to parse
	lastTokenDescription TokenDescription   // description of the last token that was read
	component            Component          // component of the compiler that is using the token handler
	errorMap             map[Failure]string // map of error codes to error messages
	errorHandler         ErrorHandler       // error handler that is used to handle errors that occured during parsing
}

// Create a new token handler for the PL/0 parser.
func newTokenHandler(tokenStream TokenStream, errorHandler ErrorHandler, component Component, errorMap map[Failure]string) TokenHandler {
	return &tokenHandler{
		tokenStream:  tokenStream,
		component:    component,
		errorMap:     errorMap,
		errorHandler: errorHandler,
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

// Check if the last token is an expected token and forward to an fallback set of tokens in the case of a syntax error.
func (t *tokenHandler) Rebase(code Failure, expected, fallback Tokens) bool {
	var hasError bool

	if !t.LastToken().In(expected) {
		hasError = true
		t.AppendError(t.NewError(code, t.LastTokenName()))

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

// Append an error to the error report of the token handler which is used to store all errors that occured during parsing.
func (t *tokenHandler) AppendError(err error) error {
	if err != nil {
		t.errorHandler.AppendError(err, t.tokenStreamIndex)
	}

	return err
}

// Create a new error by mapping the error code to its corresponding error message.
func (t *tokenHandler) NewError(code Failure, value any) error {
	line, column := t.lastTokenDescription.Line, t.lastTokenDescription.Column
	return NewFailure(t.component, t.errorMap, code, value, line, column)
}
