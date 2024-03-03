// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package parser

import scn "github.com/petersen65/PL0/scanner"

// The eof token is used to indicate the end of the token stream and is used only internally by the token handler.
const eof scn.Token = -1

var (
	// Tokens that are used to begin constants, variables, and procedures declarations.
	declarations = scn.Tokens{
		scn.ConstWord,
		scn.VarWord,
		scn.ProcedureWord,
	}

	// Tokens that are used to begin statements within a block.
	statements = scn.Tokens{
		scn.Read,
		scn.Write,
		scn.BeginWord,
		scn.CallWord,
		scn.IfWord,
		scn.WhileWord,
	}

	// Tokens that are used to begin factors in expressions.
	factors = scn.Tokens{
		scn.Identifier,
		scn.Number,
		scn.LeftParenthesis,
	}
)

// Token handler manages the current and next token in the token stream.
type tokenHandler struct {
	tokenStreamIndex     int                  // index of the current token in the token stream table
	tokenStream          scn.TokenStream      // token stream to parse
	lastTokenDescription scn.TokenDescription // description of the last token that was read
	errorReport          ErrorReport          // error report that stores all errors that occured during parsing
}

// Create a new token handler for the PL/0 parser.
func newTokenHandler(tokenStream scn.TokenStream) *tokenHandler {
	return &tokenHandler{
		tokenStream: tokenStream,
		errorReport: make(ErrorReport, 0),
	}
}

// Set wrapper returns a joined slice of all tokens within the given TokenSet interfaces. Redundant tokens are removed.
func set(tss ...scn.TokenSet) scn.Tokens {
	return scn.Set(tss...)
}

// Set next token description in the token stream or an eof description.
func (t *tokenHandler) nextTokenDescription() bool {
	if t.tokenStreamIndex >= len(t.tokenStream) {
		if t.lastTokenDescription.Token != eof {
			t.lastTokenDescription = scn.TokenDescription{
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
func (t *tokenHandler) lastToken() scn.Token {
	return t.lastTokenDescription.Token
}

// Get token name from the last token description.
func (t *tokenHandler) lastTokenName() string {
	return t.lastTokenDescription.TokenName
}

// Get token value from the last token description.
func (t *tokenHandler) lastTokenValue() string {
	return t.lastTokenDescription.TokenValue
}

// Check if the last token is an expected token and forward to an fallback set of tokens in the case of a syntax error.
func (t *tokenHandler) rebase(code failure, expected, fallback scn.Tokens) {
	if !t.lastToken().In(expected) {
		t.appendError(t.error(code, t.lastTokenName()))

		for next := set(expected, fallback, eof); !t.lastToken().In(next); {
			t.nextTokenDescription()
		}
	}
}

// The token stream is fully parsed if the index of the current token is equal to the length of the token stream table.
func (t *tokenHandler) isFullyParsed() bool {
	return t.tokenStreamIndex == len(t.tokenStream)
}

// Set index of the current token to the last entry of the token stream table and update next token description.
func (t *tokenHandler) setFullyParsed() {
	t.tokenStreamIndex = len(t.tokenStream) - 1
	t.lastTokenDescription = t.tokenStream[t.tokenStreamIndex]
	t.tokenStreamIndex++
}

// Expose error report to the parser.
func (t *tokenHandler) getErrorReport() ErrorReport {
	return t.errorReport
}
