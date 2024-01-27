// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package parser

import scn "github.com/petersen65/PL0/scanner"

var (
	declarations = scn.Tokens{
		scn.ConstWord,
		scn.VarWord,
		scn.ProcedureWord,
	}

	statements = scn.Tokens{
		scn.Read,
		scn.Write,
		scn.BeginWord,
		scn.CallWord,
		scn.IfWord,
		scn.WhileWord,
	}

	factors = scn.Tokens{
		scn.Identifier,
		scn.Number,
		scn.LeftParenthesis,
	}
)

// Token handler manages the current and next token in the concrete syntax.
type tokenHandler struct {
	concreteSyntaxIndex       int                  // index of the current token in the concrete syntax
	concreteSyntax            scn.ConcreteSyntax   // concrete syntax to parse
	lastTokenDescription, eof scn.TokenDescription // description of the last token that was read
	errorReport               ErrorReport          // error report that stores all errors that occured during parsing
}

func newTokenHandler(concreteSyntax scn.ConcreteSyntax) *tokenHandler {
	return &tokenHandler{
		concreteSyntax: concreteSyntax,
		errorReport:    make(ErrorReport, 0),
	}
}

func set(tss ...scn.TokenSet) scn.Tokens {
	return scn.Set(tss...)
}

func (t *tokenHandler) nextTokenDescription() bool {
	if t.concreteSyntaxIndex >= len(t.concreteSyntax) {
		if t.eof.Token == scn.Null {
			t.eof = scn.TokenDescription{
				Token:       scn.Eof,
				TokenName:   "eof",
				TokenValue:  nil,
				TokenType:   scn.None,
				Line:        t.lastTokenDescription.Line,
				Column:      t.lastTokenDescription.Column,
				CurrentLine: t.lastTokenDescription.CurrentLine,
			}

			t.lastTokenDescription = t.eof
		}

		return false
	}

	t.lastTokenDescription = t.concreteSyntax[t.concreteSyntaxIndex]
	t.concreteSyntaxIndex++
	return true
}

func (t *tokenHandler) lastToken() scn.Token {
	return t.lastTokenDescription.Token
}

func (t *tokenHandler) lastTokenName() string {
	return t.lastTokenDescription.TokenName
}

func (t *tokenHandler) lastTokenValue() any {
	return t.lastTokenDescription.TokenValue
}

func (t *tokenHandler) rebase(code failure, expected, expanded scn.Tokens) {
	if !t.lastToken().In(expected) {
		t.appendError(t.error(code, t.lastTokenName()))

		for next := set(expected, expanded, scn.Eof); !t.lastToken().In(next); {
			t.nextTokenDescription()
		}
	}
}

func (t *tokenHandler) getErrorReport() ErrorReport {
	return t.errorReport
}
