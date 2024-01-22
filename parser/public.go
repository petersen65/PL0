// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

// Package parser implements the PL/0 parser that performs a syntactical analysis of the concrete syntax.
package parser

import (
	emt "github.com/petersen65/PL0/emitter"
	scn "github.com/petersen65/PL0/scanner"
)

// Core API for the PL/0 parser.
type (
	ErrorReport []Error

	Error struct {
		Err          error
		Line, Column int
		CurrentLine  []byte
	}

	Parser interface {
		Parse(concreteSyntax scn.ConcreteSyntax, emitter emt.Emitter) (ErrorReport, error)
	}
)

// Return the public interface of the private parser implementation.
func NewParser() Parser {
	return newParser()
}
