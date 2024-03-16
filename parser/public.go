// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

// Package parser implements the PL/0 parser that performs a syntactical analysis of the token stream.
package parser

import (
	ast "github.com/petersen65/PL0/ast"
	tok "github.com/petersen65/PL0/token"
)

// Name of the entry point procedure of a program.
const EntryPointName = "_start"

type (
	// Parser is the public interface of the parser implementation.
	Parser interface {
		Parse(tokenStream tok.TokenStream, errorHandler tok.ErrorHandler) (ast.Block, error)
	}
)

// Return the public interface of the private parser implementation.
func NewParser() Parser {
	return newParser()
}
