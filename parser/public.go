// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package parser implements the PL/0 parser that performs a syntactical analysis of the token stream.
package parser

import (
	ast "github.com/petersen65/PL0/ast"
	tok "github.com/petersen65/PL0/token"
)

// Name of the entry point procedure of a program.
const EntryPointName = "_start"

type (
	// The parser interface provides methods for parsing a token stream into an abstract syntax tree.
	Parser interface {
		Parse() (ast.Block, tok.TokenHandler, error)
	}
)

// Return the public interface of the private parser implementation.
func NewParser(tokenStream tok.TokenStream, errorHandler tok.ErrorHandler) Parser {
	return newParser(tokenStream, errorHandler)
}
