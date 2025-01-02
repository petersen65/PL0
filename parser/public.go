// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package parser implements the PL/0 parser that performs a syntactical analysis of the token stream.
package parser

import (
	ast "github.com/petersen65/PL0/v2/ast"
	cor "github.com/petersen65/PL0/v2/core"
)

type (
	// The parser interface provides methods for parsing a token stream into an abstract syntax tree.
	Parser interface {
		Parse() (ast.Block, cor.TokenHandler)
	}
)

// Return the public interface of the private parser implementation.
func NewParser(tokenStream cor.TokenStream, errorHandler cor.ErrorHandler) Parser {
	return newParser(tokenStream, errorHandler)
}
