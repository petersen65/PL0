// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package parser implements a syntax analyzer for the parsing compiler phase.
package parser

import (
	ast "github.com/petersen65/pl0/v3/ast"
	eh "github.com/petersen65/pl0/v3/errors"
	tok "github.com/petersen65/pl0/v3/token"
)

// The parser processes a token stream, produces an abstract syntax tree (AST), and returns it along with an initial token handler.
type Parser interface {
	Parse() (ast.Block, tok.TokenHandler)
}

// Return the interface of the parser implementation.
func NewParser(tokenStream tok.TokenStream, errorHandler eh.ErrorHandler) Parser {
	return newParser(tokenStream, errorHandler)
}
