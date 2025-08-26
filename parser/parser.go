// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package parser implements a syntax analyzer for the programming language PL/0.
package parser

import (
	ast "github.com/petersen65/PL0/v3/ast"
	cor "github.com/petersen65/PL0/v3/core"
)

// The parser processes a token stream, produces an abstract syntax tree (AST), and returns it along with an initial token handler.
type Parser interface {
	Parse() (ast.Block, cor.TokenHandler)
}

// Return the interface of the parser implementation.
func NewParser(tokenStream cor.TokenStream, errorHandler cor.ErrorHandler) Parser {
	return newParser(tokenStream, errorHandler)
}
