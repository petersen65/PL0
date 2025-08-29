// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package analyzer implements a semantic analyzer for the programming language PL/0.
package analyzer

import (
	ast "github.com/petersen65/pl0/v3/ast"
	eh "github.com/petersen65/pl0/v3/errors"
	tok "github.com/petersen65/pl0/v3/token"
)

// Name analysis validates the correctness of identifier declarations and creates a symbol table with type information provided by the abstract syntax tree (AST).
// Herby, the analyzer checks for duplicate declarations, verifies that identifiers are declared before use, and creates a closure for accessing identifiers in lexical parents.
type Analyzer interface {
	Analyze()
}

// Return the interface of the analyzer implementation.
func NewAnalyzer(abstractSyntax ast.Block, errorHandler eh.ErrorHandler, tokenHandler tok.TokenHandler) Analyzer {
	tokenHandler.ReplaceComponent(eh.Analyzer, failureMap)
	return newAnalyzer(abstractSyntax, errorHandler, tokenHandler)
}
