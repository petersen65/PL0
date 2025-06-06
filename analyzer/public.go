// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package analyzer implements semantic analysis compiler phases by traversing the abstract syntax tree.
package analyzer

import (
	ast "github.com/petersen65/PL0/v2/ast"
	cor "github.com/petersen65/PL0/v2/core"
)

// Name analysis is a compiler phase that validates the correctness of identifier declarations and their usage. It fills in the symbol table with declared identifiers and their types. It also checks for duplicate declarations and undeclared identifiers. As a final step, the closure of each block is determined.
type NameAnalysis interface {
	Analyze()
}

// Return the public interface of the private name analysis implementation.
func NewNameAnalysis(abstractSyntax ast.Block, errorHandler cor.ErrorHandler, tokenHandler cor.TokenHandler) NameAnalysis {
	tokenHandler.ReplaceComponent(cor.Analyzer, failureMap)
	return newNameAnalysis(abstractSyntax, errorHandler, tokenHandler)
}
