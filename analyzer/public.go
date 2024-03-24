// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package analyzer implements semantic analysis compiler passes by traversing the abstract syntax tree.
package analyzer

import (
	ast "github.com/petersen65/PL0/ast"
	cor "github.com/petersen65/PL0/core"
)

// Name analysis is a compiler pass that validates the correctness of identifier declarations and their usage. It fills in the symbol table with declared identifiers and their types. It also checks for duplicate declarations and undeclared identifiers.
type NameAnalysis interface {
	Analyze()
}

// Return the public interface of the private name analysis implementation.
func NewNameAnalysis(abstractSyntax ast.Block, errorHandler cor.ErrorHandler, tokenHandler cor.TokenHandler) NameAnalysis {
	tokenHandler.ReplaceComponent(cor.Analyzer, failureMap)
	return newNameAnalysis(abstractSyntax, errorHandler, tokenHandler)
}
