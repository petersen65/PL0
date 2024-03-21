// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package analyzer implements semantic analysis compiler passes by traversing the abstract syntax tree.
package analyzer

import (
	ast "github.com/petersen65/PL0/ast"
	tok "github.com/petersen65/PL0/token"
)

// Declaration analysis is a compiler pass that validates the correctness of identifier declarations and their usage. It fills in the symbol table with declared identifiers and their types. It also checks for duplicate declarations and undeclared identifiers. 
type DeclarationAnalysis interface {
	Analyze() error
}

// Return the public interface of the private declaration analysis implementation.
func NewDeclarationAnalysis(abstractSyntax ast.Block, errorHandler tok.ErrorHandler, tokenHandler tok.TokenHandler) DeclarationAnalysis {
	tokenHandler.ReplaceComponent(tok.Analyzer, failureMap)
	return newDeclarationAnalysis(abstractSyntax, errorHandler, tokenHandler)
}
