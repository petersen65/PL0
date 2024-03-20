// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package analyzer implements semantic analysis compiler passes by traversing the abstract syntax tree.
package analyzer

import (
	ast "github.com/petersen65/PL0/ast"
	tok "github.com/petersen65/PL0/token"
)

// DeclarationAnalysis is the public interface of the declaration analyzer implementation.
type DeclarationAnalysis interface {
	Analyze()
}

// Return the public interface of the private declaration analyzer implementation.
func NewDeclarationAnalysis(abstractSyntax ast.Block, tokenHandler tok.TokenHandler) DeclarationAnalysis {
	return newDeclarationAnalysis(abstractSyntax, tokenHandler)
}
