// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package analyzer implements the semantic analysis compiler phase.
package analyzer

import (
	ast "github.com/petersen65/pl0/v3/ast"
	eh "github.com/petersen65/pl0/v3/errors"
	tok "github.com/petersen65/pl0/v3/token"
)

type Analyzer interface {
	SetupBuiltInSymbols()
	PerformSemanticAnalysis()
}

// Return the interface of the semantic analyzer implementation.
func NewAnalyzer(abstractSyntax ast.Block, tokenHandler tok.TokenHandler) Analyzer {
	tokenHandler.ReplaceComponent(eh.Analyzer, failureMap)
	return newAnalyzer(abstractSyntax, tokenHandler)
}
