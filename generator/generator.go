// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package generator implements the intermediate code generation compiler phase by traversing the abstract syntax tree.
package generator

import ast "github.com/petersen65/PL0/v2/ast"

// Generator is the interface for the intermediate code generation compiler phase.
type Generator interface {
	Generate()
	GetIntermediateCodeUnit() IntermediateCodeUnit
}

// Return the interface of the generator implementation.
func NewGenerator(abstractSyntax ast.Block) Generator {
	return newGenerator(abstractSyntax)
}
