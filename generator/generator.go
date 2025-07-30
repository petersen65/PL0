// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package generator implements the intermediate code generation compiler phase by traversing the abstract syntax tree (AST).
package generator

import (
	ast "github.com/petersen65/PL0/v2/ast"
	ic "github.com/petersen65/PL0/v2/generator/intermediate"
)

// The generator translates the abstract syntax tree (AST) into an intermediate code representation and maps AST data types to intermediate code data types.
type Generator interface {
	Generate()
	GetIntermediateCodeUnit() ic.IntermediateCodeUnit
}

// Return the interface of the generator implementation.
func NewGenerator(abstractSyntax ast.Block) Generator {
	return newGenerator(abstractSyntax)
}
