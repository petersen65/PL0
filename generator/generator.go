// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package generator implements the translation of an abstract syntax tree (AST) into a flat intermediate code representation.
package generator

import (
	ast "github.com/petersen65/PL0/v3/ast"
	cor "github.com/petersen65/PL0/v3/core"
	ic "github.com/petersen65/PL0/v3/generator/intermediate"
)

// Translate an abstract syntax tree (AST) into intermediate code and map AST data types to intermediate code data types.
type Generator interface {
	Generate()
	GetIntermediateCodeUnit() ic.IntermediateCodeUnit
	GetDebugInformation() cor.DebugInformation
}

// Return the interface of the generator implementation.
func NewGenerator(abstractSyntax ast.Block, buildConfiguration cor.BuildConfiguration, tokenHandler cor.TokenHandler) Generator {
	return newGenerator(abstractSyntax, buildConfiguration, tokenHandler)
}
