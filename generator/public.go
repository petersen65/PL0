// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package generator implements compiler passes for code generation. Code generators traverse the abstract syntax tree and generate code by driving an emitter.
package generator

import (
	asm "github.com/petersen65/PL0/v2/assembler"
	ast "github.com/petersen65/PL0/v2/ast"
	emt "github.com/petersen65/PL0/v2/emitter"
)

// The generator drives the emitter and assembler to generate code from the abstract syntax tree.
type Generator interface {
	Generate() (emt.Emitter, asm.Assembler)
}

// Return the public interface of the private generator implementation.
func NewGenerator(source string, abstractSyntax ast.Block) Generator {
	return newGenerator(source, abstractSyntax)
}
