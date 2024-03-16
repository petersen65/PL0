// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

// Generator is the compiler pass for code generation. It implements the Visitor interface to traverse the AST and generate code.
package generator

import (
	ast "github.com/petersen65/PL0/ast"
	emt "github.com/petersen65/PL0/emitter"
)

type (
	// Generator is the public interface of the generator implementation.
	Generator interface {
		Generate() emt.Emitter
	}
)

// Return the public interface of the private generator implementation.
func NewGenerator(abstractSyntax ast.Block) Generator {
	return newGenerator(abstractSyntax)
}
