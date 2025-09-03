// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	sym "github.com/petersen65/pl0/v3/symbol"
	tok "github.com/petersen65/pl0/v3/token"
	ts "github.com/petersen65/pl0/v3/typesystem"
)

type (
	// A constant declaration node in the abstract syntax tree.
	ConstantDeclaration interface {
		Declaration
		Value() any
	}

	// A variable declaration node in the abstract syntax tree.
	VariableDeclaration interface {
		Declaration
	}

	// A procedure declaration node in the abstract syntax tree.
	ProcedureDeclaration interface {
		Declaration
	}

	// Any declaration node in the abstract syntax tree.
	Declaration interface {
		Node
		Name() string
		DataTypeName() string
		SetSymbol(sym *sym.Symbol)
		Usage() []Expression
	}
)

// An empty declaration is a 0 constant with special name, should only be used in the context of parser errors, and is free from any side-effect.
func NewEmptyDeclaration() Declaration {
	return newConstantDeclaration(emptyConstantName, ts.Integer64.String(), int64(0), tok.NoTokenStreamIndex)
}

// Create a new constant declaration node in the abstract syntax tree.
func NewConstantDeclaration(name, dataTypeName string, value any, index int) ConstantDeclaration {
	return newConstantDeclaration(name, dataTypeName, value, index)
}

// Create a new variable declaration node in the abstract syntax tree.
func NewVariableDeclaration(name, dataTypeName string, index int) VariableDeclaration {
	return newVariableDeclaration(name, dataTypeName, index)
}

// Create a new procedure declaration node in the abstract syntax tree.
func NewProcedureDeclaration(name string, block Block, index int) ProcedureDeclaration {
	return newProcedureDeclaration(name, block, index)
}
