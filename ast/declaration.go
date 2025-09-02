// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	tok "github.com/petersen65/pl0/v3/token"
	ts "github.com/petersen65/pl0/v3/typesystem"
)

type (
	// ConstantDeclaration node represents a constant declaration in the AST.
	ConstantDeclarationNode struct {
		commonNode          // embedded common node
		declarationNode     // embedded declaration node
		Value           any `json:"value"` // value of the constant
	}

	// VariableDeclaration node represents a variable declaration in the AST.
	VariableDeclarationNode struct {
		commonNode      // embedded common node
		declarationNode // embedded declaration node
	}

	// ProcedureDeclaration node represents a procedure declaration in the AST.
	ProcedureDeclarationNode struct {
		commonNode            // embedded common node
		declarationNode       // embedded declaration node
		Block           Block `json:"procedure_block"` // block of the procedure
	}

	// A declaration represented as an abstract syntax tree.
	Declaration interface {
		Node
		Usage() []Expression
	}
)

// An empty declaration is a 0 constant with special name, should only be used in the context of parser errors, and is free from any side-effect.
func NewEmptyDeclaration() Declaration {
	return newConstantDeclaration(emptyConstantName, ts.Integer64.String(), int64(0), tok.NoTokenStreamIndex)
}

// NewConstantDeclaration creates a new constant declaration node in the abstract syntax tree.
func NewConstantDeclaration(name, dataTypeName string, value any, index int) Declaration {
	return newConstantDeclaration(name, dataTypeName, value, index)
}

// NewVariableDeclaration creates a new variable declaration node in the abstract syntax tree.
func NewVariableDeclaration(name, dataTypeName string, index int) Declaration {
	return newVariableDeclaration(name, dataTypeName, index)
}

// NewProcedureDeclaration creates a new procedure declaration node in the abstract syntax tree.
func NewProcedureDeclaration(name string, block Block, index int) Declaration {
	return newProcedureDeclaration(name, block, index)
}
