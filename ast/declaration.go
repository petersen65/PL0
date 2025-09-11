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
		Expression() Expression
		Value() any
		SetDataTypeName(dataTypeName string)
	}

	// A variable declaration node in the abstract syntax tree.
	VariableDeclaration interface {
		Declaration
	}

	// A function or procedure declaration node in the abstract syntax tree.
	FunctionDeclaration interface {
		Declaration
		Block() Block
		SetBlock(block Block)
		IsFunction() bool
		IsProcedure() bool
		Parameters() []*ts.FunctionParameter
		ReturnTypeName() string
	}

	// Any declaration node in the abstract syntax tree.
	Declaration interface {
		Node
		Depth() int
		IdentifierName() string
		DataTypeName() string
		DataType() ts.TypeDescriptor
		IndexPair() (int, int)
		Symbol() *sym.Symbol
		SetSymbol(sym *sym.Symbol)
		Usage() []Expression
		AddUsage(use Expression)
	}
)

// An empty declaration is a 0 constant with special name, should only be used in the context of parser errors, and is free from any side-effect.
func NewEmptyDeclaration() Declaration {
	return newConstantDeclaration(emptyConstantName, NewEmptyExpression(), tok.NoTokenStreamIndex)
}

// Create a new constant declaration node in the abstract syntax tree.
func NewConstantDeclaration(identifierName string, expression Expression, index int) ConstantDeclaration {
	return newConstantDeclaration(identifierName, expression, index)
}

// Create a new variable declaration node in the abstract syntax tree.
func NewVariableDeclaration(identifierName, dataTypeName string, identifierIndex, dataTypeIndex int) VariableDeclaration {
	return newVariableDeclaration(identifierName, dataTypeName, identifierIndex, dataTypeIndex)
}

// Create a new function parameter for a function or procedure declaration.
func NewFunctionParameter(parameterName, dataTypeName string, mode ts.ParameterPassingMode) *ts.FunctionParameter {
	return ts.NewFunctionParameter(parameterName, dataTypeName, nil, mode)
}

// Create a new function or procedure declaration node in the abstract syntax tree. For a procedure, the return type name has to be an empty string.
func NewFunctionDeclaration(identifierName string, block Block, parameters []*ts.FunctionParameter, returnTypeName string, index int) FunctionDeclaration {
	if parameters == nil {
		parameters = make([]*ts.FunctionParameter, 0)
	}

	return newFunctionDeclaration(identifierName, block, parameters, returnTypeName, index)
}
