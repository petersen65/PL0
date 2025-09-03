// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	sym "github.com/petersen65/pl0/v3/symbol"
	tok "github.com/petersen65/pl0/v3/token"
	ts "github.com/petersen65/pl0/v3/typesystem"
)

// Parameter passing modes for functions and procedures.
const (
	CallByValue          PassingMode = iota // call by value
	CallByReference                         // call by reference
	CallByConstReference                    // call by const reference
	OutputParameter                         // output parameter
)

type (
	// Function or procedure parameter passing modes.
	PassingMode int

	// A constant declaration node in the abstract syntax tree.
	ConstantDeclaration interface {
		Declaration
		Value() any
	}

	// A variable declaration node in the abstract syntax tree.
	VariableDeclaration interface {
		Declaration
	}

	// Parameter and its passing mode for a function or procedure call.
	FunctionParameter struct {
		Name         string      `json:"name"`           // identifier name used to reference this parameter within the function
		DataTypeName string      `json:"data_type_name"` // data type name defining the data type of this parameter
		PassingMode  PassingMode `json:"passing_mode"`   // determines how the parameter is passed (e.g., by value, by reference)
	}

	// A function or procedure declaration node in the abstract syntax tree.
	FunctionDeclaration interface {
		Declaration
		Block() Block
		SetBlock(block Block)
		IsFunction() bool
		IsProcedure() bool
		Parameters() []*FunctionParameter
		ReturnTypeName() string
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

// Create a new function parameter for a function or procedure declaration.
func NewFunctionParameter(name, dataTypeName string, mode PassingMode) *FunctionParameter {
	return &FunctionParameter{Name: name, DataTypeName: dataTypeName, PassingMode: mode}
}

// Create a new function or procedure declaration node in the abstract syntax tree. For a procedure, the return type name has to be an empty string.
func NewFunctionDeclaration(name string, block Block, parameters []*FunctionParameter, returnTypeName string, index int) FunctionDeclaration {
	if parameters == nil {
		parameters = make([]*FunctionParameter, 0)
	}

	return newFunctionDeclaration(name, block, parameters, returnTypeName, index)
}
