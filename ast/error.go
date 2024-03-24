// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import cor "github.com/petersen65/PL0/core"

// Failure codes for the PL/0 abstract syntax tree.
const (
	_ = cor.Failure(iota + 3000)
	unknownConstantDataType
	unknownVariableDataType
	unknownLiteralDataType
	unknownSymbolKind
	unknownUnaryOperation
	unknownBinaryOperation
	unknownConditionalOperation
	cannotWalkOnNilNode
	walkRequiresVisitorOrFunction
	walkRequiresInterfaceOrFunction
	inOrderRequiresTwoChildren
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	unknownConstantDataType:         "unknown constant data type",
	unknownVariableDataType:         "unknown variable data type",
	unknownLiteralDataType:          "unknown literal data type",
	unknownSymbolKind:               "unknown symbol kind",
	unknownUnaryOperation:           "unknown unary operation",
	unknownBinaryOperation:          "unknown binary operation",
	unknownConditionalOperation:     "unknown conditional operation",
	cannotWalkOnNilNode:             "cannot walk on nil node",
	walkRequiresVisitorOrFunction:   "cannot walk without a visitor or visit function",
	walkRequiresInterfaceOrFunction: "walk requires a visitor with a Visitor interface or a visit function",
	inOrderRequiresTwoChildren:      "in-order traversal requires exactly two children",
}
