// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import cor "github.com/petersen65/PL0/v2/core"

// Failure codes for the PL/0 abstract syntax tree.
const (
	_ = cor.Failure(iota + 3000)
	unknownDataTypeRepresentation
	illegalIdentifierName
	unknownSymbolKind
	unknownUnaryOperation
	unknownBinaryOperation
	unknownConditionalOperation
	cannotWalkOnNilNode
	walkRequiresVisitorOrFunction
	walkRequiresInterfaceOrFunction
	inOrderRequiresTwoChildren
	unknownExportFormat
	abstractSyntaxExportFailed
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	unknownDataTypeRepresentation:   "unknown data type representation: %v",
	illegalIdentifierName:           "illegal compiler-generated unique identifier name: %v",
	unknownSymbolKind:               "unknown symbol kind",
	unknownUnaryOperation:           "unknown unary operation",
	unknownBinaryOperation:          "unknown binary operation",
	unknownConditionalOperation:     "unknown conditional operation",
	cannotWalkOnNilNode:             "cannot walk on nil node",
	walkRequiresVisitorOrFunction:   "cannot walk without a visitor or visit function",
	walkRequiresInterfaceOrFunction: "walk requires a visitor with a Visitor interface or a visit function",
	inOrderRequiresTwoChildren:      "in-order traversal requires exactly two children",
	unknownExportFormat:             "unknown export format: %v",
	abstractSyntaxExportFailed:      "failed to export abstract syntax tree",
}
