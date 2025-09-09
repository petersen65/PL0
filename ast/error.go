// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import eh "github.com/petersen65/pl0/v3/errors"

// Failure codes for the abstract syntax tree.
const (
	_ eh.Failure = iota + 3000
	unknownIdentifierKind
	unknownUnaryOperation
	unknownBinaryOperation
	unknownComparisonOperation
	cannotWalkOnNilNode
	walkRequiresVisitorOrFunction
	walkRequiresInterfaceOrFunction
	inOrderRequiresBinaryTree
	unknownTraversalOrder
	unknownExportFormat
	abstractSyntaxExportFailed
)

// Map failure codes to error messages.
var failureMap = map[eh.Failure]string{
	unknownIdentifierKind:           "unknown identifier kind: %v",
	unknownUnaryOperation:           "unknown unary operation: %v",
	unknownBinaryOperation:          "unknown binary operation: %v",
	unknownComparisonOperation:      "unknown comparison operation: %v",
	cannotWalkOnNilNode:             "cannot walk on nil node",
	walkRequiresVisitorOrFunction:   "cannot walk without a visitor or visit function",
	walkRequiresInterfaceOrFunction: "walk requires a visitor with a Visitor interface or a visit function",
	inOrderRequiresBinaryTree:       "in-order traversal requires a binary tree that has exactly two children per node",
	unknownTraversalOrder:           "unknown traversal order: %v",
	unknownExportFormat:             "unknown export format: %v",
	abstractSyntaxExportFailed:      "failed to export abstract syntax tree",
}
