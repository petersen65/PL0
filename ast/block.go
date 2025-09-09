// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	exp "github.com/petersen65/pl0/v3/export"
	sym "github.com/petersen65/pl0/v3/symbol"
	ts "github.com/petersen65/pl0/v3/typesystem"
)

// Search parent block nodes in the abstract syntax tree.
const (
	CurrentBlock BlockSearchMode = iota
	RootBlock
)

type (
	// Search mode for block nodes in the abstract syntax tree.
	BlockSearchMode int

	// A symbol table is a collection of symbols that can be used to look up names and their associated information.
	SymbolTable interface {
		Insert(name string, symbol *sym.Symbol)
		Lookup(name string) *sym.Symbol
		LookupCurrent(name string) *sym.Symbol
		UniqueName(prefix rune) string
		BuiltInDataType(name string) ts.TypeDescriptor
	}

	// Block represents a block of declarations and statements in the abstract syntax tree.
	Block interface {
		Node
		SymbolTable
		exp.Exporter
		Depth() int
		Function() FunctionDeclaration
		Declarations() []Declaration
		Statement() Statement
		CapturedDeclarations() []Declaration
		AddCapturedDeclaration(declaration Declaration)
		RootBlock() Block
		IsRootBlock() bool
	}
)

// Prepare a new block node in the abstract syntax tree. The parent can be nil, if the new block is the root block.
// The prepared block is not yet fully initialized and needs to be finished with declarations and a statement.
func PrepareBlock(parent Block, depth int, id int) Block {
	return prepareBlock(parent, depth, id)
}

// Finish the prepared block by adding all its declarations and its statement.
func FinishBlock(block Block, declarations []Declaration, statement Statement) {
	finishBlock(block.(*blockNode), declarations, statement)
}

// Search for a parent block node in the abstract syntax tree based on the search mode.
func SearchBlock(node Node, mode BlockSearchMode) Block {
	return searchBlock(node, mode)
}

// Search for a declaration node in the abstract syntax tree based on its associated symbol information.
func SearchDeclaration(node Node, symbol *sym.Symbol) Declaration {
	return searchDeclaration(node, symbol)
}
