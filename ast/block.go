// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"io"

	exp "github.com/petersen65/pl0/v3/export"
	sym "github.com/petersen65/pl0/v3/symbol"
)

// Search parent block nodes in the abstract syntax tree.
const (
	CurrentBlock BlockSearchMode = iota
	RootBlock
)

type (
	// Search mode for block nodes in the abstract syntax tree.
	BlockSearchMode int

	// Block represents a block of declarations and statements in the abstract syntax tree.
	Block interface {
		Node
		Children() []Node
		String() string
		Index() int
		Accept(visitor Visitor)
		RootBlock() Block
		Insert(name string, symbol *sym.Symbol)
		Lookup(name string) *sym.Symbol
		LookupCurrent(name string) *sym.Symbol
		UniqueName(prefix rune) string
		Print(print io.Writer, args ...any) error
		Export(format exp.ExportFormat, print io.Writer) error
	}
)

// Prepare a new block node in the abstract syntax tree. The parent can be nil, if the new block is the root block.
// The prepared block is not yet fully initialized and needs to be finished with declarations and a statement.
func PrepareBlock(parent Block, depth int32, id int) Block {
	return prepareBlock(parent, depth, id)
}

// Finish the prepared block by adding all its declarations and its statement.
func FinishBlock(block Block, declarations []Declaration, statement Statement) {
	finishBlock(block.(*blockNode), declarations, statement)
}

// Search for a parent block node in the abstract syntax tree based on the search mode.
func SearchBlock(current Node, mode BlockSearchMode) Block {
	return searchBlock(current, mode)
}
