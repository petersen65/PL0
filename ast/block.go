// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"io"

	exp "github.com/petersen65/pl0/v3/export"
	sym "github.com/petersen65/pl0/v3/symbol"
)

type (
	// Block represents a block of declarations and statements in the abstract syntax tree.
	Block interface {
		Node
		Children() []Node
		String() string
		Index() int
		Accept(visitor Visitor)
		RootBlock() Block
		Lookup(name string) *sym.Symbol
		LookupCurrent(name string) *sym.Symbol
		UniqueName(prefix rune) string
		Print(print io.Writer, args ...any) error
		Export(format exp.ExportFormat, print io.Writer) error
	}
)

// NewBlock creates a new block node in the abstract syntax tree.
func NewBlock(depth int32, scope sym.Scope, declarations []Declaration, statement Statement, id int) Block {
	return newBlock(depth, scope, declarations, statement, id)
}
