// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"io"

	exp "github.com/petersen65/pl0/v3/export"
	sym "github.com/petersen65/pl0/v3/symbol"
)

type (
	// Block node represents a block in the AST.
	BlockNode struct {
		commonNode                 // embedded common node
		Depth        int32         `json:"depth"`              // block nesting depth
		Scope        sym.Scope     `json:"scope"`              // scope with the symbol table of the block
		Declarations []Declaration `json:"declarations"`       // all declarations of the block
		Closure      []Declaration `json:"closure"`            // all captured variable declarations from lexical parents of the block
		Statement    Statement     `json:"statement"`          // statement of the block
		Id           int           `json:"id"`                 // each block needs to be uniquely identifiable
		Counter      map[rune]uint `json:"identifier_counter"` // counter for compiler-generated unique names
	}

	// A block represented as an abstract syntax tree.
	Block interface {
		Node
		Children() []Node
		String() string
		Index() int
		Accept(visitor Visitor)
		Lookup(name string) *sym.Symbol
		UniqueName(prefix rune) string
		Print(print io.Writer, args ...any) error
		Export(format exp.ExportFormat, print io.Writer) error
	}
)

// NewBlock creates a new block node in the abstract syntax tree.
func NewBlock(depth int32, scope sym.Scope, declarations []Declaration, statement Statement, id int) Block {
	return newBlock(depth, scope, declarations, statement, id)
}
