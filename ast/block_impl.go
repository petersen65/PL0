// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"encoding/json"
	"fmt"
	"io"

	eh "github.com/petersen65/pl0/v3/errors"
	exp "github.com/petersen65/pl0/v3/export"
	sym "github.com/petersen65/pl0/v3/symbol"
)

// Format for the string representation of a block node.
const blockFormat = "block(depth=%v)"

// Create a new block node in the abstract syntax tree.
func newBlock(depth int32, scope sym.Scope[Declaration], declarations []Declaration, statement Statement) Block {
	block := &BlockNode{
		CommonNode:   CommonNode{NodeKind: KindBlock},
		Depth:        depth,
		Scope:        scope,
		Declarations: declarations,
		Closure:      make([]Declaration, 0),
		Statement:    statement,
	}

	for _, declaration := range block.Declarations {
		declaration.SetParent(block)
	}

	statement.SetParent(block)
	return block
}

// Children nodes of the block node.
func (b *BlockNode) Children() []Node {
	children := make([]Node, 0, len(b.Declarations)+1)

	for _, declaration := range b.Declarations {
		children = append(children, declaration)
	}

	return append(children, b.Statement)
}

// String of the block node.
func (b *BlockNode) String() string {
	return fmt.Sprintf(blockFormat, b.Depth)
}

// Index returns the token stream index of the block node.
func (b *BlockNode) Index() int {
	if len(b.Declarations) > 0 {
		return b.Declarations[0].Index()
	}

	return b.Statement.Index()
}

// Accept the visitor for the block node.
func (b *BlockNode) Accept(visitor Visitor) {
	visitor.VisitBlock(b)
}

// Print the abstract syntax tree to the specified writer.
func (b *BlockNode) Print(print io.Writer, args ...any) error {
	// traverse the abstract syntax tree and print each node
	if err := printAbstractSyntaxTree(b, "", true, print); err != nil {
		return eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Error, abstractSyntaxExportFailed, nil, err)
	}

	return nil
}

// Export the abstract syntax tree of the block node to the specified writer in the specified format.
func (b *BlockNode) Export(format exp.ExportFormat, print io.Writer) error {
	// JSON formatting requires a prefix and indent for pretty printing
	const prefix, indent = "", "  "

	switch format {
	case exp.Json:
		// export the abstract syntax tree as a JSON object
		if raw, err := json.MarshalIndent(b, prefix, indent); err != nil {
			return eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Error, abstractSyntaxExportFailed, nil, err)
		} else {
			_, err = print.Write(raw)

			if err != nil {
				err = eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Error, abstractSyntaxExportFailed, nil, err)
			}

			return err
		}

	case exp.Text:
		// print is a convenience function to export the abstract syntax tree as a string to the print writer
		return b.Print(print)

	default:
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownExportFormat, format, nil))
	}
}
