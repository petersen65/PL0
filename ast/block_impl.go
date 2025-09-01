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
const blockFormat = "%v(depth=%v)"

// Create a new block node in the abstract syntax tree.
func newBlock(depth int32, scope sym.Scope, declarations []Declaration, statement Statement) Block {
	blockNode := &BlockNode{
		CommonNode:   CommonNode{NodeKind: KindBlock},
		Depth:        depth,
		Scope:        scope,
		Declarations: declarations,
		Closure:      make([]Declaration, 0),
		Statement:    statement,
	}

	for _, declaration := range blockNode.Declarations {
		declaration.SetParent(blockNode)
	}

	statement.SetParent(blockNode)
	return blockNode
}

// Children nodes of the block node.
func (n *BlockNode) Children() []Node {
	children := make([]Node, 0, len(n.Declarations)+1)

	for _, declaration := range n.Declarations {
		children = append(children, declaration)
	}

	return append(children, n.Statement)
}

// String representation of the block node.
func (n *BlockNode) String() string {
	return fmt.Sprintf(blockFormat, n.Kind(), n.Depth)
}

// Index returns the token stream index of the block node.
func (n *BlockNode) Index() int {
	if len(n.Declarations) > 0 {
		return n.Declarations[0].Index()
	}

	return n.Statement.Index()
}

// Accept the visitor for the block node.
func (n *BlockNode) Accept(visitor Visitor) {
	visitor.VisitBlock(n)
}

// Print the abstract syntax tree to the specified writer.
func (n *BlockNode) Print(print io.Writer, args ...any) error {
	// traverse the abstract syntax tree and print each node
	if err := printAbstractSyntaxTree(n, "", true, print); err != nil {
		return eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Error, abstractSyntaxExportFailed, nil, err)
	}

	return nil
}

// Export the abstract syntax tree of the block node to the specified writer in the specified format.
func (n *BlockNode) Export(format exp.ExportFormat, print io.Writer) error {
	// JSON formatting requires a prefix and indent for pretty printing
	const prefix, indent = "", "  "

	switch format {
	case exp.Json:
		// export the abstract syntax tree as a JSON object
		if raw, err := json.MarshalIndent(n, prefix, indent); err != nil {
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
		return n.Print(print)

	default:
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownExportFormat, format, nil))
	}
}
