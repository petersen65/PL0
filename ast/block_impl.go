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

// The block node represents a block in the AST.
type blockNode struct {
	commonNode                 // embedded common node
	Depth        int32         `json:"depth"`              // block nesting depth
	Scope        sym.Scope     `json:"scope"`              // scope with the symbol table of the block
	Declarations []Declaration `json:"declarations"`       // all declarations of the block
	Closure      []Declaration `json:"closure"`            // all captured variable declarations from lexical parents of the block
	Statement    Statement     `json:"statement"`          // statement of the block
	Id           int           `json:"id"`                 // each block needs to be uniquely identifiable
	Counter      map[rune]uint `json:"identifier_counter"` // counter for compiler-generated unique names
}

// Prepare a new block node in the abstract syntax tree. The parent can be nil, if the new block is the root block.
// The prepared block is not yet fully initialized and needs to be finished with declarations and a statement.
func prepareBlock(parent Block, depth int32, id int) Block {
	var outerScope sym.Scope

	// inherit the outer scope from the parent block
	if parent != nil {
		outerScope = parent.(*blockNode).Scope
	}

	// return a prepared block node with an initialized scope hierarchy
	return &blockNode{
		commonNode:   commonNode{NodeKind: KindBlock, ParentNode: parent},
		Depth:        depth,
		Scope:        sym.NewScope(outerScope),
		Declarations: make([]Declaration, 0),
		Closure:      make([]Declaration, 0),
		Statement:    NewEmptyStatement(),
		Id:           id,
		Counter:      make(map[rune]uint),
	}
}

// Finish the prepared block by adding all its declarations and its statement.
func finishBlock(blockNode *blockNode, declarations []Declaration, statement Statement) {
	// add all declarations and the statement to this block node
	blockNode.Declarations = declarations
	blockNode.Statement = statement

	// ensure that all declarations belong to this block node
	for _, declaration := range blockNode.Declarations {
		declaration.SetParent(blockNode)
	}

	// ensure that the statement is the statement of this block node
	blockNode.Statement.SetParent(blockNode)
}

// Children nodes of the block node.
func (n *blockNode) Children() []Node {
	children := make([]Node, 0, len(n.Declarations)+1)

	for _, declaration := range n.Declarations {
		children = append(children, declaration)
	}

	return append(children, n.Statement)
}

// String representation of the block node.
func (n *blockNode) String() string {
	return fmt.Sprintf(blockFormat, n.Kind(), n.Depth)
}

// Accept the visitor for the block node.
func (n *blockNode) Accept(visitor Visitor) {
	visitor.VisitBlock(n)
}

// Index returns the token stream index of the block node.
func (n *blockNode) Index() int {
	if len(n.Declarations) > 0 {
		return n.Declarations[0].Index()
	}

	return n.Statement.Index()
}

// Find the root block node that contains all block nodes.
func (n *blockNode) RootBlock() Block {
	return searchBlock(n, RootBlock)
}

// Find the current block node that contains this block node.
func (n *blockNode) CurrentBlock() Block {
	return searchBlock(n, CurrentBlock)
}

// Insert a new symbol into the block's current scope.
func (n *blockNode) Insert(name string, symbol *sym.Symbol) {
	n.Scope.Insert(name, symbol)
}

// Find a symbol in the block's scope and all its parent scopes.
func (n *blockNode) Lookup(name string) *sym.Symbol {
	return n.Scope.Lookup(name)
}

// Find a symbol in the block's current scope.
func (n *blockNode) LookupCurrent(name string) *sym.Symbol {
	return n.Scope.LookupCurrent(name)
}

// Create a new compiler-generated unique name for a block.
func (s *blockNode) UniqueName(prefix rune) string {
	if _, ok := s.Counter[prefix]; !ok {
		s.Counter[prefix] = 0
	}

	s.Counter[prefix]++
	return fmt.Sprintf("%c%v.%v", prefix, s.Id, s.Counter[prefix])
}

// Print the abstract syntax tree to the specified writer.
func (n *blockNode) Print(print io.Writer, args ...any) error {
	// traverse the abstract syntax tree and print each node
	if err := printAbstractSyntaxTree(n, "", true, print); err != nil {
		return eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Error, abstractSyntaxExportFailed, nil, err)
	}

	return nil
}

// Export the abstract syntax tree of the block node to the specified writer in the specified format.
func (n *blockNode) Export(format exp.ExportFormat, print io.Writer) error {
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

// Search for a parent block node in the abstract syntax tree based on the search mode.
func searchBlock(current Node, mode BlockSearchMode) *blockNode {
	for current != nil {
		if block, ok := current.(*blockNode); ok {
			if mode == CurrentBlock {
				return block
			} else if mode == RootBlock && block.Parent() == nil {
				return block
			}
		}

		current = current.Parent()
	}

	return nil
}
