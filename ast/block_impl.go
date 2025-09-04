// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"encoding/json"
	"fmt"
	"io"
	"slices"

	eh "github.com/petersen65/pl0/v3/errors"
	exp "github.com/petersen65/pl0/v3/export"
	sym "github.com/petersen65/pl0/v3/symbol"
)

// Format for the string representation of a block node.
const blockFormat = "%v(depth=%v)"

// The block node represents a block in the AST.
type blockNode struct {
	commonNode                    // embedded common node
	NestingDepth    int           `json:"depth"`        // block nesting depth
	Scope           sym.Scope     `json:"scope"`        // scope with the symbol table of the block
	AllDeclarations []Declaration `json:"declarations"` // all declarations of the block
	AllCaptures     []Declaration `json:"captures"`     // all captured declarations from lexical parents of the block
	BlockStatement  Statement     `json:"statement"`    // statement of the block
	UniqueId        int           `json:"unique_id"`    // each block needs to be uniquely identifiable
	Counter         map[rune]uint `json:"name_counter"` // counter for compiler-generated unique names
}

// Prepare a new block node in the abstract syntax tree. The parent can be nil, if the new block is the root block.
// The prepared block is not yet fully initialized and needs to be finished with declarations and a statement.
func prepareBlock(parent Block, depth int, id int) Block {
	var outerScope sym.Scope

	// inherit the outer scope from the parent block
	if parent != nil {
		outerScope = parent.(*blockNode).Scope
	}

	// return a prepared block node with an initialized scope hierarchy
	return &blockNode{
		commonNode:      commonNode{NodeKind: KindBlock, ParentNode: parent},
		NestingDepth:    depth,
		Scope:           sym.NewScope(outerScope),
		AllDeclarations: make([]Declaration, 0),
		AllCaptures:     make([]Declaration, 0),
		BlockStatement:  NewEmptyStatement(),
		UniqueId:        id,
		Counter:         make(map[rune]uint),
	}
}

// Finish the prepared block by adding all its declarations and its statement.
func finishBlock(blockNode *blockNode, declarations []Declaration, statement Statement) {
	// add all declarations and the statement to this block node
	blockNode.AllDeclarations = declarations
	blockNode.BlockStatement = statement

	// ensure that all declarations belong to this block node
	for _, declaration := range blockNode.AllDeclarations {
		declaration.SetParent(blockNode)
	}

	// ensure that the statement is the statement of this block node
	blockNode.BlockStatement.SetParent(blockNode)
}

// Children nodes of the block node.
func (n *blockNode) Children() []Node {
	children := make([]Node, 0, len(n.AllDeclarations)+1)

	for _, declaration := range n.AllDeclarations {
		children = append(children, declaration)
	}

	return append(children, n.BlockStatement)
}

// String representation of the block node.
func (n *blockNode) String() string {
	return fmt.Sprintf(blockFormat, n.Kind(), n.NestingDepth)
}

// Accept the visitor for the block node.
func (n *blockNode) Accept(visitor Visitor) {
	visitor.VisitBlock(n)
}

// Index returns the token stream index of the block node.
func (n *blockNode) Index() int {
	if len(n.AllDeclarations) > 0 {
		return n.AllDeclarations[0].Index()
	}

	return n.BlockStatement.Index()
}

// The nesting depth of the block node.
func (n *blockNode) Depth() int {
	return n.NestingDepth
}

// All declarations contained in the block node.
func (n *blockNode) Declarations() []Declaration {
	return n.AllDeclarations
}

// The statement contained in the block node.
func (n *blockNode) Statement() Statement {
	return n.BlockStatement
}

// All captured declarations from lexical parents of the block.
func (n *blockNode) CapturedDeclarations() []Declaration {
	return n.AllCaptures
}

// Add a captured declaration to the block.
func (n *blockNode) AddCapturedDeclaration(declaration Declaration) {
	if !slices.Contains(n.AllCaptures, declaration) {
		n.AllCaptures = append(n.AllCaptures, declaration)
	}
}

// Find the root block node that contains all block nodes.
func (n *blockNode) RootBlock() Block {
	return searchBlock(n, RootBlock)
}

// Check if the block node is the root block node.
func (n *blockNode) IsRootBlock() bool {
	return n.ParentNode == nil
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

// Create a new compiler-generated unique name for the block.
func (s *blockNode) UniqueName(prefix rune) string {
	if _, ok := s.Counter[prefix]; !ok {
		s.Counter[prefix] = 0
	}

	s.Counter[prefix]++
	return fmt.Sprintf("%c%v.%v", prefix, s.UniqueId, s.Counter[prefix])
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
func searchBlock(node Node, mode BlockSearchMode) Block {
	for current := node.Parent(); current != nil; current = current.Parent() {
		if block, ok := current.(*blockNode); ok {
			if mode == CurrentBlock {
				return block
			} else if mode == RootBlock && block.Parent() == nil {
				return block
			}
		}
	}

	return nil
}

// Search for a declaration node in the abstract syntax tree based on its associated symbol information.
func searchDeclaration(node Node, symbol *sym.Symbol) Declaration {
	for block := searchBlock(node, CurrentBlock); block != nil; block = searchBlock(block, CurrentBlock) {
		for _, declaration := range block.Declarations() {
			if declaration.Symbol() == symbol {
				return declaration
			}
		}
	}

	return nil
}
