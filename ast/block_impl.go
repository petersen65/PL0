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
	ts "github.com/petersen65/pl0/v3/typesystem"
)

// Format for the string representation of a block node.
const blockFormat = "%v(depth=%v)"

// The block node represents a block in the AST.
type blockNode struct {
	commonNode                  // embedded common node
	NestingDepth  int           `json:"depth"`        // block nesting depth
	Scope         sym.Scope     `json:"scope"`        // scope with the symbol table of the block
	Declarations_ []Declaration `json:"declarations"` // all declarations of the block
	Captures_     []Declaration `json:"captures"`     // all captured declarations from lexical parents of the block
	Statement_    Statement     `json:"statement"`    // statement of the block
	UniqueId      int           `json:"unique_id"`    // each block needs to be uniquely identifiable
	Counter       map[rune]uint `json:"name_counter"` // counter for compiler-generated unique names
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
	// note: the parent node of the block node is set by the parser to its function declaration node
	return &blockNode{
		commonNode:    commonNode{NodeKind: KindBlock},
		NestingDepth:  depth,
		Scope:         sym.NewScope(outerScope),
		Declarations_: make([]Declaration, 0),
		Captures_:     make([]Declaration, 0),
		Statement_:    NewEmptyStatement(),
		UniqueId:      id,
		Counter:       make(map[rune]uint),
	}
}

// Finish the prepared block by adding all its declarations and its statement.
func finishBlock(blockNode *blockNode, declarations []Declaration, statement Statement) {
	// add all declarations and the statement to this block node
	blockNode.Declarations_ = declarations
	blockNode.Statement_ = statement

	// ensure that all declarations belong to this block node
	for _, declaration := range blockNode.Declarations_ {
		declaration.SetParent(blockNode)
	}

	// ensure that the statement is the statement of this block node
	blockNode.Statement_.SetParent(blockNode)
}

// Children nodes of the block node.
func (n *blockNode) Children() []Node {
	children := make([]Node, 0, len(n.Declarations_)+1)

	for _, declaration := range n.Declarations_ {
		children = append(children, declaration)
	}

	return append(children, n.Statement_)
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
	if len(n.Declarations_) > 0 {
		return n.Declarations_[0].Index()
	}

	return n.Statement_.Index()
}

// The nesting depth of the block node.
func (n *blockNode) Depth() int {
	return n.NestingDepth
}

// The function declaration associated with the block node, or nil if the block is the root block.
func (n *blockNode) Function() FunctionDeclaration {
	if n.ParentNode != nil {
		return n.ParentNode.(FunctionDeclaration)
	}

	return nil
}

// All declarations contained in the block node.
func (n *blockNode) Declarations() []Declaration {
	return n.Declarations_
}

// The statement contained in the block node.
func (n *blockNode) Statement() Statement {
	return n.Statement_
}

// All captured declarations from lexical parents of the block.
func (n *blockNode) CapturedDeclarations() []Declaration {
	return n.Captures_
}

// Add a captured declaration from a lexical parent to the block.
func (n *blockNode) AddCapturedDeclaration(declaration Declaration) {
	if !slices.Contains(n.Captures_, declaration) {
		n.Captures_ = append(n.Captures_, declaration)
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

// Find a built-in data type in the root block's scope.
func (s *blockNode) BuiltInDataType(name string) ts.TypeDescriptor {
	// built-in data types are always defined in the outermost scope of the root block
	rb := s.RootBlock()

	// look for the built-in data type in the root block's scope
	if symbol := rb.Lookup(name); symbol != nil && symbol.Kind == sym.DataTypeEntry && symbol.DataType.IsBuiltIn() {
		return symbol.DataType
	}

	// there is no built-in data type with the specified name
	return nil
}

// Print the abstract syntax tree to the specified writer.
func (n *blockNode) Print(print io.Writer, args ...any) error {
	// traverse the abstract syntax tree and print each node
	if err := printAbstractSyntaxTree(n, "", true, print); err != nil {
		return eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Error, abstractSyntaxExportFailed, err)
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
			return eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Error, abstractSyntaxExportFailed, err)
		} else {
			_, err = print.Write(raw)

			if err != nil {
				err = eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Error, abstractSyntaxExportFailed, err)
			}

			return err
		}

	case exp.Text:
		// print is a convenience function to export the abstract syntax tree as a string to the print writer
		return n.Print(print)

	default:
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownExportFormat, nil, format))
	}
}

// Search for a parent block node in the abstract syntax tree based on the search mode.
func searchBlock(node Node, mode BlockSearchMode) Block {
	// the parent of a block node is its function declaration node or nil for the root block
	//   - the parent of the function declaration node is a block node that will be seen as the parent block of the current block
	//
	// if the current node is a block node and the search mode is CurrentBlock, the search must start at the current block's parent node
	//   - otherwise, the search would always return the current block node itself
	if node.Kind() == KindBlock && node.Parent() != nil && mode == CurrentBlock {
		node = node.Parent()
	}

	// traverse up the parent chain of block nodes and skip all non-block nodes
	//   - if the search mode is CurrentBlock, return the first block node found
	//   - if the search mode is RootBlock, return the block node without a parent (the root block)
	for current := node; current != nil; current = current.Parent() {
		// skip all non-block nodes
		if current.Kind() != KindBlock {
			continue
		}

		// at this point current is a block node
		if mode == CurrentBlock || mode == RootBlock && current.Parent() == nil {
			return current.(*blockNode)
		}
	}

	return nil
}

// Search for a declaration node in the abstract syntax tree based on its associated symbol information.
func searchDeclaration(node Node, symbol *sym.Symbol) Declaration {
	// traverse up the parent chain of block nodes and skip all non-block nodes
	//   - in each block node, search for the declaration with the specified symbol
	//   - return the first declaration found or nil if no declaration was found
	for block := searchBlock(node, CurrentBlock); block != nil; block = searchBlock(block, CurrentBlock) {
		// search for the declaration with the specified symbol in the current block node
		for _, declaration := range block.Declarations() {
			if declaration.Symbol() == symbol {
				return declaration
			}
		}
	}

	return nil
}
