// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"fmt"

	eh "github.com/petersen65/pl0/v3/errors"
	sym "github.com/petersen65/pl0/v3/symbol"
)

// Format for the string representation of unknown identifier-use nodes.
const unknownUseFormat = "%v(kind=unknown,name=%v,usage=%v)"

// Formats for the string representation of identifier-use nodes.
var identifierUseFormats = map[sym.Entry]string{
	sym.ConstantEntry:  "%v(kind=%v,name=%v,value=%v,usage=%v)",
	sym.VariableEntry:  "%v(kind=%v,name=%v,usage=%v)",
	sym.ProcedureEntry: "%v(kind=%v,name=%v,usage=%v)",
}

// Create a new identifier-use node in the abstract syntax tree.
func newIdentifierUse(name string, context sym.Entry, index int) Expression {
	return &IdentifierUseNode{
		commonNode:     commonNode{NodeKind: KindIdentifierUse},
		expressionNode: expressionNode{TokenStreamIndex: index},
		Name:           name,
		Context:        context,
	}
}

// Children nodes of the identifier-use node.
func (n *IdentifierUseNode) Children() []Node {
	return make([]Node, 0)
}

// String representation of the identifier-use node.
func (n *IdentifierUseNode) String() string {
	if symbol := n.CurrentBlock().Lookup(n.Name); symbol != nil {
		format := identifierUseFormats[symbol.Kind]

		switch symbol.Kind {
		case sym.ConstantEntry:
			return fmt.Sprintf(format, n.Kind(), symbol.Kind, symbol.Name, symbol.Value, n.Use)

		case sym.VariableEntry, sym.ProcedureEntry:
			return fmt.Sprintf(format, n.Kind(), symbol.Kind, symbol.Name, n.Use)

		default:
			panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownSymbolKind, nil, nil))
		}
	}

	// if the symbol is not found, return a generic identifier-use string
	return fmt.Sprintf(unknownUseFormat, n.Kind(), n.Name, n.Use)
}

// Accept the visitor for the identifier-use node.
func (n *IdentifierUseNode) Accept(visitor Visitor) {
	visitor.VisitIdentifierUse(n)
}

// Find the current block node that contains this identifier-use node.
func (n *IdentifierUseNode) CurrentBlock() Block {
	return searchBlock(n, CurrentBlock)
}
