// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"slices"

	sym "github.com/petersen65/pl0/v3/symbol"
)

// Allows the detection of empty constants because of parsing errors. They should be ignored in all compiler phases.
const emptyConstantName = "@constant"

// Base structure for all declaration nodes in the abstract syntax tree.
type declarationNode struct {
	Identifier        string       `json:"name"`               // name of the declared identifier
	DataType          string       `json:"data_type_name"`     // datatype name of the identifier
	IdentifierUsage   []Expression `json:"usage"`              // all usages of the identifier
	SymbolInformation *sym.Symbol  `json:"symbol"`             // symbol information of the declared identifier
	TokenStreamIndex  int          `json:"token_stream_index"` // index of the token in the token stream
}

// Name of the declared identifier.
func (n *declarationNode) Name() string {
	return n.Identifier
}

// Data type name of the declared identifier.
func (n *declarationNode) DataTypeName() string {
	return n.DataType
}

// Token stream index of the declaration node.
func (n *declarationNode) Index() int {
	return n.TokenStreamIndex
}

// Get the symbol information associated with the declaration node.
func (n *declarationNode) Symbol() *sym.Symbol {
	return n.SymbolInformation
}

// Associate the declaration node with its symbol information.
// The declaration node is created during syntax analysis and the symbol is created during semantic analysis.
func (n *declarationNode) SetSymbol(symbol *sym.Symbol) {
	n.SymbolInformation = symbol
}

// All usages of the declared identifier in expressions.
func (n *declarationNode) Usage() []Expression {
	return n.IdentifierUsage
}

// Add a usage of the declared identifier in an expression.
func (n *declarationNode) AddUsage(use Expression) {
	if !slices.Contains(n.IdentifierUsage, use) {
		n.IdentifierUsage = append(n.IdentifierUsage, use)
	}
}
