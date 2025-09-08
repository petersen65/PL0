// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"slices"

	ts "github.com/petersen65/pl0/v3/typesystem"
	sym "github.com/petersen65/pl0/v3/symbol"
)

// Allows the detection of empty constants because of parsing errors. They should be ignored in all compiler phases.
const emptyConstantName = "@constant"

// Base structure for all declaration nodes in the abstract syntax tree.
type declarationNode struct {
	commonNode                              // embedded common node
	IdentifierName_            string       `json:"identifier_name"`               // name of the declared identifier
	DataTypeName_              string       `json:"data_type_name"`                // datatype name of the identifier
	Usage_                     []Expression `json:"usage"`                         // all usages of the identifier in expressions
	Symbol_                    *sym.Symbol  `json:"symbol"`                        // symbol information of the declared identifier
	TokenStreamIndexIdentifier int          `json:"token_stream_index_identifier"` // identifier index of the token in the token stream
	TokenStreamIndexDataType   int          `json:"token_stream_index_data_type"`  // data type index of the token in the token stream
}

// Identifier name of the declared identifier.
func (n *declarationNode) IdentifierName() string {
	return n.IdentifierName_
}

// Data type name of the declared identifier.
func (n *declarationNode) DataTypeName() string {
	return n.DataTypeName_
}

// Data type descriptor of the declared identifier. Returns nil if the symbol is not set.
func (n *declarationNode) DataType() ts.TypeDescriptor {
	if n.Symbol_ != nil {
		return n.Symbol_.DataType
	}

	return nil
}

// Token stream identifier index of the declaration node.
func (n *declarationNode) Index() int {
	return n.TokenStreamIndexIdentifier
}

// Token stream identifier index and data type index of the declaration node.
func (n *declarationNode) IndexPair() (int, int) {
	return n.TokenStreamIndexIdentifier, n.TokenStreamIndexDataType
}

// Get the symbol information associated with the declaration node.
func (n *declarationNode) Symbol() *sym.Symbol {
	return n.Symbol_
}

// Associate the declaration node with its symbol information.
// The declaration node is created during syntax analysis and the symbol is created during semantic analysis.
// If the symbol has a data type and the declaration node does not have a data type name, it is set from the symbol.
func (n *declarationNode) SetSymbol(symbol *sym.Symbol) {
	n.Symbol_ = symbol

	if symbol != nil && n.DataTypeName_ == "" && symbol.DataType != nil {
		n.DataTypeName_ = symbol.DataType.String()
	}
}

// All usages of the declared identifier in expressions.
func (n *declarationNode) Usage() []Expression {
	return n.Usage_
}

// Add a usage of the declared identifier in an expression.
func (n *declarationNode) AddUsage(usage Expression) {
	if !slices.Contains(n.Usage_, usage) {
		n.Usage_ = append(n.Usage_, usage)
	}
}
