// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// Allows the detection of empty constants because of parsing errors. They should be ignored in all compiler phases.
const emptyConstantName = "@constant"

// Base structure for all declaration nodes in the AST.
type declarationNode struct {
	Name             string       `json:"name"`               // name of the declared identifier
	DataTypeName     string       `json:"data_type_name"`     // datatype name of the identifier
	IdentifierUsage  []Expression `json:"usage"`              // all usages of the identifier
	TokenStreamIndex int          `json:"token_stream_index"` // index of the token in the token stream
}

// Token stream index of the declaration node.
func (n *declarationNode) Index() int {
	return n.TokenStreamIndex
}

// All usages of the declared identifier in expressions.
func (n *declarationNode) Usage() []Expression {
	return n.IdentifierUsage
}
