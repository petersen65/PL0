// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// Base structure for all expression nodes in the AST.
type expressionNode struct {
	TokenStreamIndex int `json:"token_stream_index"` // index of the token in the token stream
}

// Token stream index of the expression node.
func (n *expressionNode) Index() int {
	return n.TokenStreamIndex
}
