// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// Base structure for all statement nodes in the AST.
type statementNode struct {
	commonNode                // embedded common node
	TokenStreamIndexBegin int `json:"token_stream_index_begin"` // begin index of the token in the token stream
	TokenStreamIndexEnd   int `json:"token_stream_index_end"`   // end index of the token in the token stream
}

// Token stream begin index of the statement node.
func (n *statementNode) Index() int {
	return n.TokenStreamIndexBegin
}

// Token stream begin index and end index of the statement node.
func (n *statementNode) IndexPair() (int, int) {
	return n.TokenStreamIndexBegin, n.TokenStreamIndexEnd
}
