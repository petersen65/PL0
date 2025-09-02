// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"fmt"
	"strings"

	eh "github.com/petersen65/pl0/v3/errors"
)

// Separator for the string representation of a bit-mask.
const bitMaskSeparator = "|"

// Format for the string representation of an identifier-use node.
const identifierUseFormat = "%v(name=%v,usage=%v)"

var (
	// Map identifier kinds to their string representation.
	identifierKindNames = map[IdentifierKind]string{
		Constant:  "constant",
		Variable:  "variable",
		Procedure: "procedure",
	}

	// Map usage modes to their string representation.
	usageModeNames = map[UsageMode]string{
		Read:    "read",
		Write:   "write",
		Execute: "execute",
	}
)

// Create a new identifier-use node for the abstract syntax tree.
func newIdentifierUse(name string, kind IdentifierKind, index int) Expression {
	return &IdentifierUseNode{
		commonNode:     commonNode{NodeKind: KindIdentifierUse},
		expressionNode: expressionNode{TokenStreamIndex: index},
		Name:           name,
		IdentifierKind: kind,
	}
}

// String representation of an identifier kind bit-mask.
func (u IdentifierKind) String() string {
	var parts []string

	for kind, name := range identifierKindNames {
		if u&kind != 0 {
			parts = append(parts, name)
		}
	}

	return strings.Join(parts, bitMaskSeparator)
}

// String representation of a usage mode bit-mask.
func (u UsageMode) String() string {
	var parts []string

	for usage, name := range usageModeNames {
		if u&usage != 0 {
			parts = append(parts, name)
		}
	}

	return strings.Join(parts, bitMaskSeparator)
}

// Children nodes of the identifier-use node.
func (n *IdentifierUseNode) Children() []Node {
	return make([]Node, 0)
}

// String representation of the identifier-use node.
func (n *IdentifierUseNode) String() string {
	switch n.IdentifierKind {
	case Constant, Variable, Procedure:
		return fmt.Sprintf(identifierUseFormat, n.IdentifierKind, n.Name, n.UsageMode)

	default:
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownIdentifierKind, n.IdentifierKind, nil))
	}
}

// Accept the visitor for the identifier-use node.
func (n *IdentifierUseNode) Accept(visitor Visitor) {
	visitor.VisitIdentifierUse(n)
}

// Find the current block node that contains this identifier-use node.
func (n *IdentifierUseNode) CurrentBlock() Block {
	return searchBlock(n, CurrentBlock)
}
