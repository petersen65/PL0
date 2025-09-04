// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"fmt"
	"strings"

	eh "github.com/petersen65/pl0/v3/errors"
	sym "github.com/petersen65/pl0/v3/symbol"
)

// Separator for the string representation of a bit-mask.
const bitMaskSeparator = "|"

// Format for the string representation of an identifier-use node.
const identifierUseFormat = "use(kind=%v,name=%v,mode=%v)"

// Represents a single use of an identifier.
type identifierUseNode struct {
	expressionNode                // embedded expression node
	Identifier     string         `json:"name"`            // name of the identifier
	IdentifierKind IdentifierKind `json:"identifier_kind"` // kind of the identifier used
	Mode           UsageMode      `json:"usage_mode"`      // usage mode of the identifier
}

var (
	// Map identifier kinds to their string representation.
	identifierKindNames = map[IdentifierKind]string{
		Constant:  "constant",
		Variable:  "variable",
		Function:  "function",
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
func newIdentifierUse(name string, kind IdentifierKind, index int) IdentifierUse {
	return &identifierUseNode{
		expressionNode: expressionNode{
			commonNode:       commonNode{NodeKind: KindIdentifierUse},
			TokenStreamIndex: index,
		},
		Identifier:     name,
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
func (n *identifierUseNode) Children() []Node {
	return make([]Node, 0)
}

// String representation of the identifier-use node.
func (n *identifierUseNode) String() string {
	switch n.IdentifierKind {
	case Constant, Variable, Function, Procedure:
		return fmt.Sprintf(identifierUseFormat, n.IdentifierKind, n.Identifier, n.Mode)

	default:
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownIdentifierKind, n.IdentifierKind, nil))
	}
}

// Accept the visitor for the identifier-use node.
func (n *identifierUseNode) Accept(visitor Visitor) {
	visitor.VisitIdentifierUse(n)
}

// Find the current block node that contains this identifier-use node.
func (n *identifierUseNode) CurrentBlock() Block {
	return searchBlock(n, CurrentBlock)
}

// Determine if the identifier-use node represents a constant value.
func (n *identifierUseNode) IsConstant() bool {
	// an identifier use is constant if it is a constant identifier
	return n.IdentifierKind == Constant
}

// Block nesting depth of the identifier use.
func (n *identifierUseNode) Depth() int {
	return n.CurrentBlock().Depth()
}

// The name of the used identifier.
func (n *identifierUseNode) Name() string {
	return n.Identifier
}

// The context in which the identifier is used (e.g., constant or variable).
func (n *identifierUseNode) Context() IdentifierKind {
	return n.IdentifierKind
}

// Set the context in which the identifier is used.
func (n *identifierUseNode) SetContext(kind IdentifierKind) {
	n.IdentifierKind = kind
}

// The way how the identifier is used (e.g., read or write).
func (n *identifierUseNode) UsageMode() UsageMode {
	return n.Mode
}

// Set the way how the identifier is used.
func (n *identifierUseNode) SetUsageMode(mode UsageMode) {
	n.Mode = mode
}

// Find the declaration of the identifier used by this identifier-use node.
// An identifier use refers to a declaration if it is declared in the same block or in a lexical parent block.
// The used identifier might not have a declaration if it was never declared. In that case, nil is returned.
func (n *identifierUseNode) Declaration() Declaration {
	if symbol := n.CurrentBlock().Lookup(n.Name()); symbol == nil {
		return nil
	} else {
		return SearchDeclaration(n, symbol)
	}
}

// Find the symbol of the identifier used by this identifier-use node.
// The symbol might be nil if the identifier is used but was never declared.
func (n *identifierUseNode) Symbol() *sym.Symbol {
	if declaration := n.Declaration(); declaration == nil {
		return nil
	} else {
		return n.Declaration().Symbol()
	}
}
