// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"fmt"
	"strings"

	eh "github.com/petersen65/pl0/v3/errors"
	sym "github.com/petersen65/pl0/v3/symbol"
	ts "github.com/petersen65/pl0/v3/typesystem"
)

// Separator for the string representation of a bit-mask.
const bitMaskSeparator = "|"

// Format for the string representation of an identifier-use node.
const identifierUseFormat = "use(kind=%v,name=%v,mode=%v)"

// Represents a single use of an identifier.
type identifierUseNode struct {
	expressionNode                 // embedded expression node
	IdentifierName_ string         `json:"identifier_name"` // name of the identifier
	IdentifierKind_ IdentifierKind `json:"identifier_kind"` // kind of the identifier used
	UsageMode_      UsageMode      `json:"usage_mode"`      // usage mode of the identifier
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
		IdentifierName_: name,
		IdentifierKind_: kind,
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
	switch n.IdentifierKind_ {
	case Constant, Variable, Function, Procedure:
		return fmt.Sprintf(identifierUseFormat, n.IdentifierKind_, n.IdentifierName_, n.UsageMode_)

	default:
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownIdentifierKind, nil, n.IdentifierKind_))
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
	return n.IdentifierKind_ == Constant
}

// Determine the value of the identifier used by this identifier-use node.
// The used identifier might not have a value if it was never declared or if it is not a constant. In that case, nil is returned.
func (n *identifierUseNode) Value() any {
	declaration := n.Declaration()

	// there is no declaration for the used identifier
	if declaration == nil {
		return nil
	}

	symbol := declaration.Symbol()

	// there is no symbol for the declaration or the declaration is not a constant
	if symbol == nil || symbol.Kind != sym.ConstantEntry || !n.IsConstant() {
		return nil
	}

	// return the value of the constant symbol entry
	return symbol.Value
}

// Block nesting depth of the identifier use.
func (n *identifierUseNode) Depth() int {
	return n.CurrentBlock().Depth()
}

// The name of the used identifier.
func (n *identifierUseNode) IdentifierName() string {
	return n.IdentifierName_
}

// The kind of the used identifier (e.g., constant, variable, function, or procedure).
func (n *identifierUseNode) IdentifierKind() IdentifierKind {
	return n.IdentifierKind_
}

// Set the kind of the used identifier.
func (n *identifierUseNode) SetIdentifierKind(kind IdentifierKind) {
	n.IdentifierKind_ = kind
}

// The way how the identifier is used (e.g., read or write).
func (n *identifierUseNode) UsageMode() UsageMode {
	return n.UsageMode_
}

// Set the way how the identifier is used.
func (n *identifierUseNode) SetUsageMode(mode UsageMode) {
	n.UsageMode_ = mode
}

// Determine the declaration of the identifier used by this identifier-use node.
// The used identifier might not have a declaration if it was never declared. In that case, nil is returned.
// An identifier use refers to a declaration if it is declared in the same block or in a lexical parent block.
func (n *identifierUseNode) Declaration() Declaration {
	if symbol := n.CurrentBlock().Lookup(n.IdentifierName()); symbol == nil {
		return nil
	} else {
		return SearchDeclaration(n, symbol)
	}
}

// Determine the data type of the identifier used by this identifier-use node.
// The used identifier might not have a data type if it was never declared. In that case, nil is returned.
func (n *identifierUseNode) DataType() ts.TypeDescriptor {
	if declaration := n.Declaration(); declaration != nil {
		return declaration.DataType()
	}

	return nil
}
