// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"fmt"

	eh "github.com/petersen65/pl0/v3/errors"
	sym "github.com/petersen65/pl0/v3/symbol"
)

// Format for the string representation of unknown identifier-use nodes.
const unknownUseFormat = "use(kind=unknown,name=%v,usage=%v)"

// Formats for the string representation of identifier-use nodes.
var identifierUseFormats = map[sym.Entry]string{
	sym.ConstantEntry:  "use(kind=%v,name=%v,value=%v,usage=%v)",
	sym.VariableEntry:  "use(kind=%v,name=%v,usage=%v)",
	sym.ProcedureEntry: "use(kind=%v,name=%v,usage=%v)",
}

// Create a new identifier-use node in the abstract syntax tree.
func newIdentifierUse(name string, scope sym.Scope[Declaration], context sym.Entry, index int) Expression {
	return &IdentifierUseNode{
		CommonNode:     CommonNode{NodeKind: KindIdentifierUse},
		ExpressionNode: ExpressionNode{Scope: scope, TokenStreamIndex: index},
		Name:           name,
		Context:        context,
	}
}

// Children nodes of the identifier-use node.
func (u *IdentifierUseNode) Children() []Node {
	return make([]Node, 0)
}

// String of the identifier-use node.
func (u *IdentifierUseNode) String() string {
	if symbol := u.Scope.Lookup(u.Name); symbol != nil {
		switch symbol.Kind {
		case sym.ConstantEntry:
			return fmt.Sprintf(identifierUseFormats[sym.ConstantEntry], symbol.Kind, symbol.Name, symbol.Declaration.(*ConstantDeclarationNode).Value, u.Use)

		case sym.VariableEntry:
			return fmt.Sprintf(identifierUseFormats[sym.VariableEntry], symbol.Kind, symbol.Name, u.Use)

		case sym.ProcedureEntry:
			return fmt.Sprintf(identifierUseFormats[sym.ProcedureEntry], symbol.Kind, symbol.Name, u.Use)

		default:
			panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownSymbolKind, nil, nil))
		}
	}

	// if the symbol is not found, return a generic identifier-use string
	return fmt.Sprintf(unknownUseFormat, u.Name, u.Use)
}

// Accept the visitor for the identifier-use node.
func (u *IdentifierUseNode) Accept(visitor Visitor) {
	visitor.VisitIdentifierUse(u)
}
