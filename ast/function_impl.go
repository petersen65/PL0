// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"fmt"

	ts "github.com/petersen65/pl0/v3/typesystem"
)

// Format for the string representation of a function or procedure declaration node.
const functionFormat = "declaration(%v,name=%v,type=%v,used=%v)"

// The node represents a function or procedure declaration in the abstract syntax tree.
type functionDeclarationNode struct {
	declarationNode                            // embedded declaration node
	FunctionBlock      Block                   `json:"function_block"`      // block of the function
	FunctionParameters []*ts.FunctionParameter `json:"function_parameters"` // ordered list of parameters that the function accepts
	ReturnTypeName_    string                  `json:"return_type_name"`    // type name of the value returned by the function (empty string for procedures)
}

// Create a new function or procedure declaration node in the abstract syntax tree.
func newFunctionDeclaration(identifierName string, block Block, parameters []*ts.FunctionParameter, returnTypeName string, index int) FunctionDeclaration {
	return &functionDeclarationNode{
		declarationNode: declarationNode{
			commonNode:                 commonNode{NodeKind: KindFunctionDeclaration},
			IdentifierName_:            identifierName,
			Usage_:                     make([]Expression, 0),
			TokenStreamIndexIdentifier: index,
		},
		FunctionBlock:      block,
		FunctionParameters: parameters,
		ReturnTypeName_:    returnTypeName,
	}
}

// Children nodes of the function declaration node.
func (n *functionDeclarationNode) Children() []Node {
	return []Node{n.FunctionBlock}
}

// String representation of the function declaration node.
func (n *functionDeclarationNode) String() string {
	return fmt.Sprintf(functionFormat, n.NodeKind, n.IdentifierName_, n.DataTypeName_, len(n.Usage_))
}

// Accept the visitor for the function declaration node.
func (n *functionDeclarationNode) Accept(visitor Visitor) {
	visitor.VisitFunctionDeclaration(n)
}

// Find the current block node that contains this function declaration node.
func (n *functionDeclarationNode) CurrentBlock() Block {
	return searchBlock(n, CurrentBlock)
}

// Block nesting depth of the function declaration.
func (n *functionDeclarationNode) Depth() int {
	return n.CurrentBlock().Depth()
}

// Get the block of the function declaration node.
func (n *functionDeclarationNode) Block() Block {
	return n.FunctionBlock
}

// Set the block of the function declaration node.
func (n *functionDeclarationNode) SetBlock(block Block) {
	n.FunctionBlock = block
}

// The node represents a function declaration.
func (n *functionDeclarationNode) IsFunction() bool {
	return len(n.ReturnTypeName_) > 0
}

// The node represents a procedure declaration.
func (n *functionDeclarationNode) IsProcedure() bool {
	return len(n.ReturnTypeName_) == 0
}

// Parameter list of a function or procedure declaration.
func (n *functionDeclarationNode) Parameters() []*ts.FunctionParameter {
	return n.FunctionParameters
}

// Return type name of a function declaration (empty string for procedure declarations).
func (n *functionDeclarationNode) ReturnTypeName() string {
	return n.ReturnTypeName_
}

// The data type name of the function declaration will be set by the semantic analyzer after a data type for the function or procedure has been determined.
func (n *functionDeclarationNode) SetDataTypeName(dataTypeName string) {
	n.DataTypeName_ = dataTypeName
}
