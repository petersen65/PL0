// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import "fmt"

// Format for the string representation of a function or procedure declaration node.
const functionFormat = "declaration(%v,name=%v,used=%v)"

// The node represents a function or procedure declaration in the abstract syntax tree.
type functionDeclarationNode struct {
	declarationNode                         // embedded declaration node
	FunctionBlock      Block                `json:"function_block"` // block of the function
	FunctionParameters []*FunctionParameter `json:"parameters"`     // ordered list of parameters that the function accepts
	ReturnType         string               `json:"return_type"`    // type name of the value returned by the function (empty string for procedures)
}

// Create a new function or procedure declaration node in the abstract syntax tree.
func newFunctionDeclaration(name string, block Block, parameters []*FunctionParameter, returnTypeName string, index int) FunctionDeclaration {
	return &functionDeclarationNode{
		declarationNode: declarationNode{
			commonNode:       commonNode{NodeKind: KindFunctionDeclaration},
			Identifier:       name,
			DataType:         "",
			IdentifierUsage:  make([]Expression, 0),
			TokenStreamIndex: index,
		},
		FunctionBlock:      block,
		FunctionParameters: parameters,
		ReturnType:         returnTypeName,
	}
}

// Children nodes of the function declaration node.
func (n *functionDeclarationNode) Children() []Node {
	return []Node{n.FunctionBlock}
}

// String representation of the function declaration node.
func (n *functionDeclarationNode) String() string {
	return fmt.Sprintf(functionFormat, n.NodeKind, n.Identifier, len(n.IdentifierUsage))
}

// Accept the visitor for the function declaration node.
func (n *functionDeclarationNode) Accept(visitor Visitor) {
	visitor.VisitFunctionDeclaration(n)
}

// Find the current block node that contains this function declaration node.
func (n *functionDeclarationNode) CurrentBlock() Block {
	return searchBlock(n, CurrentBlock)
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
	return len(n.ReturnType) > 0
}

// The node represents a procedure declaration.
func (n *functionDeclarationNode) IsProcedure() bool {
	return len(n.ReturnType) == 0
}

// Parameter list of a function or procedure declaration.
func (n *functionDeclarationNode) Parameters() []*FunctionParameter {
	return n.FunctionParameters
}

// Return type name of a function declaration (empty string for procedure declarations).
func (n *functionDeclarationNode) ReturnTypeName() string {
	return n.ReturnType
}
