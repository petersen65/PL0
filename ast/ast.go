// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package ast implements the abstract syntax tree (AST) produced in the parsing compiler phase.
package ast

// Kind of nodes in the abstract syntax tree.
const (
	KindBlock NodeKind = iota
	KindConstantDeclaration
	KindVariableDeclaration
	KindFunctionDeclaration
	KindLiteral
	KindIdentifierUse
	KindUnaryOperation
	KindBinaryOperation
	KindComparisonOperation
	KindAssignmentStatement
	KindReadStatement
	KindWriteStatement
	KindCallStatement
	KindIfStatement
	KindWhileStatement
	KindCompoundStatement
)

// Traverse the abstract syntax tree in specific orders.
const (
	PreOrder TraversalOrder = iota
	InOrder
	PostOrder
	LevelOrder
)

type (
	// Kind of node in the abstract syntax tree.
	NodeKind int

	// Traversal order for the abstract syntax tree.
	TraversalOrder int

	// A node in the abstract syntax tree.
	Node interface {
		Kind() NodeKind
		Parent() Node
		SetParent(node Node)
		Children() []Node
		String() string
		Accept(visitor Visitor)
		Index() int
		CurrentBlock() Block
	}

	// A visitor is an interface for visiting nodes in the abstract syntax tree. It allows all the methods for a parser phase to be grouped in a single visitor struct.
	// The visitor design pattern allows implementing double dispatch for traversing the abstract syntax tree. Each parser phase method is chosen based on:
	//   - the dynamic type of the object (the AST node) determines the method to be called, and
	//   - the dynamic type of the argument (the visitor) determines the behavior of the method.
	Visitor interface {
		VisitBlock(block Block)
		VisitConstantDeclaration(declaration ConstantDeclaration)
		VisitVariableDeclaration(declaration VariableDeclaration)
		VisitFunctionDeclaration(declaration FunctionDeclaration)
		VisitLiteral(literal *LiteralNode)
		VisitIdentifierUse(use *IdentifierUseNode)
		VisitUnaryOperation(operation *UnaryOperationNode)
		VisitBinaryOperation(operation *BinaryOperationNode)
		VisitComparisonOperation(operation *ComparisonOperationNode)
		VisitAssignmentStatement(assignment *AssignmentStatementNode)
		VisitReadStatement(read *ReadStatementNode)
		VisitWriteStatement(write *WriteStatementNode)
		VisitCallStatement(call *CallStatementNode)
		VisitIfStatement(ifStmt *IfStatementNode)
		VisitWhileStatement(whileStmt *WhileStatementNode)
		VisitCompoundStatement(compound *CompoundStatementNode)
	}
)

// String representation of a node kind.
func (n NodeKind) String() string {
	return nodeKindNames[n]
}

// Walk traverses an abstract syntax tree in a specific order and calls the visitor or the visit function for each node.
func Walk(parent Node, order TraversalOrder, visitor any, visit func(node Node, visitor any)) error {
	return walk(parent, order, visitor, visit)
}
