// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package ast implements the abstract syntax tree (AST) produced in the parsing compiler phase.
package ast

import (
	sym "github.com/petersen65/pl0/v3/symbol"
	tok "github.com/petersen65/pl0/v3/token"
	ts "github.com/petersen65/pl0/v3/typesystem"
)

// Kind of nodes in the abstract syntax tree.
const (
	KindBlock NodeKind = iota
	KindConstantDeclaration
	KindVariableDeclaration
	KindProcedureDeclaration
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

// Operators with one operand.
const (
	Odd UnaryOperator = iota
	Negate
)

// Operators with two operands.
const (
	Plus BinaryOperator = iota
	Minus
	Times
	Divide
)

// Operators for comparison.
const (
	Equal ComparisonOperator = iota
	NotEqual
	Less
	LessEqual
	Greater
	GreaterEqual
)

// Usage mode of an identifier as bit-mask.
const (
	Read Usage = 1 << iota
	Write
	Execute
)

// Search parent block nodes in the abstract syntax tree.
const (
	CurrentBlock BlockSearchMode = iota
	RootBlock
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

	// Take one operand and perform an operation on it.
	UnaryOperator int

	// Take two operands and perform an operation on them.
	BinaryOperator int

	// Take two operands and perform a comparison on them.
	ComparisonOperator int

	// Usage mode of an identifier (bit-mask).
	Usage uint64

	// Search mode for block nodes in the abstract syntax tree.
	BlockSearchMode int

	// Traversal order for the abstract syntax tree.
	TraversalOrder int

	// ConstantDeclaration node represents a constant declaration in the AST.
	ConstantDeclarationNode struct {
		commonNode          // embedded common node
		declarationNode     // embedded declaration node
		Value           any `json:"value"` // value of the constant
	}

	// VariableDeclaration node represents a variable declaration in the AST.
	VariableDeclarationNode struct {
		commonNode      // embedded common node
		declarationNode // embedded declaration node
	}

	// ProcedureDeclaration node represents a procedure declaration in the AST.
	ProcedureDeclarationNode struct {
		commonNode            // embedded common node
		declarationNode       // embedded declaration node
		Block           Block `json:"procedure_block"` // block of the procedure
	}

	// Literal node represents the usage of a literal value in the AST.
	LiteralNode struct {
		commonNode         // embedded common node
		expressionNode     // embedded expression node
		Value          any `json:"value"` // literal value
	}

	// IdentifierUseNode represents the usage of an identifier in the AST.
	IdentifierUseNode struct {
		commonNode               // embedded common node
		expressionNode           // embedded expression node
		Name           string    `json:"name"`    // name of the identifier
		Context        sym.Entry `json:"context"` // context of the identifier
		Use            Usage     `json:"use"`     // usage mode of the identifier
	}

	// UnaryOperation node represents a unary operation in the AST.
	UnaryOperationNode struct {
		commonNode                   // embedded common node
		expressionNode               // embedded expression node
		Operation      UnaryOperator `json:"operation"` // unary operation
		Operand        Expression    `json:"operand"`   // operand of the unary operation
	}

	// BinaryOperation node represents a binary operation in the AST.
	BinaryOperationNode struct {
		commonNode                    // embedded common node
		expressionNode                // embedded expression node
		Operation      BinaryOperator `json:"operation"` // binary operation
		Left           Expression     `json:"left"`      // left operand of the binary operation
		Right          Expression     `json:"right"`     // right operand of the binary operation
	}

	// ComparisonOperationNode node represents a comparison operation in the AST.
	ComparisonOperationNode struct {
		commonNode                        // embedded common node
		expressionNode                    // embedded expression node
		Operation      ComparisonOperator `json:"operation"` // comparison operation
		Left           Expression         `json:"left"`      // left operand of the comparison operation
		Right          Expression         `json:"right"`     // right operand of the comparison operation
	}

	// AssignmentStatement node represents an assignment statement in the AST.
	AssignmentStatementNode struct {
		commonNode
		statementNode
		Variable   Expression `json:"variable"`   // variable use on the left side of the assignment statement
		Expression Expression `json:"expression"` // expression on the right side of the assignment statement
	}

	// ReadStatement node represents a read statement in the AST.
	ReadStatementNode struct {
		commonNode
		statementNode
		Variable Expression `json:"variable"` // variable use of the read statement
	}

	// WriteStatement node represents a write statement in the AST.
	WriteStatementNode struct {
		commonNode
		statementNode
		Expression Expression `json:"expression"` // expression of the write statement
	}

	// CallStatement node represents a call statement in the AST.
	CallStatementNode struct {
		commonNode
		statementNode
		Procedure Expression `json:"procedure"` // procedure use of the call statement
	}

	// IfStatement node represents an if-then statement in the AST.
	IfStatementNode struct {
		commonNode
		statementNode
		Condition Expression `json:"condition"` // if-condition of the if-then statement
		Statement Statement  `json:"statement"` // then-statement of the if-then statement
	}

	// WhileStatement node represents a while-do statement in the AST.
	WhileStatementNode struct {
		commonNode
		statementNode
		Condition Expression `json:"condition"` // while-condition of the while-do statement
		Statement Statement  `json:"statement"` // do-statement of the while-do statement
	}

	// CompoundStatement node represents a begin-end statement in the AST.
	CompoundStatementNode struct {
		commonNode
		statementNode
		Statements []Statement `json:"statements"` // all statements of the begin-end compound statement
	}

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

	// A declaration represented as an abstract syntax tree.
	Declaration interface {
		Node
		Usage() []Expression
	}

	// An expression represented as an abstract syntax tree.
	Expression interface {
		Node
	}

	// A statement represented as an abstract syntax tree.
	Statement interface {
		Node
		IndexPair() (int, int)
	}

	// A visitor is an interface for visiting nodes in the abstract syntax tree. It allows all the methods for a parser phase to be grouped in a single visitor struct.
	// The visitor design pattern allows implementing double dispatch for traversing the abstract syntax tree. Each parser phase method is chosen based on:
	//   - the dynamic type of the object (the AST node) determines the method to be called, and
	//   - the dynamic type of the argument (the visitor) determines the behavior of the method.
	Visitor interface {
		VisitBlock(block Block)
		VisitConstantDeclaration(declaration *ConstantDeclarationNode)
		VisitVariableDeclaration(declaration *VariableDeclarationNode)
		VisitProcedureDeclaration(declaration *ProcedureDeclarationNode)
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

// An empty declaration is a 0 constant with special name, should only be used in the context of parser errors, and is free from any side-effect.
func NewEmptyDeclaration() Declaration {
	return newConstantDeclaration(emptyConstantName, ts.Integer64.String(), int64(0), tok.NoTokenStreamIndex)
}

// An empty expression is a 0 literal, should only be used in the context of parser errors, and is free from any side-effect.
func NewEmptyExpression() Expression {
	return newLiteral(int64(0), tok.NoTokenStreamIndex)
}

// An empty statement does not generate code, should only be used in the context of parser errors, and is free from any side-effect.
func NewEmptyStatement() Statement {
	return newCompoundStatement(make([]Statement, 0), tok.NoTokenStreamIndex, tok.NoTokenStreamIndex)
}

// NewConstantDeclaration creates a new constant declaration node in the abstract syntax tree.
func NewConstantDeclaration(name, dataTypeName string, value any, index int) Declaration {
	return newConstantDeclaration(name, dataTypeName, value, index)
}

// NewVariableDeclaration creates a new variable declaration node in the abstract syntax tree.
func NewVariableDeclaration(name, dataTypeName string, index int) Declaration {
	return newVariableDeclaration(name, dataTypeName, index)
}

// NewProcedureDeclaration creates a new procedure declaration node in the abstract syntax tree.
func NewProcedureDeclaration(name string, block Block, index int) Declaration {
	return newProcedureDeclaration(name, block, index)
}

// NewLiteral creates a new literal node in the abstract syntax tree.
func NewLiteral(value any, index int) Expression {
	return newLiteral(value, index)
}

// NewIdentifierUse creates a new identifier-use node in the abstract syntax tree.
func NewIdentifierUse(name string, context sym.Entry, index int) Expression {
	return newIdentifierUse(name, context, index)
}

// NewUnaryOperation creates a new unary operation node in the abstract syntax tree.
func NewUnaryOperation(operation UnaryOperator, operand Expression, index int) Expression {
	return newUnaryOperation(operation, operand, index)
}

// NewBinaryOperation creates a new binary operation node in the abstract syntax tree.
func NewBinaryOperation(operation BinaryOperator, left, right Expression, index int) Expression {
	return newBinaryOperation(operation, left, right, index)
}

// NewComparisonOperation creates a new comparison operation node in the abstract syntax tree.
func NewComparisonOperation(operation ComparisonOperator, left, right Expression, index int) Expression {
	return newComparisonOperation(operation, left, right, index)
}

// NewAssignmentStatement creates a new assignment statement node in the abstract syntax tree.
func NewAssignmentStatement(variable, expression Expression, beginIndex, endIndex int) Statement {
	return newAssignmentStatement(variable, expression, beginIndex, endIndex)
}

// NewReadStatement creates a new read statement node in the abstract syntax tree.
func NewReadStatement(variable Expression, beginIndex, endIndex int) Statement {
	return newReadStatement(variable, beginIndex, endIndex)
}

// NewWriteStatement creates a new write statement node in the abstract syntax tree.
func NewWriteStatement(expression Expression, beginIndex, endIndex int) Statement {
	return newWriteStatement(expression, beginIndex, endIndex)
}

// NewCallStatement creates a new call statement node in the abstract syntax tree.
func NewCallStatement(procedure Expression, beginIndex, endIndex int) Statement {
	return newCallStatement(procedure, beginIndex, endIndex)
}

// NewIfStatement creates a new if-then statement node in the abstract syntax tree.
func NewIfStatement(condition Expression, statement Statement, beginIndex, endIndex int) Statement {
	return newIfStatement(condition, statement, beginIndex, endIndex)
}

// NewWhileStatement creates a new while-do statement node in the abstract syntax tree.
func NewWhileStatement(condition Expression, statement Statement, beginIndex, endIndex int) Statement {
	return newWhileStatement(condition, statement, beginIndex, endIndex)
}

// NewCompoundStatement creates a compound statement node in the abstract syntax tree.
func NewCompoundStatement(statements []Statement, beginIndex, endIndex int) Statement {
	return newCompoundStatement(statements, beginIndex, endIndex)
}

// String representation of a node kind.
func (n NodeKind) String() string {
	return nodeKindNames[n]
}

// Walk traverses an abstract syntax tree in a specific order and calls the visitor or the visit function for each node.
func Walk(parent Node, order TraversalOrder, visitor any, visit func(node Node, visitor any)) error {
	return walk(parent, order, visitor, visit)
}
