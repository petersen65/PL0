// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import tok "github.com/petersen65/pl0/v3/token"

type (
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

	// A statement represented as an abstract syntax tree.
	Statement interface {
		Node
		IndexPair() (int, int)
	}
)

// An empty statement does not generate code, should only be used in the context of parser errors, and is free from any side-effect.
func NewEmptyStatement() Statement {
	return newCompoundStatement(make([]Statement, 0), tok.NoTokenStreamIndex, tok.NoTokenStreamIndex)
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
