// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import tok "github.com/petersen65/pl0/v3/token"

type (
	// An assignment statement node in the abstract syntax tree.
	AssignmentStatement interface {
		Statement
		Variable() Expression
		Expression() Expression
	}

	// A read statement node in the abstract syntax tree.
	ReadStatement interface {
		Statement
		Variable() Expression
	}

	// A write statement node in the abstract syntax tree.
	WriteStatement interface {
		Statement
		Expression() Expression
	}

	// A call statement node in the abstract syntax tree.
	CallStatement interface {
		Statement
		Function() Expression
	}

	// An if-then statement node in the abstract syntax tree.
	IfStatement interface {
		Statement
		Condition() Expression
		Statement() Statement
	}

	// A while-do statement node in the abstract syntax tree.
	WhileStatement interface {
		Statement
		Condition() Expression
		Statement() Statement
	}

	// A compound statement node in the abstract syntax tree.
	CompoundStatement interface {
		Statement
		Statements() []Statement
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

// Create a new assignment statement node in the abstract syntax tree.
func NewAssignmentStatement(variable, expression Expression, beginIndex, endIndex int) AssignmentStatement {
	return newAssignmentStatement(variable, expression, beginIndex, endIndex)
}

// Create a new read statement node in the abstract syntax tree.
func NewReadStatement(variable Expression, beginIndex, endIndex int) ReadStatement {
	return newReadStatement(variable, beginIndex, endIndex)
}

// Create a new write statement node in the abstract syntax tree.
func NewWriteStatement(expression Expression, beginIndex, endIndex int) WriteStatement {
	return newWriteStatement(expression, beginIndex, endIndex)
}

// Create a new call statement node in the abstract syntax tree.
func NewCallStatement(function Expression, beginIndex, endIndex int) CallStatement {
	return newCallStatement(function, beginIndex, endIndex)
}

// Create a new if-then statement node in the abstract syntax tree.
func NewIfStatement(condition Expression, statement Statement, beginIndex, endIndex int) IfStatement {
	return newIfStatement(condition, statement, beginIndex, endIndex)
}

// Create a new while-do statement node in the abstract syntax tree.
func NewWhileStatement(condition Expression, statement Statement, beginIndex, endIndex int) WhileStatement {
	return newWhileStatement(condition, statement, beginIndex, endIndex)
}

// Create a new compound statement node in the abstract syntax tree.
func NewCompoundStatement(statements []Statement, beginIndex, endIndex int) CompoundStatement {
	return newCompoundStatement(statements, beginIndex, endIndex)
}
