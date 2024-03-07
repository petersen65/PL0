// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

// Package ast implements the abstract syntax tree (AST) for the PL/0 parser.
package ast

// Operators with one operand.
const (
	_ = UnaryOperator(iota)
	Odd
	Negate
)

// Operators with two operands.
const (
	_ = BinaryOperator(iota)
	Plus
	Minus
	Times
	Divide
)

// Operators for comparison.
const (
	_ = RelationalOperator(iota)
	Equal
	NotEqual
	Less
	LessEqual
	Greater
	GreaterEqual
)

// Data types of symbols.
const (
	_ = DataType(iota)
	Integer64
)

// Kind of supported symbol table entry.
const (
	Constant = Entry(iota)
	Variable
	Procedure
)

type (
	// Take one operand and perform an operation on it.
	UnaryOperator int

	// Take two operands and perform an operation on them.
	BinaryOperator int

	// Take two operands and perform a comparison on them.
	RelationalOperator int

	// The data type of a symbol.
	DataType int

	// Kind of symbol table entries.
	Entry int

	// The Symbol table entry.
	Symbol struct {
		Name    string // name of constant, variable, or procedure
		Kind    Entry  // constant, variable, or procedure
		Depth   int32  // declaration nesting depth of constant, variable, or procedure
		Value   int64  // value of constant
		Offset  uint64 // offset of variable in its runtime procedure stack frame
		Address uint64 // absolute address of procedure in text section
	}

	// Describes a position in the source code.
	SourceDescription struct {
		Line, Column int
		CurrentLine  []byte
	}

	// A block represented as an abstract syntax tree.
	Block interface {
		BlockString() string
	}

	// An expression represented as an abstract syntax tree.
	Expression interface {
		ExpressionString() string
	}

	// A statement represented as an abstract syntax tree.
	Statement interface {
		StatementString() string
	}
)

// NewBlock creates a new block node in the abstract syntax tree.
func NewBlock(symbol Symbol, depth int32, declarations []Symbol, procedures []Block, statement Statement, source SourceDescription) Block {
	return newBlock(symbol, depth, declarations, procedures, statement, source)
}

// NewLiteral creates a new literal node in the abstract syntax tree.
func NewLiteral(value any, dataType DataType, source SourceDescription) Expression {
	return newLiteral(value, dataType, source)
}

// NewConstant creates a new constant node in the abstract syntax tree.
func NewConstant(symbol Symbol, source SourceDescription) Expression {
	return newConstant(symbol, source)
}

// NewVariable creates a new variable node in the abstract syntax tree.
func NewVariable(symbol Symbol, source SourceDescription) Expression {
	return newVariable(symbol, source)
}

// NewUnaryOperation creates a new unary operation node in the abstract syntax tree.
func NewUnaryOperation(operation UnaryOperator, operand Expression, source SourceDescription) Expression {
	return newUnaryOperation(operation, operand, source)
}

// NewBinaryOperation creates a new binary operation node in the abstract syntax tree.
func NewBinaryOperation(operation BinaryOperator, left, right Expression, source SourceDescription) Expression {
	return newBinaryOperation(operation, left, right, source)
}

// NewConditionalOperation creates a new conditional operation node in the abstract syntax tree.
func NewConditionalOperation(operation RelationalOperator, left, right Expression, source SourceDescription) Expression {
	return newConditionalOperation(operation, left, right, source)
}

// NewAssignmentStatement creates a new assignment statement node in the abstract syntax tree.
func NewAssignmentStatement(symbol Symbol, expression Expression, source SourceDescription) Statement {
	return newAssignmentStatement(symbol, expression, source)
}

// NewReadStatement creates a new read statement node in the abstract syntax tree.
func NewReadStatement(symbol Symbol, source SourceDescription) Statement {
	return newReadStatement(symbol, source)
}

// NewWriteStatement creates a new write statement node in the abstract syntax tree.
func NewWriteStatement(expression Expression, source SourceDescription) Statement {
	return newWriteStatement(expression, source)
}

// NewCallStatement creates a new call statement node in the abstract syntax tree.
func NewCallStatement(symbol Symbol, source SourceDescription) Statement {
	return newCallStatement(symbol, source)
}

// NewIfStatement creates a new if-then statement node in the abstract syntax tree.
func NewIfStatement(condition Expression, statement Statement, source SourceDescription) Statement {
	return newIfStatement(condition, statement, source)
}

// NewWhileStatement creates a new while-do statement node in the abstract syntax tree.
func NewWhileStatement(condition Expression, statement Statement, source SourceDescription) Statement {
	return newWhileStatement(condition, statement, source)
}

// NewCompoundStatement creates a compound statement node in the abstract syntax tree.
func NewCompoundStatement(statements []Statement, source SourceDescription) Statement {
	return newCompoundStatement(statements, source)
}
