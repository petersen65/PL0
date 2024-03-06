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

	// An expression represented as an abstract syntax tree.
	Expression interface {
		ExpressionString() string
	}

	// A statement represented as an abstract syntax tree.
	Statement interface {
		StatementString() string
	}
)

// NewLiteral creates a new literal node in the abstract syntax tree.
func NewLiteral(value any, dataType DataType) Expression {
	return newLiteral(value, dataType)
}

// NewConstant creates a new constant node in the abstract syntax tree.
func NewConstant(symbol Symbol) Expression {
	return newConstant(symbol)
}

// NewVariable creates a new variable node in the abstract syntax tree.
func NewVariable(symbol Symbol) Expression {
	return newVariable(symbol)
}

// NewProcedure creates a new procedure node in the abstract syntax tree.
func NewProcedure(symbol Symbol, statement Statement) Statement {
	return newProcedure(symbol, statement)
}

// NewProgram creates the root program node in the abstract syntax tree.
func NewProgram(symbol Symbol, statement Statement) Statement {
	return newProgram(symbol, statement)
}

// NewProcedures creates a procedures statement node in the abstract syntax tree.
func NewProcedures(statements []Statement) Statement {
	return newProcedures(statements)
}

// NewUnaryOperation creates a new unary operation node in the abstract syntax tree.
func NewUnaryOperation(operation UnaryOperator, operand Expression) Expression {
	return newUnaryOperation(operation, operand)
}

// NewBinaryOperation creates a new binary operation node in the abstract syntax tree.
func NewBinaryOperation(operation BinaryOperator, left, right Expression) Expression {
	return newBinaryOperation(operation, left, right)
}

// NewConditionalOperation creates a new conditional operation node in the abstract syntax tree.
func NewConditionalOperation(operation RelationalOperator, left, right Expression) Expression {
	return newConditionalOperation(operation, left, right)
}

// NewAssignmentStatement creates a new assignment statement node in the abstract syntax tree.
func NewAssignmentStatement(symbol Symbol, expression Expression) Statement {
	return newAssignmentStatement(symbol, expression)
}

// NewReadStatement creates a new read statement node in the abstract syntax tree.
func NewReadStatement(symbol Symbol) Statement {
	return newReadStatement(symbol)
}

// NewWriteStatement creates a new write statement node in the abstract syntax tree.
func NewWriteStatement(expression Expression) Statement {
	return newWriteStatement(expression)
}

// NewCallStatement creates a new call statement node in the abstract syntax tree.
func NewCallStatement(symbol Symbol) Statement {
	return newCallStatement(symbol)
}

// NewIfStatement creates a new if-then statement node in the abstract syntax tree.
func NewIfStatement(condition Expression, statement Statement) Statement {
	return newIfStatement(condition, statement)
}

// NewWhileStatement creates a new while-do statement node in the abstract syntax tree.
func NewWhileStatement(condition Expression, statement Statement) Statement {
	return newWhileStatement(condition, statement)
}

// NewCompoundStatement creates a compound statement node in the abstract syntax tree.
func NewCompoundStatement(statements []Statement) Statement {
	return newCompoundStatement(statements)
}
