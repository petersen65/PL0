// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package ast

import (
	"fmt"
	"strings"
)

type (
	// Literal node represents a literal value in the AST.
	literal struct {
		value    any
		dataType DataType
	}

	// Constant node represents a constant in the AST.
	constant struct {
		symbol Symbol
	}

	// Variable node represents a variable in the AST.
	variable struct {
		symbol Symbol
	}

	// Procedure node represents a procedure in the AST.
	procedure struct {
		symbol    Symbol
		statement Statement
	}

	// Program node represents the main program and root in the AST.
	program struct {
		symbol    Symbol
		statement Statement
	}

	// Procedures node represents a list of procedure nodes in the AST.
	procedures struct {
		statements []Statement
	}

	// UnaryOperation node represents a unary operation in the AST.
	unaryOperation struct {
		operation UnaryOperator
		operand   Expression
	}

	// BinaryOperation node represents a binary operation in the AST.
	binaryOperation struct {
		operation BinaryOperator
		left      Expression
		right     Expression
	}

	// ConditionalOperation node represents a conditional operation in the AST.
	conditionalOperation struct {
		operation RelationalOperator
		left      Expression
		right     Expression
	}

	// AssignmentStatement node represents an assignment statement in the AST.
	assignmentStatement struct {
		symbol     Symbol
		expression Expression
	}

	// ReadStatement node represents a read statement in the AST.
	readStatement struct {
		symbol Symbol
	}

	// WriteStatement node represents a write statement in the AST.
	writeStatement struct {
		expression Expression
	}

	// CallStatement node represents a call statement in the AST.
	callStatement struct {
		symbol Symbol
	}

	// IfStatement node represents an if-then statement in the AST.
	ifStatement struct {
		condition Expression
		statement Statement
	}

	// WhileStatement node represents a while-do statement in the AST.
	whileStatement struct {
		condition Expression
		statement Statement
	}

	// CompoundStatement node represents a begin-end statement in the AST.
	compoundStatement struct {
		statements []Statement
	}
)

// Create a new literal node in the abstract syntax tree.
func newLiteral(value any, dataType DataType) Expression {
	return &literal{
		value:    value,
		dataType: dataType,
	}
}

// Create a new constant node in the abstract syntax tree.
func newConstant(symbol Symbol) Expression {
	return &constant{
		symbol: symbol,
	}
}

// Create a new variable node in the abstract syntax tree.
func newVariable(symbol Symbol) Expression {
	return &variable{
		symbol: symbol,
	}
}

// Create a new procedure node in the abstract syntax tree.
func newProcedure(symbol Symbol, statement Statement) Statement {
	return &procedure{
		symbol:    symbol,
		statement: statement,
	}
}

// Create the root program node in the abstract syntax tree.
func newProgram(symbol Symbol, statement Statement) Statement {
	return &program{
		symbol:    symbol,
		statement: statement,
	}
}

// Create a new procedures node in the abstract syntax tree.
func newProcedures(statements []Statement) Statement {
	return &procedures{
		statements: statements,
	}
}

// Create a new unary operation node in the abstract syntax tree.
func newUnaryOperation(operation UnaryOperator, operand Expression) Expression {
	return &unaryOperation{
		operation: operation,
		operand:   operand,
	}
}

// Create a new binary operation node in the abstract syntax tree.
func newBinaryOperation(operation BinaryOperator, left, right Expression) Expression {
	return &binaryOperation{
		operation: operation,
		left:      left,
		right:     right,
	}
}

// Create a new conditional operation node in the abstract syntax tree.
func newConditionalOperation(operation RelationalOperator, left, right Expression) Expression {
	return &conditionalOperation{
		operation: operation,
		left:      left,
		right:     right,
	}
}

// Create a new assignment statement node in the abstract syntax tree.
func newAssignmentStatement(symbol Symbol, expression Expression) Statement {
	return &assignmentStatement{
		symbol:     symbol,
		expression: expression,
	}
}

// Create a new read statement node in the abstract syntax tree.
func newReadStatement(symbol Symbol) Statement {
	return &readStatement{
		symbol: symbol,
	}
}

// Create a new write statement node in the abstract syntax tree.
func newWriteStatement(expression Expression) Statement {
	return &writeStatement{
		expression: expression,
	}
}

// Create a new call statement node in the abstract syntax tree.
func newCallStatement(symbol Symbol) Statement {
	return &callStatement{
		symbol: symbol,
	}
}

// Create a new if-then statement node in the abstract syntax tree.
func newIfStatement(condition Expression, statement Statement) Statement {
	return &ifStatement{
		condition: condition,
		statement: statement,
	}
}

// Create a new while-do statement node in the abstract syntax tree.
func newWhileStatement(condition Expression, statement Statement) Statement {
	return &whileStatement{
		condition: condition,
		statement: statement,
	}
}

// Create a new compound statement node in the abstract syntax tree.
func newCompoundStatement(statements []Statement) Statement {
	return &compoundStatement{
		statements: statements,
	}
}

// ExpressionString returns the string representation of the literal node.
func (l *literal) ExpressionString() string {
	return fmt.Sprint(l.value)
}

// ExpressionString returns the string representation of the constant node.
func (c *constant) ExpressionString() string {
	return c.symbol.Name
}

// ExpressionString returns the string representation of the variable node.
func (v *variable) ExpressionString() string {
	return v.symbol.Name
}

// StatementString returns the string representation of the procedure node.
func (p *procedure) StatementString() string {
	return fmt.Sprintf("procedure %v", p.symbol.Name)
}

// StatementString returns the string representation of the root program node.
func (p *program) StatementString() string {
	return fmt.Sprintf("program %v", p.symbol.Name)
}

// StatementString returns the string representation of the procedures node.
func (p *procedures) StatementString() string {
	var builder strings.Builder	

	for _, s := range p.statements {
		builder.WriteString(s.StatementString())
	}

	return builder.String()
}

// ExpressionString returns the string representation of the unary operation node.
func (o *unaryOperation) ExpressionString() string {
	switch o.operation {
	case Odd:
		return "odd"

	case Negate:
		return "negate"

	default:
		return "unknown unary operation"
	}
}

// ExpressionString returns the string representation of the binary operation node.
func (o *binaryOperation) ExpressionString() string {
	switch o.operation {
	case Plus:
		return "addition"

	case Minus:
		return "subtraction"

	case Times:
		return "multiplication"

	case Divide:
		return "division"

	default:
		return "unknown binary operation"
	}
}

// ConditionString returns the string representation of the conditional operation node.
func (o *conditionalOperation) ExpressionString() string {
	switch o.operation {
	case Equal:
		return "equal"

	case NotEqual:
		return "not equal"

	case Less:
		return "less than"

	case LessEqual:
		return "less than or equal"

	case Greater:
		return "greater than"

	case GreaterEqual:
		return "greater than or equal"

	default:
		return "unknown conditional operation"
	}
}

// StatementString returns the string representation of the assignment statement node.
func (s *assignmentStatement) StatementString() string {
	return fmt.Sprintf("assignment %v", s.symbol.Name)
}

// StatementString returns the string representation of the read statement node.
func (s *readStatement) StatementString() string {
	return fmt.Sprintf("read %v", s.symbol.Name)
}

// StatementString returns the string representation of the write statement node.
func (s *writeStatement) StatementString() string {
	return "write"
}

// StatementString returns the string representation of the call statement node.
func (s *callStatement) StatementString() string {
	return fmt.Sprintf("call %v", s.symbol.Name)
}

// StatementString returns the string representation of the if-then statement node.
func (s *ifStatement) StatementString() string {
	return "if then"
}

// StatementString returns the string representation of the while-do statement node.
func (s *whileStatement) StatementString() string {
	return "while do"
}

// StatementString returns the string representation of the compound statement node.
func (s *compoundStatement) StatementString() string {
	return "begin end"
}
