// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package parser

import "fmt"

const (
	_ = unaryOperator(iota)
	odd
	negate
)

const (
	_ = binaryOperator(iota)
	plus
	minus
	times
	divide
	becomes
)

const (
	_ = conditionalOperator(iota)
	equal
	notEqual
	less
	lessEqual
	greater
	greaterEqual
)

const (
	_ = dataType(iota)
	integer64
)

type (
	unaryOperator       int
	binaryOperator      int
	conditionalOperator int
	dataType            int

	expression interface {
		expression() string
	}

	condition interface {
		condition() string
	}

	statement interface {
		statement() string
	}

	program struct {
		symbol symbol
		main   statement
	}

	assignmentStatement struct {
		symbol     symbol
		expression expression
	}

	readStatement struct {
		symbol symbol
	}

	writeStatement struct {
		expression expression
	}

	callStatement struct {
		symbol symbol
	}

	ifStatement struct {
		condition     condition
		trueStatement statement
	}

	whileStatement struct {
		condition     condition
		trueStatement statement
	}

	beginStatement struct {
		statements []statement
	}

	literal struct {
		value    any
		dataType dataType
	}

	identifier struct {
		symbol symbol
	}

	binaryOperation struct {
		operation binaryOperator
		left      expression
		right     expression
	}

	conditionalOperation struct {
		operation conditionalOperator
		left      expression
		right     expression
	}
)

func newProgram(symbol symbol, statement statement) statement {
	return &program{
		symbol: symbol,
		main:   statement,
	}
}

func newAssignmentStatement(symbol symbol, expression expression) statement {
	return &assignmentStatement{
		symbol:     symbol,
		expression: expression,
	}
}

func newReadStatement(symbol symbol) statement {
	return &readStatement{
		symbol: symbol,
	}
}

func newWriteStatement(expression expression) statement {
	return &writeStatement{
		expression: expression,
	}
}

func newCallStatement(symbol symbol) statement {
	return &callStatement{
		symbol: symbol,
	}
}

func newIfStatement(condition condition, trueStatement statement) statement {
	return &ifStatement{
		condition:     condition,
		trueStatement: trueStatement,
	}
}

func newWhileStatement(condition condition, trueStatement statement) statement {
	return &whileStatement{
		condition:     condition,
		trueStatement: trueStatement,
	}
}

func newBeginStatement(statements []statement) statement {
	return &beginStatement{
		statements: statements,
	}
}

func newLiteral(value any, dataType dataType) expression {
	return &literal{
		value:    value,
		dataType: dataType,
	}
}

func newIdentifier(symbol symbol) expression {
	return &identifier{
		symbol: symbol,
	}
}

func newBinaryOperation(operation binaryOperator, left, right expression) expression {
	return &binaryOperation{
		operation: operation,
		left:      left,
		right:     right,
	}
}

func (l *literal) expression() string {
	return fmt.Sprint(l.value)
}

func (i *identifier) expression() string {
	return i.symbol.name
}

func (b *binaryOperation) expression() string {
	switch b.operation {
	case plus:
		return "addition"

	case minus:
		return "subtraction"

	case times:
		return "multiplication"

	case divide:
		return "division"

	case becomes:
		return "assignment"

	default:
		return "unknown binary operation"
	}
}

func (c *conditionalOperation) condition() string {
	switch c.operation {
	case equal:
		return "equal"

	case notEqual:
		return "not equal"

	case less:
		return "less than"

	case lessEqual:
		return "less than or equal"

	case greater:
		return "greater than"

	case greaterEqual:
		return "greater than or equal"

	default:
		return "unknown conditional operation"
	}
}

func (p *program) statement() string {
	return fmt.Sprintf("program %v", p.symbol.name)
}

func (as *assignmentStatement) statement() string {
	return fmt.Sprintf("assignment %v", as.symbol.name)
}

func (rs *readStatement) statement() string {
	return fmt.Sprintf("read %v", rs.symbol.name)
}

func (ws *writeStatement) statement() string {
	return "write"
}

func (cs *callStatement) statement() string {
	return fmt.Sprintf("call %v", cs.symbol.name)
}

func (is *ifStatement) statement() string {
	return "if then"
}

func (ws *whileStatement) statement() string {
	return "while do"
}

func (bs *beginStatement) statement() string {
	return "begin end"
}
