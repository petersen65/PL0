// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package ast

type (
	// Block node represents a block in the AST.
	block struct {
		symbol       Symbol            // procedure symbol entry of the block
		depth        int32             // declaration nesting depth
		declarations []Symbol          // local declarations of the block
		procedures   []Block           // nested procedures of the block
		statement    Statement         // statement of the block
		source       SourceDescription // source description for the block node
	}

	// Literal node represents the usage of a literal value in the AST.
	literal struct {
		value    any               // literal value
		dataType DataType          // data type of the literal
		source   SourceDescription // source description for the literal node
	}

	// Constant node represents the usage of a constant in the AST.
	constant struct {
		symbol Symbol            // constant symbol entry
		source SourceDescription // source description for the constant node
	}

	// Variable node represents the usage of a variable in the AST.
	variable struct {
		symbol Symbol            // variable symbol entry
		source SourceDescription // source description for the variable node
	}

	// UnaryOperation node represents a unary operation in the AST.
	unaryOperation struct {
		operation UnaryOperator     // unary operation
		operand   Expression        // operand of the unary operation
		source    SourceDescription // source description for the unary operation node
	}

	// BinaryOperation node represents a binary operation in the AST.
	binaryOperation struct {
		operation BinaryOperator    // binary operation
		left      Expression        // left operand of the binary operation
		right     Expression        // right operand of the binary operation
		source    SourceDescription // source description for the binary operation node
	}

	// ConditionalOperation node represents a conditional operation in the AST.
	conditionalOperation struct {
		operation RelationalOperator // conditional operation
		left      Expression         // left operand of the conditional operation
		right     Expression         // right operand of the conditional operation
		source    SourceDescription  // source description for the conditional operation node
	}

	// AssignmentStatement node represents an assignment statement in the AST.
	assignmentStatement struct {
		symbol     Symbol            // variable symbol entry on the left side of the assignment statement
		expression Expression        // expression on the right side of the assignment statement
		source     SourceDescription // source description for the assignment statement node
	}

	// ReadStatement node represents a read statement in the AST.
	readStatement struct {
		symbol Symbol            // variable symbol entry of the read statement
		source SourceDescription // source description for the read statement node
	}

	// WriteStatement node represents a write statement in the AST.
	writeStatement struct {
		expression Expression        // expression of the write statement
		source     SourceDescription // source description for the write statement node
	}

	// CallStatement node represents a call statement in the AST.
	callStatement struct {
		symbol Symbol            // procedure symbol entry of the call statement
		source SourceDescription // source description for the call statement node
	}

	// IfStatement node represents an if-then statement in the AST.
	ifStatement struct {
		condition Expression        // if-condition of the if-then statement
		statement Statement         // then-statement of the if-then statement
		source    SourceDescription // source description for the if-then statement node
	}

	// WhileStatement node represents a while-do statement in the AST.
	whileStatement struct {
		condition Expression        // while-condition of the while-do statement
		statement Statement         // do-statement of the while-do statement
		source    SourceDescription // source description for the while-do statement node
	}

	// CompoundStatement node represents a begin-end statement in the AST.
	compoundStatement struct {
		statements []Statement       // all statements of the begin-end compound statement
		source     SourceDescription // source description for the begin-end statement node
	}
)

// Create a new block node in the abstract syntax tree.
func newBlock(symbol Symbol, depth int32, declarations []Symbol, procedures []Block, statement Statement, source SourceDescription) Block {
	return &block{
		symbol:       symbol,
		depth:        depth,
		declarations: declarations,
		procedures:   procedures,
		statement:    statement,
		source:       source,
	}
}

// Create a new literal node in the abstract syntax tree.
func newLiteral(value any, dataType DataType, source SourceDescription) Expression {
	return &literal{
		value:    value,
		dataType: dataType,
		source:   source,
	}
}

// Create a new constant node in the abstract syntax tree.
func newConstant(symbol Symbol, source SourceDescription) Expression {
	return &constant{
		symbol: symbol,
		source: source,
	}
}

// Create a new variable node in the abstract syntax tree.
func newVariable(symbol Symbol, source SourceDescription) Expression {
	return &variable{
		symbol: symbol,
		source: source,
	}
}

// Create a new unary operation node in the abstract syntax tree.
func newUnaryOperation(operation UnaryOperator, operand Expression, source SourceDescription) Expression {
	return &unaryOperation{
		operation: operation,
		operand:   operand,
		source:    source,
	}
}

// Create a new binary operation node in the abstract syntax tree.
func newBinaryOperation(operation BinaryOperator, left, right Expression, source SourceDescription) Expression {
	return &binaryOperation{
		operation: operation,
		left:      left,
		right:     right,
		source:    source,
	}
}

// Create a new conditional operation node in the abstract syntax tree.
func newConditionalOperation(operation RelationalOperator, left, right Expression, source SourceDescription) Expression {
	return &conditionalOperation{
		operation: operation,
		left:      left,
		right:     right,
		source:    source,
	}
}

// Create a new assignment statement node in the abstract syntax tree.
func newAssignmentStatement(symbol Symbol, expression Expression, source SourceDescription) Statement {
	return &assignmentStatement{
		symbol:     symbol,
		expression: expression,
		source:     source,
	}
}

// Create a new read statement node in the abstract syntax tree.
func newReadStatement(symbol Symbol, source SourceDescription) Statement {
	return &readStatement{
		symbol: symbol,
		source: source,
	}
}

// Create a new write statement node in the abstract syntax tree.
func newWriteStatement(expression Expression, source SourceDescription) Statement {
	return &writeStatement{
		expression: expression,
		source:     source,
	}
}

// Create a new call statement node in the abstract syntax tree.
func newCallStatement(symbol Symbol, source SourceDescription) Statement {
	return &callStatement{
		symbol: symbol,
		source: source,
	}
}

// Create a new if-then statement node in the abstract syntax tree.
func newIfStatement(condition Expression, statement Statement, source SourceDescription) Statement {
	return &ifStatement{
		condition: condition,
		statement: statement,
		source:    source,
	}
}

// Create a new while-do statement node in the abstract syntax tree.
func newWhileStatement(condition Expression, statement Statement, source SourceDescription) Statement {
	return &whileStatement{
		condition: condition,
		statement: statement,
		source:    source,
	}
}

// Create a new compound statement node in the abstract syntax tree.
func newCompoundStatement(statements []Statement, source SourceDescription) Statement {
	return &compoundStatement{
		statements: statements,
		source:     source,
	}
}
