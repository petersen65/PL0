// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package parser

import (
	"strconv"

	ast "github.com/petersen65/PL0/ast"
	scn "github.com/petersen65/PL0/scanner"
)

// Number of bits of a signed integer.
const integerBitSize = 64

// Private implementation of the recursive descent PL/0 parser.
type parser struct {
	declarationDepth int32         // declaration depth of nested blocks
	tokenHandler     *tokenHandler // token handler that manages the tokens of the token stream
	abstractSyntax   ast.Block     // abstract syntax tree of the program
}

// Return the public interface of the private parser implementation.
func newParser() Parser {
	return &parser{}
}

// Run the recursive descent parser to map the token stream to its corresponding abstract syntax tree.
func (p *parser) Parse(tokenStream scn.TokenStream) (ast.Block, ErrorReport, error) {
	if err := p.reset(tokenStream); err != nil {
		return nil, p.tokenHandler.getErrorReport(), err
	}

	// the main block starts with the
	//   declaration of constants, variables and procedures
	//   followed by a statement
	//   and ends with the program-end
	p.abstractSyntax = p.block(EntryPointName, nil, set(declarations, statements, scn.ProgramEnd))

	// the program must end with a specific token
	if p.lastToken() != scn.ProgramEnd {
		p.appendError(expectedPeriod, p.lastTokenName())
	}

	// the program must comply with the syntax rules of the programming language
	if !p.tokenHandler.isFullyParsed() {
		p.tokenHandler.setFullyParsed()
		p.appendError(notFullyParsed, nil)
	}

	// collect all errors from the parser and return the error report
	errorReport := p.tokenHandler.getErrorReport()

	if len(errorReport) == 1 {
		return nil, errorReport, p.tokenHandler.error(parsingError, nil)
	} else if len(errorReport) > 1 {
		return nil, errorReport, p.tokenHandler.error(parsingErrors, len(errorReport))
	} else {
		return p.abstractSyntax, errorReport, nil
	}
}

// Reset the parser to its initial state so that it can be reused.
func (p *parser) reset(tokenStream scn.TokenStream) error {
	p.declarationDepth = 0
	p.tokenHandler = newTokenHandler(tokenStream)
	p.abstractSyntax = nil

	if len(tokenStream) == 0 || !p.nextToken() {
		return p.tokenHandler.error(eofReached, nil)
	}

	return nil
}

// A block is a sequence of declarations followed by a statement. The statement runs within its own stack frame.
func (p *parser) block(name string, outer *ast.Scope, expected scn.Tokens) ast.Block {
	var scope = ast.NewScope(outer)

	// block of main program with outermost scope that has no further outer scope
	if outer == nil {
		// a program starts with a block of declaration depth 0 and an entrypoint address 0
		scope.Insert(&ast.Symbol{
			Name:    name,
			Kind:    ast.Procedure,
			Depth:   0,
			Address: 0,
		})
	}

	// a block can contain a sequence of procedures, so the list of procedures is initialized
	procedures := make([]ast.Block, 0)

	if p.declarationDepth > blockNestingMax {
		p.appendError(maxBlockDepth, p.declarationDepth)
	}

	// declare all constants, variables and procedures of the block to fill up the symbol table
	for {
		if p.lastToken() == scn.ConstWord {
			p.constWord(scope)
		}

		if p.lastToken() == scn.VarWord {
			p.varWord(scope)
		}

		if p.lastToken() == scn.ProcedureWord {
			procedures = p.procedureWord(scope, expected)
		}

		// after declarations, the block expects
		//   a statement which also can be an assignment starting with an identifier
		//   or the parser would fall back to declarations as anchor in the case of a syntax error
		p.tokenHandler.rebase(expectedStatementsIdentifiers, set(statements, scn.Identifier), declarations)

		if !p.lastToken().In(declarations) {
			break
		}
	}

	// parse and emit all statement instructions which are defining the code logic of the block
	//   or the parser forwards to all expected tokens as anchors in the case of a syntax error
	statement := p.statement(scope, set(expected, scn.Semicolon, scn.EndWord))

	// after the block ends
	//   a semicolon is expected to separate the block from the parent block
	//   or a program-end is expected to end the program
	//   or the parser would forward to all expected tokens as anchors in the case of a syntax error
	p.tokenHandler.rebase(unexpectedTokens, expected, scn.Empty)

	// return a new block node in the abstract syntax tree
	return ast.NewBlock(name, p.declarationDepth, scope, procedures, statement)
}

// Sequence of constants declarations.
func (p *parser) constWord(scope *ast.Scope) {
	p.nextToken()

	for {
		p.constantIdentifier(scope)

		for p.lastToken() == scn.Comma {
			p.nextToken()
			p.constantIdentifier(scope)
		}

		if p.lastToken() == scn.Semicolon {
			p.nextToken()
		} else {
			p.appendError(expectedSemicolon, p.lastTokenName())
		}

		if p.lastToken() != scn.Identifier {
			break
		}
	}
}

// Sequence of variable declarations.
func (p *parser) varWord(scope *ast.Scope) {
	p.nextToken()

	for {
		p.variableIdentifier(scope)

		for p.lastToken() == scn.Comma {
			p.nextToken()
			p.variableIdentifier(scope)
		}

		if p.lastToken() == scn.Semicolon {
			p.nextToken()
		} else {
			p.appendError(expectedSemicolon, p.lastTokenName())
		}

		if p.lastToken() != scn.Identifier {
			break
		}
	}
}

// Sequence of procedure declarations.
func (p *parser) procedureWord(outer *ast.Scope, anchors scn.Tokens) []ast.Block {
	procedures := make([]ast.Block, 0)

	for p.lastToken() == scn.ProcedureWord {
		p.nextToken()
		procedureName := p.procedureIdentifier(outer)

		if p.lastToken() == scn.Semicolon {
			p.nextToken()
		} else {
			p.appendError(expectedSemicolon, p.lastTokenName())
		}

		// the procedure block gets anchor tokens from the parent block and starts with the
		//   declaration of constants, variables and procedures
		//   followed by a statement
		//   and ends with a semicolon
		p.declarationDepth++
		block := p.block(procedureName, outer, set(anchors, scn.Semicolon))
		p.declarationDepth--

		// after the procedure block ends a semicolon is expected to separate
		//   the block from the parent block
		//   or from the next procedure declaration
		if p.lastToken() == scn.Semicolon {
			p.nextToken()

			// after the procedure block, the parser expects
			//   a statement which also can be an assignment starting with an identifier
			//   the beginning of a new procedure declaration
			//   or the parser would fall back to parent tokens as anchors in the case of a syntax error
			p.tokenHandler.rebase(expectedStatementsIdentifiersProcedures, set(statements, scn.Identifier, scn.ProcedureWord), anchors)
		} else {
			p.appendError(expectedSemicolon, p.lastTokenName())
		}

		// add the procedure block node to the list of procedures for the parent block node
		procedures = append(procedures, block)
	}

	return procedures
}

// An assignment is an identifier followed by becomes followed by an expression.
func (p *parser) assignment(scope *ast.Scope, anchors scn.Tokens) ast.Statement {
	symbol := scope.Lookup(p.lastTokenValue())

	if symbol == nil {
		p.appendError(identifierNotFound, p.lastTokenValue())
	} else if symbol.Kind != ast.Variable {
		p.appendError(expectedVariableIdentifier, ast.KindNames[symbol.Kind])
	}

	p.nextToken()

	if p.lastToken() == scn.Becomes {
		p.nextToken()
	} else {
		p.appendError(expectedBecomes, p.lastTokenName())
	}

	right := p.expression(scope, anchors)

	if symbol == nil || symbol.Kind != ast.Variable {
		return nil
	}

	return ast.NewAssignmentStatement(symbol, right)
}

// A read statement is the read operator followed by an identifier that must be a variable.
func (p *parser) read(scope *ast.Scope) ast.Statement {
	var symbol *ast.Symbol
	p.nextToken()

	if p.lastToken() != scn.Identifier {
		p.appendError(expectedIdentifier, p.lastTokenName())
	} else {
		if entry := scope.Lookup(p.lastTokenValue()); entry != nil {
			if entry.Kind == ast.Variable {
				symbol = entry
			} else {
				p.appendError(expectedVariableIdentifier, ast.KindNames[entry.Kind])
			}
		} else {
			p.appendError(identifierNotFound, p.lastTokenValue())
		}
	}

	p.nextToken()

	if symbol != nil {
		return ast.NewReadStatement(symbol)
	}

	return nil
}

// A write statement is the write operator followed by an expression.
func (p *parser) write(scope *ast.Scope, anchors scn.Tokens) ast.Statement {
	p.nextToken()
	expression := p.expression(scope, anchors)
	return ast.NewWriteStatement(expression)
}

// A call statement is the call word followed by a procedure identifier.
func (p *parser) callWord(scope *ast.Scope) ast.Statement {
	var symbol *ast.Symbol
	p.nextToken()

	if p.lastToken() != scn.Identifier {
		p.appendError(expectedIdentifier, p.lastTokenName())
	} else {
		if entry := scope.Lookup(p.lastTokenValue()); entry != nil {
			if entry.Kind == ast.Procedure {
				symbol = entry
			} else {
				p.appendError(expectedProcedureIdentifier, ast.KindNames[entry.Kind])
			}
		} else {
			p.appendError(identifierNotFound, p.lastTokenValue())
		}

		p.nextToken()
	}

	if symbol != nil {
		return ast.NewCallStatement(symbol)
	}

	return nil
}

// An if statement is the if word followed by a condition followed by the then word followed by a statement.
func (p *parser) ifWord(scope *ast.Scope, anchors scn.Tokens) ast.Statement {
	p.nextToken()
	condition := p.condition(scope, set(anchors, scn.ThenWord, scn.DoWord))

	if p.lastToken() == scn.ThenWord {
		p.nextToken()
	} else {
		p.appendError(expectedThen, p.lastTokenName())
	}

	// parse the statement which is executed if the condition is true
	statement := p.statement(scope, anchors)
	return ast.NewIfStatement(condition, statement)
}

// A while statement is the while word followed by a condition followed by the do word followed by a statement.
func (p *parser) whileWord(scope *ast.Scope, anchors scn.Tokens) ast.Statement {
	p.nextToken()
	condition := p.condition(scope, set(anchors, scn.DoWord))

	if p.lastToken() == scn.DoWord {
		p.nextToken()
	} else {
		p.appendError(expectedDo, p.lastTokenName())
	}

	// parse the statement which is executed as long as the condition is true
	statement := p.statement(scope, anchors)
	return ast.NewWhileStatement(condition, statement)
}

// A begin-end statement is the begin word followed by a statements with semicolons followed by the end word.
func (p *parser) beginWord(scope *ast.Scope, anchors scn.Tokens) ast.Statement {
	compound := make([]ast.Statement, 0)
	p.nextToken()

	// the first statement of a begin-end block
	if statement := p.statement(scope, set(anchors, scn.EndWord, scn.Semicolon)); statement != nil {
		compound = append(compound, statement)
	}

	for p.lastToken().In(set(statements, scn.Semicolon)) {
		if p.lastToken() == scn.Semicolon {
			p.nextToken()
		} else {
			p.appendError(expectedSemicolon, p.lastTokenName())
		}

		// the next statement of a begin-end block
		if statement := p.statement(scope, set(anchors, scn.EndWord, scn.Semicolon)); statement != nil {
			compound = append(compound, statement)
		}
	}

	if p.lastToken() == scn.EndWord {
		p.nextToken()
	} else {
		p.appendError(expectedEnd, p.lastTokenName())
	}

	return ast.NewCompoundStatement(compound)
}

// A constant identifier is an identifier followed by an equal sign followed by a number to be stored in the symbol table.
func (p *parser) constantIdentifier(scope *ast.Scope) {
	if p.lastToken() != scn.Identifier {
		p.appendError(expectedIdentifier, p.lastTokenName())
		return
	}

	constantName := p.lastTokenValue()

	if scope.Lookup(constantName) != nil {
		p.appendError(identifierAlreadyDeclared, constantName)
	}

	p.nextToken()

	if p.lastToken().In(set(scn.Equal, scn.Becomes)) {
		if p.lastToken() == scn.Becomes {
			p.appendError(expectedEqual, p.lastTokenName())
		}

		p.nextToken()
		var sign scn.Token

		if p.lastToken() == scn.Plus || p.lastToken() == scn.Minus {
			sign = p.lastToken()
			p.nextToken()
		}

		if p.lastToken() != scn.Number {
			p.appendError(expectedNumber, p.lastTokenName())
		} else {
			scope.Insert(&ast.Symbol{
				Name:  constantName,
				Kind:  ast.Constant,
				Depth: p.declarationDepth,
				Value: p.numberValue(sign, p.lastTokenValue())})

			p.nextToken()
		}
	} else {
		p.appendError(expectedEqual, p.lastTokenName())
	}
}

// A variable identifier is an identifier to be stored in the symbol table.
func (p *parser) variableIdentifier(scope *ast.Scope) {
	if p.lastToken() != scn.Identifier {
		p.appendError(expectedIdentifier, p.lastTokenName())
	} else {
		if scope.Lookup(p.lastTokenValue()) != nil {
			p.appendError(identifierAlreadyDeclared, p.lastTokenValue())
		} else {
			scope.Insert(&ast.Symbol{
				Name:  p.lastTokenValue(),
				Kind:  ast.Variable,
				Depth: p.declarationDepth})
		}

		p.nextToken()
	}
}

// A procedure identifier is an identifier to be stored in the symbol table.
func (p *parser) procedureIdentifier(scope *ast.Scope) string {
	var procedureName string

	if p.lastToken() != scn.Identifier {
		p.appendError(expectedIdentifier, p.lastTokenName())
	} else {
		if scope.Lookup(p.lastTokenValue()) != nil {
			p.appendError(identifierAlreadyDeclared, p.lastTokenValue())
		} else {
			procedureName = p.lastTokenValue()

			scope.Insert(&ast.Symbol{
				Name:    procedureName,
				Kind:    ast.Procedure,
				Depth:   p.declarationDepth,
				Address: 0})
		}

		p.nextToken()
	}

	return procedureName
}

// A statement is either
//
//	an assignment statement,
//	a read statement,
//	a write statement,
//	a procedure call,
//	an if statement,
//	a while statement,
//	or a sequence of statements surrounded by begin and end.
func (p *parser) statement(scope *ast.Scope, anchors scn.Tokens) ast.Statement {
	var statement ast.Statement

	switch p.lastToken() {
	case scn.Identifier:
		statement = p.assignment(scope, anchors)

	case scn.Read:
		statement = p.read(scope)

	case scn.Write:
		statement = p.write(scope, anchors)

	case scn.CallWord:
		statement = p.callWord(scope)

	case scn.IfWord:
		statement = p.ifWord(scope, anchors)

	case scn.WhileWord:
		statement = p.whileWord(scope, anchors)

	case scn.BeginWord:
		statement = p.beginWord(scope, anchors)
	}

	// after a statement, the parser expects
	//   a semicolon to separate the statement from the next statement
	//   or the end of the parent block
	//   or the parser would forward to all block-tokens as anchors in the case of a syntax error
	p.tokenHandler.rebase(expectedStatement, anchors, scn.Empty)
	return statement
}

// A condition is either an odd expression or two expressions separated by a relational operator.
func (p *parser) condition(scope *ast.Scope, anchors scn.Tokens) ast.Expression {
	var operation ast.Expression

	if p.lastToken() == scn.OddWord {
		p.nextToken()
		operand := p.expression(scope, anchors)
		operation = ast.NewUnaryOperation(ast.Odd, operand)
	} else {
		// handle left expression of a relational operator
		left := p.expression(scope, set(anchors, scn.Equal, scn.NotEqual, scn.Less, scn.LessEqual, scn.Greater, scn.GreaterEqual))

		if !p.lastToken().In(set(scn.Equal, scn.NotEqual, scn.Less, scn.LessEqual, scn.Greater, scn.GreaterEqual)) {
			p.appendError(expectedRelationalOperator, p.lastTokenName())
		} else {
			relationalOperator := p.lastToken()
			p.nextToken()

			// handle right expression of a relational operator
			right := p.expression(scope, anchors)

			switch relationalOperator {
			case scn.Equal:
				operation = ast.NewConditionalOperation(ast.Equal, left, right)

			case scn.NotEqual:
				operation = ast.NewConditionalOperation(ast.NotEqual, left, right)

			case scn.Less:
				operation = ast.NewConditionalOperation(ast.Less, left, right)

			case scn.LessEqual:
				operation = ast.NewConditionalOperation(ast.LessEqual, left, right)

			case scn.Greater:
				operation = ast.NewConditionalOperation(ast.Greater, left, right)

			case scn.GreaterEqual:
				operation = ast.NewConditionalOperation(ast.GreaterEqual, left, right)
			}
		}
	}

	return operation
}

// An expression is a sequence of terms separated by plus or minus.
func (p *parser) expression(scope *ast.Scope, anchors scn.Tokens) ast.Expression {
	var operation ast.Expression

	// handle left term of a plus or minus operator
	left := p.term(scope, set(anchors, scn.Plus, scn.Minus))

	for p.lastToken() == scn.Plus || p.lastToken() == scn.Minus {
		plusOrMinus := p.lastToken()
		p.nextToken()

		// handle right term of a plus or minus operator
		right := p.term(scope, set(anchors, scn.Plus, scn.Minus))

		if plusOrMinus == scn.Plus {
			operation = ast.NewBinaryOperation(ast.Plus, left, right)
		} else {
			operation = ast.NewBinaryOperation(ast.Minus, left, right)
		}

		left = operation
	}

	if operation == nil {
		operation = left
	}

	return operation
}

// A term is a sequence of factors separated by times or divide.
func (p *parser) term(scope *ast.Scope, anchors scn.Tokens) ast.Expression {
	var operation ast.Expression

	// handle left factor of a times or divide operator
	left := p.factor(scope, set(anchors, scn.Times, scn.Divide))

	for p.lastToken() == scn.Times || p.lastToken() == scn.Divide {
		timesOrDevide := p.lastToken()
		p.nextToken()

		// handle right factor of a times or divide operator
		right := p.factor(scope, set(anchors, scn.Times, scn.Divide))

		if timesOrDevide == scn.Times {
			operation = ast.NewBinaryOperation(ast.Times, left, right)

		} else {
			operation = ast.NewBinaryOperation(ast.Divide, left, right)
		}

		left = operation
	}

	if operation == nil {
		operation = left
	}

	return operation
}

// A factor is either an identifier, a number, or an expression surrounded by parentheses.
func (p *parser) factor(scope *ast.Scope, anchors scn.Tokens) ast.Expression {
	var sign scn.Token
	var operand ast.Expression

	// handle leading plus or minus sign of a factor
	if p.lastToken() == scn.Plus || p.lastToken() == scn.Minus {
		sign = p.lastToken()
		p.nextToken()
	}

	// at the beginning of a factor
	//   the expected tokens are identifiers, numbers, and left parentheses
	//   or the parser would fall back to all block-tokens as anchors in the case of a syntax error
	p.tokenHandler.rebase(expectedIdentifiersNumbersExpressions, factors, anchors)

	for p.lastToken().In(factors) {
		if p.lastToken() == scn.Identifier {
			if symbol := scope.Lookup(p.lastTokenValue()); symbol != nil {
				switch symbol.Kind {
				case ast.Constant:
					operand = ast.NewConstantReference(symbol)

				case ast.Variable:
					operand = ast.NewVariableReference(symbol)

				default:
					p.appendError(expectedConstantsVariables, ast.KindNames[symbol.Kind])
				}
			} else {
				p.appendError(identifierNotFound, p.lastTokenValue())
			}

			p.nextToken()
		} else if p.lastToken() == scn.Number {
			operand = ast.NewLiteral(p.numberValue(sign, p.lastTokenValue()), ast.Integer64)
			sign = scn.Unknown
			p.nextToken()
		} else if p.lastToken() == scn.LeftParenthesis {
			p.nextToken()
			operand = p.expression(scope, set(anchors, scn.RightParenthesis))

			if p.lastToken() == scn.RightParenthesis {
				p.nextToken()
			} else {
				p.appendError(expectedRightParenthesis, p.lastTokenName())
			}
		}

		// after a factor, the parser expects
		//   a times or divide operator
		//   a plus or minus operator
		//   a relational operator
		//   a right parenthesis
		//   or the parser would fall back to all block-tokens as anchors in the case of a syntax error
		p.tokenHandler.rebase(unexpectedTokens, anchors, set(scn.LeftParenthesis))
	}

	// negate the factor if a leading minus sign is present
	if sign == scn.Minus {
		operand = ast.NewUnaryOperation(ast.Negate, operand)
	}

	return operand
}

// Return the next token description from the token handler.
func (p *parser) nextToken() bool {
	return p.tokenHandler.nextTokenDescription()
}

// Wrapper to get token from the last token description.
func (p *parser) lastToken() scn.Token {
	return p.tokenHandler.lastToken()
}

// Wrapper to get the token name from the last token description.
func (p *parser) lastTokenName() string {
	return p.tokenHandler.lastTokenName()
}

// Wrapper to get the token value from the last token description.
func (p *parser) lastTokenValue() string {
	return p.tokenHandler.lastTokenValue()
}

// Append parser error to the error report of the token handler.
func (p *parser) appendError(code failure, value any) {
	p.tokenHandler.appendError(p.tokenHandler.error(code, value))
}

// Analyze a number and convert it to an Integer64 value (-9223372036854775808 to 9223372036854775807).
func (e *parser) numberValue(sign scn.Token, number string) int64 {
	if sign == scn.Minus {
		number = "-" + number
	}

	value, err := strconv.ParseInt(number, 10, integerBitSize)

	if err != nil {
		e.appendError(illegalInteger, number)
	}

	return value
}
