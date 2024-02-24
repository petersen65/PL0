// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package parser

import (
	emt "github.com/petersen65/PL0/emitter"
	scn "github.com/petersen65/PL0/scanner"
)

// The expression parser is a parser that performs a syntactical analysis of an expression.
type expressionParser struct {
	emitter      emt.Emitter   // emitter that emits the code
	tokenHandler *tokenHandler // token handler that manages the tokens of the token stream
	symbolTable  *symbolTable  // symbol table that stores all symbols of the program
}

// Create a new expression parser with the given token handler, symbol table, and emitter.
func newExpressionParser(tokenHandler *tokenHandler, symbolTable *symbolTable, emitter emt.Emitter) *expressionParser {
	return &expressionParser{
		tokenHandler: tokenHandler,
		symbolTable:  symbolTable,
		emitter:      emitter,
	}
}

// A condition is either an odd expression or two expressions separated by a relational operator.
func (e *expressionParser) condition(depth int32, expected scn.Tokens) scn.Token {
	var relationalOperator scn.Token

	if e.lastToken() == scn.OddWord {
		relationalOperator = e.lastToken()
		e.nextToken()
		e.expression(depth, expected)
		e.emitter.Odd()
	} else {
		// handle left expression of a relational operator
		e.expression(depth, set(expected, scn.Equal, scn.NotEqual, scn.Less, scn.LessEqual, scn.Greater, scn.GreaterEqual))

		if !e.lastToken().In(set(scn.Equal, scn.NotEqual, scn.Less, scn.LessEqual, scn.Greater, scn.GreaterEqual)) {
			e.appendError(expectedRelationalOperator, e.lastTokenName())
		} else {
			relationalOperator = e.lastToken()
			e.nextToken()

			// handle right expression of a relational operator
			e.expression(depth, expected)

			switch relationalOperator {
			case scn.Equal:
				e.emitter.Equal()

			case scn.NotEqual:
				e.emitter.NotEqual()

			case scn.Less:
				e.emitter.Less()

			case scn.LessEqual:
				e.emitter.LessEqual()

			case scn.Greater:
				e.emitter.Greater()

			case scn.GreaterEqual:
				e.emitter.GreaterEqual()
			}
		}
	}

	return relationalOperator
}

// An expression is a sequence of terms separated by plus or minus.
func (e *expressionParser) expression(depth int32, expected scn.Tokens) {
	// handle left term of a plus or minus operator
	e.term(depth, set(expected, scn.Plus, scn.Minus))

	for e.lastToken() == scn.Plus || e.lastToken() == scn.Minus {
		plusOrMinus := e.lastToken()
		e.nextToken()

		// handle right term of a plus or minus operator
		e.term(depth, set(expected, scn.Plus, scn.Minus))

		if plusOrMinus == scn.Plus {
			e.emitter.Add()
		} else {
			e.emitter.Subtract()
		}
	}
}

// A term is a sequence of factors separated by times or divide.
func (e *expressionParser) term(depth int32, expected scn.Tokens) {

	// handle left factor of a times or divide operator
	e.factor(depth, set(expected, scn.Times, scn.Divide))

	for e.lastToken() == scn.Times || e.lastToken() == scn.Divide {
		timesOrDevide := e.lastToken()
		e.nextToken()

		// handle right factor of a times or divide operator
		e.factor(depth, set(expected, scn.Times, scn.Divide))

		if timesOrDevide == scn.Times {
			e.emitter.Multiply()

		} else {
			e.emitter.Divide()
		}
	}
}

// A factor is either an identifier, a number, or an expression surrounded by parentheses.
func (e *expressionParser) factor(depth int32, expected scn.Tokens) {
	var sign scn.Token
	
	// handle leading plus or minus sign of a factor
	if e.lastToken() == scn.Plus || e.lastToken() == scn.Minus {
		sign = e.lastToken()
		e.nextToken()
	}
	
	e.tokenHandler.rebase(expectedIdentifiersNumbersExpressions, factors, expected)

	for e.lastToken().In(factors) {
		if e.lastToken() == scn.Identifier {
			if symbol, ok := e.symbolTable.find(e.lastTokenValue().(string)); ok {
				switch symbol.kind {
				case constant:
					e.emitter.Constant(symbol.value)

				case variable:
					e.emitter.LoadVariable(emt.Offset(symbol.offset), depth-symbol.depth)

				case procedure:
					e.appendError(expectedConstantsVariables, kindNames[symbol.kind])
				}
			} else {
				e.appendError(identifierNotFound, e.lastTokenValue())
			}

			e.nextToken()
		} else if e.lastToken() == scn.Number {
			e.emitter.Constant(e.lastTokenValue())
			e.nextToken()
		} else if e.lastToken() == scn.LeftParenthesis {
			e.nextToken()
			e.expression(depth, set(expected, scn.RightParenthesis))

			if e.lastToken() == scn.RightParenthesis {
				e.nextToken()
			} else {
				e.appendError(expectedRightParenthesis, e.lastTokenName())
			}
		}

		e.tokenHandler.rebase(unexpectedTokens, expected, set(scn.LeftParenthesis))
	}

	// negate the factor if a leading minus sign is present
	if sign == scn.Minus {
		e.emitter.Negate()
	}
}

// Return the next token description from the token handler.
func (e *expressionParser) nextToken() bool {
	return e.tokenHandler.nextTokenDescription()
}

// Wrapper to get token from the last token description.
func (e *expressionParser) lastToken() scn.Token {
	return e.tokenHandler.lastToken()
}

// Wrapper to get the token name from the last token description.
func (e *expressionParser) lastTokenName() string {
	return e.tokenHandler.lastTokenName()
}

// Wrapper to get the token value from the last token description.
func (e *expressionParser) lastTokenValue() any {
	return e.tokenHandler.lastTokenValue()
}

// Append expression parser error to the error report of the token handler.
func (e *expressionParser) appendError(code failure, value any) {
	e.tokenHandler.appendError(e.tokenHandler.error(code, value))
}
