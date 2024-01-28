// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

// Package parser implements the PL/0 parser that performs a syntactical analysis of the concrete syntax.
package parser

import (
	emt "github.com/petersen65/PL0/emitter"
	scn "github.com/petersen65/PL0/scanner"
)

type expressionParser struct {
	emitter               emt.Emitter   // emitter that emits the code
	memoryLocation        int32         // memory location of the current expression
	maximumMemoryLocation uint64        // maximum used memory location within the current block
	tokenHandler          *tokenHandler // token handler that manages the tokens of the concrete syntax
	symbolTable           *symbolTable  // symbol table that stores all symbols of the program
}

func newExpressionParser(tokenHandler *tokenHandler, symbolTable *symbolTable, emitter emt.Emitter) *expressionParser {
	return &expressionParser{
		tokenHandler: tokenHandler,
		symbolTable:  symbolTable,
		emitter:      emitter,
	}
}

func (e *expressionParser) reset() {
	e.memoryLocation = 0
	e.maximumMemoryLocation = 0
}

func (e *expressionParser) start() {
	e.memoryLocation = 0
}

func (e *expressionParser) requiredLocations() uint64 {
	return e.maximumMemoryLocation + 1
}

func (e *expressionParser) location() int32 {
	return e.memoryLocation
}

// An expression is a sequence of terms separated by plus or minus.
func (e *expressionParser) expression(depth int32, expected scn.Tokens) {
	// handle leading plus or minus sign of a term
	if e.lastToken() == scn.Plus || e.lastToken() == scn.Minus {
		plusOrMinus := e.lastToken()
		e.nextToken()

		// handle left term of a plus or minus operator
		e.term(depth, set(expected, scn.Plus, scn.Minus))

		if plusOrMinus == scn.Minus {
			e.emitter.Negate(e.memoryLocation)
		}
	} else {
		// handle left term of a plus or minus operator
		e.term(depth, set(expected, scn.Plus, scn.Minus))
	}

	for e.lastToken() == scn.Plus || e.lastToken() == scn.Minus {
		plusOrMinus := e.lastToken()
		e.nextToken()

		// handle right term of a plus or minus operator
		e.memoryLocation++
		e.maximumMemoryLocation = max(e.maximumMemoryLocation, uint64(e.memoryLocation))
		e.term(depth, set(expected, scn.Plus, scn.Minus))
		e.memoryLocation--

		if plusOrMinus == scn.Plus {
			e.emitter.Add(e.memoryLocation)
		} else {
			e.emitter.Subtract(e.memoryLocation)
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
		e.memoryLocation++
		e.maximumMemoryLocation = max(e.maximumMemoryLocation, uint64(e.memoryLocation))
		e.factor(depth, set(expected, scn.Times, scn.Divide))
		e.memoryLocation--

		if timesOrDevide == scn.Times {
			e.emitter.Multiply(e.memoryLocation)

		} else {
			e.emitter.Divide(e.memoryLocation)
		}
	}
}

// A factor is either an identifier, a number, or an expression surrounded by parentheses.
func (e *expressionParser) factor(depth int32, expected scn.Tokens) {
	e.tokenHandler.rebase(expectedIdentifiersNumbersExpressions, factors, expected)

	for e.lastToken().In(factors) {
		if e.lastToken() == scn.Identifier {
			if symbol, ok := e.symbolTable.find(e.lastTokenValue().(string)); ok {
				switch symbol.kind {
				case constant:
					e.emitter.Constant(e.memoryLocation, symbol.value)

				case variable:
					e.emitter.LoadVariable(emt.Offset(symbol.offset), depth-symbol.depth, e.memoryLocation)

				case procedure:
					e.appendError(expectedConstantsVariables, kindNames[symbol.kind])
				}
			} else {
				e.appendError(identifierNotFound, e.lastTokenValue())
			}

			e.nextToken()
		} else if e.lastToken() == scn.Number {
			e.emitter.Constant(e.memoryLocation, e.lastTokenValue())
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
}

func (e *expressionParser) lastToken() scn.Token {
	return e.tokenHandler.lastToken()
}

func (e *expressionParser) nextToken() bool {
	return e.tokenHandler.nextTokenDescription()
}

func (e *expressionParser) lastTokenName() string {
	return e.tokenHandler.lastTokenName()
}

func (e *expressionParser) lastTokenValue() any {
	return e.tokenHandler.lastTokenValue()
}

func (e *expressionParser) appendError(code failure, value any) {
	e.tokenHandler.appendError(e.tokenHandler.error(code, value))
}
