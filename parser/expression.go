// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

// Package parser implements the PL/0 parser that performs a syntactical analysis of the concrete syntax.
package parser

import (
	emt "github.com/petersen65/PL0/emitter"
	scn "github.com/petersen65/PL0/scanner"
)

// The expression parser is a parser that performs a syntactical analysis of an expression.
type expressionParser struct {
	emitter               emt.Emitter   // emitter that emits the code
	memoryLocation        int32         // memory location of the current expression
	maximumMemoryLocation int64         // maximum used memory location within the current block
	tokenHandler          *tokenHandler // token handler that manages the tokens of the concrete syntax
	symbolTable           *symbolTable  // symbol table that stores all symbols of the program
}

// Create a new expression parser with the given token handler, symbol table, and emitter.
func newExpressionParser(tokenHandler *tokenHandler, symbolTable *symbolTable, emitter emt.Emitter) *expressionParser {
	return &expressionParser{
		tokenHandler: tokenHandler,
		symbolTable:  symbolTable,
		emitter:      emitter,
	}
}

// Reset the memory location and the maximum required memory locations of the expression parser.
func (e *expressionParser) reset() {
	e.memoryLocation = 0
	e.maximumMemoryLocation = 0
}

// Set the expression parser to start with no or a preserved memory location.
func (e *expressionParser) start(preserve bool) {
	if preserve {
		e.memoryLocation++
	} else {
		e.memoryLocation = 0
	}
}

// Return the maximum required memory locations of the expression parser after several expressions have been parsed.
func (e *expressionParser) requiredLocations() int64 {
	return e.maximumMemoryLocation + 1
}

// Return the current memory location of the expression parser after parsing one expression.
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
		e.maximumMemoryLocation = max(e.maximumMemoryLocation, int64(e.memoryLocation))
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
		e.maximumMemoryLocation = max(e.maximumMemoryLocation, int64(e.memoryLocation))
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
