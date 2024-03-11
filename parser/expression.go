// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package parser

import (
	"strconv"

	ast "github.com/petersen65/PL0/ast"
	emt "github.com/petersen65/PL0/emitter"
	scn "github.com/petersen65/PL0/scanner"
)

// Number of bits of a signed integer.
const integerBitSize = 64

// The expression parser is a parser that performs a syntactical analysis of an expression.
type expressionParser struct {
	emitter      emt.Emitter   // emitter that emits the code
	tokenHandler *tokenHandler // token handler that manages the tokens of the token stream
}

// Create a new expression parser with the given token handler and emitter.
func newExpressionParser(tokenHandler *tokenHandler, emitter emt.Emitter) *expressionParser {
	return &expressionParser{
		tokenHandler: tokenHandler,
		emitter:      emitter,
	}
}

// A condition is either an odd expression or two expressions separated by a relational operator.
func (e *expressionParser) condition(scope *ast.Scope, depth int32, anchors scn.Tokens) (scn.Token, ast.Expression) {
	from := e.gatherSourceDescription()
	var operation ast.Expression
	var relationalOperator scn.Token

	if e.lastToken() == scn.OddWord {
		relationalOperator = e.lastToken()
		e.nextToken()
		operand := e.expression(scope, depth, anchors)
		e.emitter.Odd()
		operation = ast.NewUnaryOperation(ast.Odd, operand, e.collectSourceDescription(from))
	} else {
		// handle left expression of a relational operator
		left := e.expression(scope, depth, set(anchors, scn.Equal, scn.NotEqual, scn.Less, scn.LessEqual, scn.Greater, scn.GreaterEqual))

		if !e.lastToken().In(set(scn.Equal, scn.NotEqual, scn.Less, scn.LessEqual, scn.Greater, scn.GreaterEqual)) {
			e.appendError(expectedRelationalOperator, e.lastTokenName())
		} else {
			relationalOperator = e.lastToken()
			e.nextToken()

			// handle right expression of a relational operator
			right := e.expression(scope, depth, anchors)

			switch relationalOperator {
			case scn.Equal:
				e.emitter.Equal()
				operation = ast.NewConditionalOperation(ast.Equal, left, right, e.collectSourceDescription(from))

			case scn.NotEqual:
				e.emitter.NotEqual()
				operation = ast.NewConditionalOperation(ast.NotEqual, left, right, e.collectSourceDescription(from))

			case scn.Less:
				e.emitter.Less()
				operation = ast.NewConditionalOperation(ast.Less, left, right, e.collectSourceDescription(from))

			case scn.LessEqual:
				e.emitter.LessEqual()
				operation = ast.NewConditionalOperation(ast.LessEqual, left, right, e.collectSourceDescription(from))

			case scn.Greater:
				e.emitter.Greater()
				operation = ast.NewConditionalOperation(ast.Greater, left, right, e.collectSourceDescription(from))

			case scn.GreaterEqual:
				e.emitter.GreaterEqual()
				operation = ast.NewConditionalOperation(ast.GreaterEqual, left, right, e.collectSourceDescription(from))
			}
		}
	}

	return relationalOperator, operation
}

// An expression is a sequence of terms separated by plus or minus.
func (e *expressionParser) expression(scope *ast.Scope, depth int32, anchors scn.Tokens) ast.Expression {
	from := e.gatherSourceDescription()
	var operation ast.Expression

	// handle left term of a plus or minus operator
	left := e.term(scope, depth, set(anchors, scn.Plus, scn.Minus))

	for e.lastToken() == scn.Plus || e.lastToken() == scn.Minus {
		plusOrMinus := e.lastToken()
		e.nextToken()

		// handle right term of a plus or minus operator
		right := e.term(scope, depth, set(anchors, scn.Plus, scn.Minus))

		if plusOrMinus == scn.Plus {
			e.emitter.Add()
			operation = ast.NewBinaryOperation(ast.Plus, left, right, e.collectSourceDescription(from))
		} else {
			e.emitter.Subtract()
			operation = ast.NewBinaryOperation(ast.Minus, left, right, e.collectSourceDescription(from))
		}
	}

	if operation == nil {
		operation = left
	}

	return operation
}

// A term is a sequence of factors separated by times or divide.
func (e *expressionParser) term(scope *ast.Scope, depth int32, anchors scn.Tokens) ast.Expression {
	from := e.gatherSourceDescription()
	var operation ast.Expression

	// handle left factor of a times or divide operator
	left := e.factor(scope, depth, set(anchors, scn.Times, scn.Divide))

	for e.lastToken() == scn.Times || e.lastToken() == scn.Divide {
		timesOrDevide := e.lastToken()
		e.nextToken()

		// handle right factor of a times or divide operator
		right := e.factor(scope, depth, set(anchors, scn.Times, scn.Divide))

		if timesOrDevide == scn.Times {
			e.emitter.Multiply()
			operation = ast.NewBinaryOperation(ast.Times, left, right, e.collectSourceDescription(from))

		} else {
			e.emitter.Divide()
			operation = ast.NewBinaryOperation(ast.Divide, left, right, e.collectSourceDescription(from))
		}
	}

	if operation == nil {
		operation = left
	}

	return operation
}

// A factor is either an identifier, a number, or an expression surrounded by parentheses.
func (e *expressionParser) factor(scope *ast.Scope, depth int32, anchors scn.Tokens) ast.Expression {
	from := e.gatherSourceDescription()
	var sign scn.Token
	var operand ast.Expression

	// handle leading plus or minus sign of a factor
	if e.lastToken() == scn.Plus || e.lastToken() == scn.Minus {
		sign = e.lastToken()
		e.nextToken()
	}

	// at the beginning of a factor
	//   the expected tokens are identifiers, numbers, and left parentheses
	//   or the parser would fall back to all block-tokens as anchors in the case of a syntax error
	e.tokenHandler.rebase(expectedIdentifiersNumbersExpressions, factors, anchors)

	for e.lastToken().In(factors) {
		if e.lastToken() == scn.Identifier {
			if symbol := scope.Lookup(e.lastTokenValue()); symbol != nil{
				switch symbol.Kind {
				case ast.Constant:
					e.emitter.Constant(symbol.Value)
					operand = ast.NewConstantReference(symbol, e.collectSourceDescription(from))

				case ast.Variable:
					e.emitter.LoadVariable(emt.Offset(symbol.Offset), depth-symbol.Depth)
					operand = ast.NewVariableReference(symbol, e.collectSourceDescription(from))

				default:
					e.appendError(expectedConstantsVariables, ast.KindNames[symbol.Kind])
				}
			} else {
				e.appendError(identifierNotFound, e.lastTokenValue())
			}

			e.nextToken()
		} else if e.lastToken() == scn.Number {
			e.emitter.Constant(e.numberValue(sign, e.lastTokenValue()))
			operand = ast.NewLiteralReference(e.numberValue(sign, e.lastTokenValue()), ast.Integer64, e.collectSourceDescription(from))
			sign = scn.Unknown
			e.nextToken()
		} else if e.lastToken() == scn.LeftParenthesis {
			e.nextToken()
			operand = e.expression(scope, depth, set(anchors, scn.RightParenthesis))

			if e.lastToken() == scn.RightParenthesis {
				e.nextToken()
			} else {
				e.appendError(expectedRightParenthesis, e.lastTokenName())
			}
		}

		// after a factor, the parser expects
		//   a times or divide operator
		//   a plus or minus operator
		//   a relational operator
		//   a right parenthesis
		//   or the parser would fall back to all block-tokens as anchors in the case of a syntax error
		e.tokenHandler.rebase(unexpectedTokens, anchors, set(scn.LeftParenthesis))
	}

	// negate the factor if a leading minus sign is present
	if sign == scn.Minus {
		e.emitter.Negate()
		operand = ast.NewUnaryOperation(ast.Negate, operand, e.collectSourceDescription(from))
	}

	return operand
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
func (e *expressionParser) lastTokenValue() string {
	return e.tokenHandler.lastTokenValue()
}

// Wrapper to gather source description from the current token index.
func (e *expressionParser) gatherSourceDescription() int {
	return e.tokenHandler.gatherSourceDescription()
}

// Wrapper to collect source description from the given index to the current token index.
func (e *expressionParser) collectSourceDescription(from int) *ast.SourceDescription {
	return e.tokenHandler.collectSourceDescription(from)
}

// Append expression parser error to the error report of the token handler.
func (e *expressionParser) appendError(code failure, value any) {
	e.tokenHandler.appendError(e.tokenHandler.error(code, value))
}

// Analyze a number and convert it to an Integer64 value (-9223372036854775808 to 9223372036854775807).
func (e *expressionParser) numberValue(sign scn.Token, number string) int64 {
	if sign == scn.Minus {
		number = "-" + number
	}

	value, err := strconv.ParseInt(number, 10, integerBitSize)

	if err != nil {
		e.appendError(illegalInteger, number)
	}

	return value
}
