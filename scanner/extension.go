// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

// Package scanner implements the PL/0 scanner that performs a lexical analysis of the source code.
// Extensions of the scanner analyze the concrete syntax of a basic scan to support more complex scenarios.
package scanner

import "strconv"

// This extension analyzes the concrete syntax with a 2 or 3 tokens sliding window approach to enable multi-character operators and signed numbers.
func slidingScan(syntax ConcreteSyntax) (ConcreteSyntax, error) {
	var errFull error
	fullSyntax := make(ConcreteSyntax, 0, len(syntax))

	for i := 0; errFull == nil && i < len(syntax); i++ {
		switch syntax[i].Token {
		case Colon:
			if try(i+1, syntax) == Equal {
				i++
				fullSyntax = append(fullSyntax, merge(Becomes, syntax[i]))
			} else {
				fullSyntax = append(fullSyntax, syntax[i])
			}

		case Less:
			if try(i+1, syntax) == Equal {
				i++
				fullSyntax = append(fullSyntax, merge(LessEqual, syntax[i]))
			} else {
				fullSyntax = append(fullSyntax, syntax[i])
			}

		case Greater:
			if try(i+1, syntax) == Equal {
				i++
				fullSyntax = append(fullSyntax, merge(GreaterEqual, syntax[i]))
			} else {
				fullSyntax = append(fullSyntax, syntax[i])
			}

		case Plus:
			fallthrough
		case Minus:
			if try(i+1, syntax) == Number && !try(i-1, syntax).In(Set(Identifier, Number, RightParenthesis)) {
				i++
				number := syntax[i]

				if len(number.TokenValue.(string)) == 0 {
					number.TokenValue = "0"
				} else if syntax[i-1].Token == Minus {
					number.TokenValue = "-" + number.TokenValue.(string)
				}

				number, errFull = numberValue(number)
				fullSyntax = append(fullSyntax, number)
			} else {
				fullSyntax = append(fullSyntax, syntax[i])
			}

		case Number:
			number := syntax[i]

			if len(number.TokenValue.(string)) == 0 {
				number.TokenValue = "0"
			}

			number, errFull = numberValue(number)
			fullSyntax = append(fullSyntax, number)

		default:
			fullSyntax = append(fullSyntax, syntax[i])
		}
	}

	return fullSyntax, errFull
}

// Detect whether a sliding window exists and contains an expected set of tokens.
func try(i int, syntax ConcreteSyntax) Token {
	if i > 0 && i < len(syntax) {
		return syntax[i].Token
	}

	return Null
}

// Overwrite a source token with a target token's name, value and type and return the merged result.
func merge(targetToken Token, sourceToken TokenDescription) TokenDescription {
	return TokenDescription{
		Token:       targetToken,
		TokenName:   TokenNames[targetToken],
		TokenValue:  nil,
		TokenType:   None,
		Line:        sourceToken.Line,
		Column:      sourceToken.Column,
		CurrentLine: sourceToken.CurrentLine,
	}
}

// Analyze the concrete syntax of a number and convert it to an Integer64 value.
func numberValue(number TokenDescription) (TokenDescription, error) {
	number.TokenType = Integer64

	if int64Value, err := strconv.ParseInt(number.TokenValue.(string), 10, IntegerBitSize); err != nil {
		return number, newError(illegalInteger, number.TokenValue, number.Line, number.Column)
	} else {
		number.TokenValue = int64Value
	}

	return number, nil
}
