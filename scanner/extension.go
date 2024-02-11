// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

// Extensions of the scanner analyze the concrete syntax of a basic scan to support more complex scenarios.
package scanner

import "strconv"

// This extension analyzes the concrete syntax with a sliding window approach to enable signed and valued numbers.
func slidingScan(syntax ConcreteSyntax) (ConcreteSyntax, error) {
	var errFull error
	fullSyntax := make(ConcreteSyntax, 0, len(syntax))

	for i := 0; errFull == nil && i < len(syntax); i++ {
		switch syntax[i].Token {
		case Plus:
			fallthrough
		case Minus:
			if peekToken(i+1, syntax) == Number && !peekToken(i-1, syntax).In(Set(Identifier, Number, RightParenthesis)) {
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
func peekToken(i int, syntax ConcreteSyntax) Token {
	if i > 0 && i < len(syntax) {
		return syntax[i].Token
	}

	return Null
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
