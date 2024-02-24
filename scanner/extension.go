// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

// Extensions of the scanner analyze the token stream produced by a scan to support more complex scenarios.
package scanner

import "strconv"

// This extension analyzes the token stream with a sliding window approach to implement experimental changes for the PL/0 grammar.
func experimental(tokens TokenStream) (TokenStream, error) {
	var errPreParsed error
	preParsed := make(TokenStream, 0, len(tokens))

	for i := 0; errPreParsed == nil && i < len(tokens); i++ {
		switch tokens[i].Token {
		// case Plus:
		// 	fallthrough
		// case Minus:
		// 	if peekToken(i+1, tokens) == Number && !peekToken(i-1, tokens).In(Set(Identifier, Number, RightParenthesis)) {
		// 		i++
		// 		number := tokens[i]

		// 		if len(number.TokenValue.(string)) == 0 {
		// 			number.TokenValue = "0"
		// 		} else if tokens[i-1].Token == Minus {
		// 			number.TokenValue = "-" + number.TokenValue.(string)
		// 		}

		// 		number, errPreParsed = numberValue(number)
		// 		preParsed = append(preParsed, number)
		// 	} else if peekToken(i+1, tokens) == Identifier && !peekToken(i-1, tokens).In(Set(Identifier, Number, RightParenthesis)) {
		// 		if tokens[i].Token == Minus {
		// 			preParsed = append(preParsed, mergeToken(tokens[i], LeftParenthesis))
		// 			preParsed = append(preParsed, tokens[i])
		// 			preParsed = append(preParsed, tokens[i+1])
		// 			preParsed = append(preParsed, mergeToken(tokens[i+1], RightParenthesis))
		// 			i++
		// 		}
		// 	} else {
		// 		preParsed = append(preParsed, tokens[i])
		// 	}

		case Number:
			number := tokens[i]

			if len(number.TokenValue.(string)) == 0 {
				number.TokenValue = "0"
			}

			number, errPreParsed = numberValue(number)
			preParsed = append(preParsed, number)

		default:
			preParsed = append(preParsed, tokens[i])
		}
	}

	return preParsed, errPreParsed
}

// Detect whether an expected tokens exists at a specific position in the token stream.
func peekToken(i int, tokens TokenStream) Token {
	if i > 0 && i < len(tokens) {
		return tokens[i].Token
	}

	return Unknown
}

// Merge a token with a template token description.
func mergeToken(template TokenDescription, arg Token) TokenDescription {
	return TokenDescription{
		Token:       arg,
		TokenName:   TokenNames[arg],
		TokenValue:  "",
		DataType:    None,
		Line:        template.Line,
		Column:      template.Column,
		CurrentLine: template.CurrentLine,
	}
}

// Analyze the token stream of a number and convert it to an Integer64 value.
func numberValue(number TokenDescription) (TokenDescription, error) {
	number.DataType = Integer64

	if int64Value, err := strconv.ParseInt(number.TokenValue.(string), 10, IntegerBitSize); err != nil {
		return number, newError(illegalInteger, number.TokenValue, number.Line, number.Column)
	} else {
		number.TokenValue = int64Value
	}

	return number, nil
}
