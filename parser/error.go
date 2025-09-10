// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package parser

import eh "github.com/petersen65/pl0/v3/errors"

// Maximum depth of block nesting.
const blockNestingMax = 8

// Failure codes for the parser.
const (
	_ eh.Failure = iota + 2000
	invalidParserState
	eofReached
	notFullyParsed
	maxBlockDepth
	illegalInteger
	expectedComparisonOperator
	expectedPeriod
	expectedIdentifier
	expectedEqual
	expectedBecomes
	expectedNumber
	expectedSemicolon
	expectedColon
	expectedThen
	expectedDo
	expectedEnd
	expectedRightParenthesis
	expectedStatementsIdentifiers
	expectedStatementsIdentifiersProcedures
	expectedIdentifiersLiteralsExpressions
	expectedConstantsVariables
	unexpectedTokensAfterStatement
	unexpectedTokens
)

// Map failure codes to error messages.
var failureMap = map[eh.Failure]string{
	invalidParserState:                      "parser is in an undefined state and cannot continue parsing",
	eofReached:                              "unexpected end of file",
	notFullyParsed:                          "program does not comply with the syntax rules of the programming language",
	maxBlockDepth:                           "depth of block nesting exceeded: %v",
	illegalInteger:                          "cannot parse number %s into integer value",
	expectedComparisonOperator:              "expected one of =, #, <, <=, >, >= operator, found %v",
	expectedPeriod:                          "expected period at end of the program, found %v",
	expectedIdentifier:                      "expected identifier, found %v",
	expectedEqual:                           "expected equal, found %v",
	expectedBecomes:                         "expected becomes, found %v",
	expectedNumber:                          "expected number, found %v",
	expectedSemicolon:                       "expected semicolon, found %v",
	expectedColon:                           "expected colon, found %v",
	expectedStatementsIdentifiers:           "expected statement keywords or identifier, found %v",
	expectedThen:                            "expected then keyword, found %v",
	expectedDo:                              "expected do keyword, found %v",
	expectedEnd:                             "expected end keyword, found %v",
	expectedRightParenthesis:                "expected right parenthesis, found %v",
	expectedStatementsIdentifiersProcedures: "expected statements, identifiers or procedures, found %v",
	expectedIdentifiersLiteralsExpressions:  "expected identifiers, literals or expressions surrounded by parentheses, found %v",
	expectedConstantsVariables:              "expected constants or variables, found %v",
	unexpectedTokensAfterStatement:          "unexpected set of tokens after statement, found %v",
	unexpectedTokens:                        "unexpected set of tokens, found %v",
}
