// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package parser

import tok "github.com/petersen65/PL0/token"

// Maximum depth of block nesting.
const blockNestingMax = 8

// Error codes for the PL/0 parser.
const (
	_ = tok.Failure(iota + 2000)
	eofReached
	notFullyParsed
	parsingError
	parsingErrors
	maxBlockDepth
	illegalInteger
	identifierNotFound
	identifierAlreadyDeclared
	expectedRelationalOperator
	expectedPeriod
	expectedIdentifier
	expectedProcedureIdentifier
	expectedVariableIdentifier
	expectedEqual
	expectedBecomes
	expectedNumber
	expectedSemicolon
	expectedStatement
	expectedThen
	expectedDo
	expectedEnd
	expectedRightParenthesis
	expectedStatementsIdentifiers
	expectedStatementsIdentifiersProcedures
	expectedIdentifiersNumbersExpressions
	expectedConstantsVariables
	unexpectedTokens
)

// Map error codes to error messages.
var errorMap = map[tok.Failure]string{
	eofReached:                              "unexpected end of file",
	notFullyParsed:                          "program does not comply with the syntax rules of the programming language",
	parsingError:                            "a parsing error occurred",
	parsingErrors:                           "%v parsing errors occurred",
	maxBlockDepth:                           "depth of block nesting exceeded: %v",
	illegalInteger:                          "cannot parse number %s into integer value",
	identifierNotFound:                      "identifier not found: %v",
	identifierAlreadyDeclared:               "identifier already declared: %v",
	expectedRelationalOperator:              "expected one of =, #, <, <=, >, >= operator, found %v",
	expectedPeriod:                          "expected period at end of the program, found %v",
	expectedIdentifier:                      "expected identifier, found %v",
	expectedProcedureIdentifier:             "expected procedure identifier, found %v",
	expectedVariableIdentifier:              "expected variable identifier, found %v",
	expectedEqual:                           "expected equal, found %v",
	expectedBecomes:                         "expected becomes, found %v",
	expectedNumber:                          "expected number, found %v",
	expectedSemicolon:                       "expected semicolon, found %v",
	expectedStatement:                       "expected statement keywords or identifier, found %v",
	expectedThen:                            "expected then keyword, found %v",
	expectedDo:                              "expected do keyword, found %v",
	expectedEnd:                             "expected end keyword, found %v",
	expectedRightParenthesis:                "expected right parenthesis, found %v",
	expectedStatementsIdentifiers:           "expected statements or identifiers, found %v",
	expectedStatementsIdentifiersProcedures: "expected statements, identifiers or procedures, found %v",
	expectedIdentifiersNumbersExpressions:   "expected identifiers, numbers or expressions surrounded by parentheses, found %v",
	expectedConstantsVariables:              "expected constants or variables, found %v",
	unexpectedTokens:                        "unexpected set of tokens, found %v",
}
