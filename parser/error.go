// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package parser

import "fmt"

const blockNestingMax = 8 // maximum depth of block nesting

// Error codes for the PL/0 parser.
const (
	_ = failure(iota + 2000)
	eofReached
	parsingError
	parsingErrors
	maxBlockDepth
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

type failure int

// Map error codes to error messages.
var errorMap = map[failure]string{
	eofReached:                              "unexpected end of file",
	parsingError:                            "a parsing error occurred",
	parsingErrors:                           "%v parsing errors occurred",
	maxBlockDepth:                           "depth of block nesting exceeded: %v",
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

// Append an error to the error report of the token handler which is used to store all errors that occured during parsing.
func (t *tokenHandler) appendError(err error) error {
	if err == nil {
		return nil
	}

	t.errorReport = append(t.errorReport, Error{
		Err:         err,
		Line:        t.lastTokenDescription.Line,
		Column:      t.lastTokenDescription.Column,
		CurrentLine: t.lastTokenDescription.CurrentLine,
	})

	return err
}

// Create a new error by mapping the error code to its corresponding error message.
func (t *tokenHandler) error(code failure, value any) error {
	var message string

	if value != nil {
		message = fmt.Sprintf(errorMap[code], value)
	} else {
		message = errorMap[code]
	}

	line, column := t.lastTokenDescription.Line, t.lastTokenDescription.Column
	return fmt.Errorf("parser error %v [%v,%v]: %v", code, line, column, message)
}
