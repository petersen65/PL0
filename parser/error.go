package parser

import "fmt"

const (
	blockNestingMax = 3
)

const (
	_ = failure(iota + 2000)
	eofReached
	parsingErrors
	maxBlockDepth
	emptyProcedureName
	identifierNotFound
	expectedRelationalOperator
	expectedPeriod
	expectedIdentifier
	expectedProcedureIdentifier
	expectedVariableIdentifier
	expectedEqual
	expectedNumber
	expectedSemicolon
	expectedStatement
	expectedThen
	expectedDo
	expectedEnd
	expectedStatementsIdentifiers
	expectedStatementsIdentifiersProcedures
	unexpectedTokens
)

type failure int

var errorMap = map[failure]string{
	eofReached:                              "unexpected end of file",
	parsingErrors:                           "%v parsing errors occurred",
	maxBlockDepth:                           "depth of block nesting exceeded: %v",
	emptyProcedureName:                      "procedure name cannot be empty",
	identifierNotFound:                      "identifier not found: %v",
	expectedRelationalOperator:              "expected one of =, #, <, <=, >, >= operator, found %v",
	expectedPeriod:                          "expected period at end of the program, found %v",
	expectedIdentifier:                      "expected identifier, found %v",
	expectedProcedureIdentifier:             "expected procedure identifier, found %v",
	expectedVariableIdentifier:              "expected variable identifier, found %v",
	expectedEqual:                           "expected equal sign, found %v",
	expectedNumber:                          "expected number, found %v",
	expectedSemicolon:                       "expected semicolon, found %v",
	expectedStatement:                       "expected statement keywords or identifier, found %v",
	expectedThen:                            "expected then keyword, found %v",
	expectedDo:                              "expected do keyword, found %v",
	expectedEnd:                             "expected end keyword, found %v",
	expectedStatementsIdentifiers:           "expected statements or identifiers, found %v",
	expectedStatementsIdentifiersProcedures: "expected statements, identifiers or procedures, found %v",
	unexpectedTokens:                        "unexpected set of tokens, found %v",
}

func (p *parser) appendError(err error) error {
	if err == nil {
		return nil
	}

	p.errorReport = append(p.errorReport, Error{
		Err:         err,
		Line:        p.lastTokenDescription.Line,
		Column:      p.lastTokenDescription.Column,
		CurrentLine: p.lastTokenDescription.CurrentLine,
	})

	return err
}

func (p *parser) lastError() error {
	if len(p.errorReport) > 0 {
		return p.errorReport[len(p.errorReport)-1].Err
	}

	return nil
}

func (p *parser) error(code failure, value any) error {
	var message string

	if value != nil {
		message = fmt.Sprintf(errorMap[code], value)
	} else {
		message = errorMap[code]
	}

	line, column := p.lastTokenDescription.Line, p.lastTokenDescription.Column
	return fmt.Errorf("parser error %v [%v,%v]: %v", code, line, column, message)
}
