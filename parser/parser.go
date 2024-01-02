package parser

import (
	"fmt"
	"slices"

	"github.com/petersen65/PL0/emitter"
	"github.com/petersen65/PL0/scanner"
)

const (
	blockNestingMax = 3 // maximum depth of block nesting
)

const (
	constant = entry(iota)
	variable
	procedure
)

const (
	_ = failure(iota + 2000)
	eofReached
	parsingErrors
	maxBlockLevel
	expectedPeriod
	expectedIdentifier
	expectedEqual
	expectedNumber
)

type (
	failure int
	entry   int
	tokens  []scanner.Token

	symbol struct {
		kind   entry
		level  int
		value  any
		offset uint64
	}

	parser struct {
		concreteSyntaxIndex                   int
		concreteSyntax                        scanner.ConcreteSyntax
		emitter                               emitter.Emitter
		lastTokenDescription                  scanner.TokenDescription
		blockLevel                            int
		varOffset                             uint64
		declarations, statements, expressions tokens
		symbolTable                           map[string]symbol
		errorReport                           ErrorReport
		errorMap                              map[failure]string
	}
)

func NewParser() Parser {
	return &parser{
		declarations: tokens{
			scanner.ConstWord,
			scanner.VarWord,
			scanner.ProcedureWord,
		},
		statements: tokens{
			scanner.BeginWord,
			scanner.CallWord,
			scanner.IfWord,
			scanner.WhileWord,
		},
		expressions: tokens{
			scanner.Identifier,
			scanner.Number,
			scanner.LeftParenthesis,
		},
		errorMap: map[failure]string{
			eofReached:         "unexpected end of file",
			parsingErrors:      "%v parsing errors occurred",
			maxBlockLevel:      "depth of block nesting exceeded: %v",
			expectedPeriod:     "expected period at end of the program, found %v",
			expectedIdentifier: "expected identifier, found %v",
			expectedEqual:      "expected equal sign, found %v",
			expectedNumber:     "expected number, found %v",
		},
	}
}

func (p *parser) Parse(concreteSyntax scanner.ConcreteSyntax, emitter emitter.Emitter) (ErrorReport, error) {
	if err := p.reset(concreteSyntax, emitter); err != nil {
		return p.errorReport, err
	}

	p.block(append(append(p.declarations, p.statements...), scanner.Period))

	if p.lastTokenDescription.Token != scanner.Period {
		p.appendError(p.error(expectedPeriod, p.lastTokenDescription.TokenName))
	}

	if len(p.errorReport) > 0 {
		return p.errorReport, p.error(parsingErrors, len(p.errorReport))
	} else {
		return p.errorReport, nil
	}
}

func (p *parser) reset(concreteSyntax scanner.ConcreteSyntax, emitter emitter.Emitter) error {
	p.concreteSyntaxIndex = 0
	p.concreteSyntax = concreteSyntax
	p.emitter = emitter
	p.blockLevel = 0
	p.varOffset = 0
	p.symbolTable = make(map[string]symbol)
	p.errorReport = make(ErrorReport, 0)

	if len(p.concreteSyntax) == 0 || !p.nextTokenDescription() {
		return p.error(eofReached, nil)
	}

	return nil
}

func (p *parser) block(ts tokens) {
	if p.blockLevel > blockNestingMax {
		p.appendError(p.error(maxBlockLevel, p.blockLevel))
		return
	}

	p.blockLevel++

	for {
		switch p.lastTokenDescription.Token {
		case scanner.ConstWord:

		case scanner.VarWord:

		case scanner.ProcedureWord:
		}

		if !slices.Contains(p.declarations, p.lastTokenDescription.Token) {
			break
		}
	}
}

func (p *parser) constWord() error {
	for {
		if !p.nextTokenDescription() {
			return p.appendError(p.error(eofReached, nil))
		}

		if p.lastTokenDescription.Token != scanner.Identifier {
			return p.appendError(p.error(expectedIdentifier, p.lastTokenDescription.TokenName))
		}

		constantName := p.lastTokenDescription.TokenValue

		if !p.nextTokenDescription() {
			return p.appendError(p.error(eofReached, nil))
		}

		if p.lastTokenDescription.Token == scanner.Becomes {
			p.appendError(p.error(expectedEqual, p.lastTokenDescription.TokenName))
		} else if p.lastTokenDescription.Token != scanner.Equal {
			return p.appendError(p.error(expectedEqual, p.lastTokenDescription.TokenName))
		}

		if !p.nextTokenDescription() {
			return p.appendError(p.error(eofReached, nil))
		}

		if p.lastTokenDescription.Token != scanner.Number {
			return p.appendError(p.error(expectedNumber, p.lastTokenDescription.TokenName))
		}

		p.addSymbol(constantName, constant, p.blockLevel, p.lastTokenDescription.TokenValue)
	}

	return nil
}

func (p *parser) nextTokenDescription() bool {
	if p.concreteSyntaxIndex >= len(p.concreteSyntax) {
		return false
	}

	p.lastTokenDescription = p.concreteSyntax[p.concreteSyntaxIndex]
	p.concreteSyntaxIndex++
	return true
}

func (p *parser) addSymbol(name string, kind entry, level int, value any) {
	s := symbol{
		kind:  kind,
		level: level,
		value: value,
	}

	if kind == variable {
		s.offset = p.varOffset
		p.varOffset++
	}

	p.symbolTable[name] = s
}

func (p *parser) findSymbol(name string) (symbol, bool) {
	s, ok := p.symbolTable[name]
	return s, ok
}

func (p *parser) rebase(expected, expanded tokens, code failure) {
	if !slices.Contains(expected, p.lastTokenDescription.Token) {
		p.appendError(p.error(code, p.lastTokenDescription.Token))

		for next := append(expected, expanded...); !slices.Contains(next, p.lastTokenDescription.Token) && p.lastTokenDescription.Token != scanner.Eof; {
			if !p.nextTokenDescription() {
				p.appendError(p.error(eofReached, nil))
				break
			}
		}
	}
}

func (p *parser) appendError(err error) error {
	p.errorReport = append(p.errorReport, Error{
		Err:         err,
		Line:        p.lastTokenDescription.Line,
		Column:      p.lastTokenDescription.Column,
		CurrentLine: p.lastTokenDescription.CurrentLine,
	})

	return err
}

func (p *parser) error(code failure, value any) error {
	var message string

	if value != nil {
		message = fmt.Sprintf(p.errorMap[code], value)
	} else {
		message = p.errorMap[code]
	}

	line, column := p.lastTokenDescription.Line, p.lastTokenDescription.Column
	return fmt.Errorf("parser error %v [%v,%v]: %v", code, line, column, message)
}
