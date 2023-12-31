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
	maxBlockLevel
	expectedPeriod
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
		blockLevel                            int
		varOffset                             uint64
		lastToken                             scanner.Token
		declarations, statements, expressions tokens
		symbolTable                           map[string]symbol
		errorMap                              map[failure]string
		scanner                               scanner.Scanner
		emitter                               emitter.Emitter
		report                                Report
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
			maxBlockLevel:  "depth of block nesting exceeded (%v)",
			expectedPeriod: "expected period at end of the program",
		},
	}
}

func (p *parser) Parse(s scanner.Scanner, e emitter.Emitter) Report {
	if lastToken, err := s.GetToken(); err != nil {
		p.diagnostic(err, nil)
		return p.report
	} else {
		p.blockLevel = 0
		p.varOffset = 0
		p.lastToken = lastToken
		p.scanner = s
		p.emitter = e
		p.symbolTable = make(map[string]symbol)
		p.report = make(Report, 0)

		p.block(append(append(p.declarations, p.statements...), scanner.Period))

		if p.lastToken != scanner.Period {
			p.diagnostic(p.error(expectedPeriod, nil), nil)
		}

		if len(p.report) > 0 {
			return p.report
		}

		return nil
	}
}

func (p *parser) block(ts tokens) {
	if p.blockLevel > blockNestingMax {
		p.diagnostic(p.error(maxBlockLevel, p.blockLevel), nil)
		return
	}

	p.blockLevel++

	for {
		switch p.lastToken {
		case scanner.ConstWord:

		case scanner.VarWord:

		case scanner.ProcedureWord:
		}

		if !slices.Contains(p.declarations, p.lastToken) {
			break
		}
	}
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
	if !slices.Contains(expected, p.lastToken) {
		p.diagnostic(p.error(code, p.lastToken), expected)

		for next := append(expected, expanded...); !slices.Contains(next, p.lastToken) && p.lastToken != scanner.Eof; {
			lastToken, err := p.scanner.GetToken()

			if err != nil {
				p.diagnostic(err, next)
			} 
			
			p.lastToken = lastToken
		}
	}
}

func (p *parser) diagnostic(err error, msg any) {
	line, column := p.scanner.GetTokenPosition()

	p.report = append(p.report, Diagnostic{
		Err:     err,
		Message: fmt.Sprintf("%v", msg),
		Line:    line,
		Column:  column,
		Source:  p.scanner.GetTokenLine(),
	})
}

func (p *parser) error(code failure, value any) error {
	var message string

	if value != nil {
		message = fmt.Sprintf(p.errorMap[code], value)
	} else {
		message = p.errorMap[code]
	}

	line, column := p.scanner.GetTokenPosition()
	return fmt.Errorf("parser error %v [%v,%v]: %v", code, line, column, message)
}
