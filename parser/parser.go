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
	_ = failure(iota + 2000)
	maxBlockLevel
	expectedPeriod
)

type (
	failure int
	tokens  []scanner.Token
	symbols map[string]symbol

	symbol struct {
		kind    scanner.Token
		level   int
		value   any
		address uint64
	}

	parser struct {
		level                                 int
		scanner                               scanner.Scanner
		emitter                               emitter.Emitter
		lastToken                             scanner.Token
		declarations, statements, expressions tokens
		table                                 symbols
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
		table: make(symbols, 0),
		errorMap: map[failure]string{
			maxBlockLevel:  "depth of block nesting exceeded (%v)",
			expectedPeriod: "expected period at end of the program",
		},
	}
}

func (p *parser) Parse(s scanner.Scanner, e emitter.Emitter) error {
	if lastToken, err := s.GetToken(); err != nil {
		return err
	} else {
		p.level = 0
		p.scanner = s
		p.emitter = e
		p.lastToken = lastToken

		if err := p.block(append(append(p.declarations, p.statements...), scanner.Period)); err != nil {
			return err
		} else if p.lastToken != scanner.Period {
			return p.error(expectedPeriod, nil)
		}

		return nil
	}
}

func (p *parser) block(ts tokens) error {
	if p.level > blockNestingMax {
		return p.error(maxBlockLevel, p.level)
	}

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

	return nil
}

func (p *parser) addSymbol(name string, kind scanner.Token, level int, value any, address uint64) {
	p.table[name] = symbol{
		kind:    kind,
		level:   level,
		value:   value,
		address: address,
	}
}

func (p *parser) findSymbol(name string) (symbol, bool) {
	s, ok := p.table[name]
	return s, ok
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
