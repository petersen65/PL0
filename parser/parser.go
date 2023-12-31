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
		level                                 int
		varOffset                             uint64
		scanner                               scanner.Scanner
		emitter                               emitter.Emitter
		lastToken                             scanner.Token
		declarations, statements, expressions tokens
		symbolTable                           map[string]symbol
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
		symbolTable: make(map[string]symbol, 0),
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
		p.varOffset = 0
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

	p.level++

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

func (p *parser) addSymbol(name string, kind entry, level int, value any) {
	s := symbol{
		kind:   kind,
		level:  level,
		value:  value,
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

func (p *parser) errorAndForward(expected, expanded tokens, code failure) error {
	if slices.Contains(expected, p.lastToken) {
		return nil
	}

	fmt.Println(p.error(code, p.lastToken))

	for next := append(expected, expanded...); !slices.Contains(next, p.lastToken); {
		if lastToken, err := p.scanner.GetToken(); err != nil {
			return err
		} else {
			p.lastToken = lastToken
		}
	}

	return nil
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
