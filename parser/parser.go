package parser

import (
	"fmt"
	"slices"

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

type failure int
type tokenSet []scanner.Token

type parser struct {
	level                                 int
	scanner                               scanner.Scanner
	lastToken                             scanner.Token
	declarations, statements, expressions tokenSet
	errorMap                              map[failure]string
}

func NewParser() Parser {
	return &parser{
		declarations: tokenSet{
			scanner.ConstSymbol,
			scanner.VarSymbol,
			scanner.ProcedureSymbol,
		},
		statements: tokenSet{
			scanner.BeginSymbol,
			scanner.CallSymbol,
			scanner.IfSymbol,
			scanner.WhileSymbol,
		},
		expressions: tokenSet{
			scanner.Identifier,
			scanner.Number,
			scanner.LeftParenthesis,
		},
		errorMap: map[failure]string{
			maxBlockLevel: "depth of block nesting exceeded (%v)",
			expectedPeriod: "expected period at end of the program",
		},
	}
}

func (p *parser) Parse(s scanner.Scanner) error {
	if lastToken, err := s.GetToken(); err != nil {
		return err
	} else {
		p.level = 0
		p.scanner = s
		p.lastToken = lastToken
		
		if err := p.block(append(append(p.declarations, p.statements...), scanner.Period)); err != nil {
			return err
		} else if p.lastToken != scanner.Period {
			return p.error(expectedPeriod, nil)
		}

		return nil
	}
}

func (p *parser) block(ts tokenSet) error {
	if p.level > blockNestingMax {
		return p.error(maxBlockLevel, p.level)
	}

	for {
		switch p.lastToken {
		case scanner.ConstSymbol:
		
		case scanner.VarSymbol:
	
		case scanner.ProcedureSymbol:
		}

		if !slices.Contains(p.declarations, p.lastToken) {
			break
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
