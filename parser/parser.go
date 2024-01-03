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
	expectedSemicolon
	expectedStatementsIdentifiers
	expectedStatementsIdentifiersProcedures
	unexpectedTokens
)

type (
	failure int
	entry   int

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
		lastTokenDescription, eof             scanner.TokenDescription
		blockLevel                            int
		varOffset                             uint64
		declarations, statements, expressions scanner.Tokens
		symbolTable                           map[string]symbol
		errorReport                           ErrorReport
		errorMap                              map[failure]string
	}
)

func NewParser() Parser {
	return &parser{
		declarations: scanner.Tokens{
			scanner.ConstWord,
			scanner.VarWord,
			scanner.ProcedureWord,
		},
		statements: scanner.Tokens{
			scanner.BeginWord,
			scanner.CallWord,
			scanner.IfWord,
			scanner.WhileWord,
		},
		expressions: scanner.Tokens{
			scanner.Identifier,
			scanner.Number,
			scanner.LeftParenthesis,
		},
		errorMap: map[failure]string{
			eofReached:                              "unexpected end of file",
			parsingErrors:                           "%v parsing errors occurred",
			maxBlockLevel:                           "depth of block nesting exceeded: %v",
			expectedPeriod:                          "expected period at end of the program, found %v",
			expectedIdentifier:                      "expected identifier, found %v",
			expectedEqual:                           "expected equal sign, found %v",
			expectedNumber:                          "expected number, found %v",
			expectedSemicolon:                       "expected semicolon, found %v",
			expectedStatementsIdentifiers:           "expected statements or identifiers, found %v",
			expectedStatementsIdentifiersProcedures: "expected statements, identifiers or procedures, found %v",
			unexpectedTokens:                        "unexpected set of tokens, found %v",
		},
	}
}

func (p *parser) Parse(concreteSyntax scanner.ConcreteSyntax, emitter emitter.Emitter) (ErrorReport, error) {
	if err := p.reset(concreteSyntax, emitter); err != nil {
		return p.errorReport, err
	}

	/*
		cc := 0;
		cx := 0;
		ll := 0;
		ch := ' ';
		kk := al;
	*/

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

func (p *parser) block(expected scanner.Tokens) {
	/*
	   	var dx: integer;     {data allocation index}
	       tx0: integer;     {initial table index}
	       cx0: integer;     {initial code index}
	*/

	/*
		dx:=3;
		tx0:=tx;
		table[tx].adr:=cx;
		gen(jmp,0,0);
	*/

	if p.blockLevel > blockNestingMax {
		p.appendError(p.error(maxBlockLevel, p.blockLevel))
	}

	p.blockLevel++

	for {
		switch p.lastTokenDescription.Token {
		case scanner.ConstWord:
			p.constWord()

		case scanner.VarWord:
			p.varWord()

		case scanner.ProcedureWord:
			p.procedureWord(expected)
		}

		p.rebase(
			append(p.statements, scanner.Identifier),
			p.declarations,
			expectedStatementsIdentifiers)

		if !slices.Contains(p.declarations, p.lastTokenDescription.Token) {
			break
		}
	}

	/*
		code[table[tx0].adr].a := cx;

		with table[tx0] do
		begin
			adr := cx; {start adr of code}
		end;

		cx0 := 0{cx};
		gen(int, 0, dx);
	*/

	p.statement(append(append(expected, scanner.Semicolon), scanner.EndWord))

	/*
		gen(opr, 0, 0); {return}
	*/

	p.rebase(expected, tokens{}, unexpectedSetOfTokens)
}

func (p *parser) constWord() error {
	p.nextTokenDescription()

	for {
		p.identifierEqualNumber()

		for p.lastTokenDescription.Token == scanner.Comma {
			p.nextTokenDescription()
			p.identifierEqualNumber()
		}

		if p.lastTokenDescription.Token == scanner.Semicolon {
			p.nextTokenDescription()
		} else {
			p.appendError(p.error(expectedSemicolon, p.lastTokenDescription.TokenName))
		}

		if p.lastTokenDescription.Token != scanner.Identifier {
			break
		}
	}

	return p.lastError()
}

func (p *parser) varWord() error {
	p.nextTokenDescription()

	for {
		p.identifierSymbolTable()

		for p.lastTokenDescription.Token == scanner.Comma {
			p.nextTokenDescription()
			p.identifierSymbolTable()
		}

		if p.lastTokenDescription.Token == scanner.Semicolon {
			p.nextTokenDescription()
		} else {
			p.appendError(p.error(expectedSemicolon, p.lastTokenDescription.TokenName))
		}

		if p.lastTokenDescription.Token != scanner.Identifier {
			break
		}
	}

	return p.lastError()
}

func (p *parser) procedureWord(expected scanner.Tokens) error {
	for p.lastTokenDescription.Token == scanner.ProcedureWord {
		p.nextTokenDescription()

		if p.lastTokenDescription.Token == scanner.Identifier {
			p.addSymbol(p.lastTokenDescription.TokenValue, procedure, p.blockLevel, nil)
			p.nextTokenDescription()
		} else {
			p.appendError(p.error(expectedIdentifier, p.lastTokenDescription.TokenName))
		}

		if p.lastTokenDescription.Token == scanner.Semicolon {
			p.nextTokenDescription()
		} else {
			p.appendError(p.error(expectedSemicolon, p.lastTokenDescription.TokenName))
		}

		p.block(append(expected, scanner.Semicolon))

		if p.lastTokenDescription.Token == scanner.Semicolon {
			p.nextTokenDescription()

			p.rebase(
				append(append(p.statements, scanner.Identifier), scanner.ProcedureWord),
				expected,
				expectedStatementsIdentifiersProcedures)
		} else {
			p.appendError(p.error(expectedSemicolon, p.lastTokenDescription.TokenName))
		}
	}

	return p.lastError()
}

func (p *parser) statement(expected scanner.Tokens) error {
	// TODO: implement statement
	return p.lastError()
}

func (p *parser) identifierEqualNumber() error {
	if p.lastTokenDescription.Token != scanner.Identifier {
		return p.appendError(p.error(expectedIdentifier, p.lastTokenDescription.TokenName))
	}

	constantName := p.lastTokenDescription.TokenValue
	p.nextTokenDescription()

	if slices.Contains(tokens{scanner.Becomes, scanner.Equal}, p.lastTokenDescription.Token) {
		if p.lastTokenDescription.Token == scanner.Becomes {
			p.appendError(p.error(expectedEqual, p.lastTokenDescription.TokenName))
		}

		p.nextTokenDescription()

		if p.lastTokenDescription.Token != scanner.Number {
			return p.appendError(p.error(expectedNumber, p.lastTokenDescription.TokenName))
		}

		p.addSymbol(constantName, constant, p.blockLevel, p.lastTokenDescription.TokenValue)
		p.nextTokenDescription()
	} else {
		return p.appendError(p.error(expectedEqual, p.lastTokenDescription.TokenName))
	}

	return nil
}

func (p *parser) identifierSymbolTable() error {
	if p.lastTokenDescription.Token != scanner.Identifier {
		return p.appendError(p.error(expectedIdentifier, p.lastTokenDescription.TokenName))
	}

	p.addSymbol(p.lastTokenDescription.TokenValue, variable, p.blockLevel, nil)
	p.nextTokenDescription()
	return nil
}

func (p *parser) nextTokenDescription() bool {
	if p.concreteSyntaxIndex >= len(p.concreteSyntax) {
		if p.eof.Token == scanner.Null {
			p.eof = scanner.TokenDescription{
				Token:       scanner.Eof,
				TokenName:   "eof",
				TokenValue:  "",
				Line:        p.lastTokenDescription.Line,
				Column:      p.lastTokenDescription.Column,
				CurrentLine: p.lastTokenDescription.CurrentLine,
			}

			p.lastTokenDescription = p.eof
		}

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

func (p *parser) rebase(code failure, expected, expanded scanner.Tokens) {
	if !p.lastTokenDescription.Token.In(expected) {
		p.appendError(p.error(code, p.lastTokenDescription.Token))

		for set := scanner.Eof.Union(expected.Union(expanded)); !p.lastTokenDescription.Token.In(set); {
			p.nextTokenDescription()
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

func (p *parser) lastError() error {
	if len(p.errorReport) > 0 {
		return p.errorReport[len(p.errorReport)-1].Err
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

	line, column := p.lastTokenDescription.Line, p.lastTokenDescription.Column
	return fmt.Errorf("parser error %v [%v,%v]: %v", code, line, column, message)
}
