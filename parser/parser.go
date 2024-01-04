package parser

import (
	emt "github.com/petersen65/PL0/emitter"
	scn "github.com/petersen65/PL0/scanner"
)

type (
	parser struct {
		concreteSyntaxIndex       int
		concreteSyntax            scn.ConcreteSyntax
		emitter                   emt.Emitter
		lastTokenDescription, eof scn.TokenDescription
		blockLevel                int
		varOffset                 uint64
		symbolTable               map[string]symbol
		errorReport               ErrorReport
	}
)

func (p *parser) reset(concreteSyntax scn.ConcreteSyntax, emitter emt.Emitter) error {
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

func (p *parser) block(expected scn.Tokens) {
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
		switch p.lastToken() {
		case scn.ConstWord:
			p.constWord()

		case scn.VarWord:
			p.varWord()

		case scn.ProcedureWord:
			p.procedureWord(expected)
		}

		p.rebase(expectedStatementsIdentifiers, set(statements, scn.Identifier), declarations)

		if !p.lastToken().In(declarations) {
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

	p.statement(set(expected, scn.Semicolon, scn.EndWord))

	/*
		gen(opr, 0, 0); {return}
	*/

	p.rebase(unexpectedTokens, expected, scn.Empty)
}

func (p *parser) constWord() error {
	p.nextTokenDescription()

	for {
		p.identifierEqualNumber()

		for p.lastToken() == scn.Comma {
			p.nextTokenDescription()
			p.identifierEqualNumber()
		}

		if p.lastToken() == scn.Semicolon {
			p.nextTokenDescription()
		} else {
			p.appendError(p.error(expectedSemicolon, p.lastTokenName()))
		}

		if p.lastToken() != scn.Identifier {
			break
		}
	}

	return p.lastError()
}

func (p *parser) varWord() error {
	p.nextTokenDescription()

	for {
		p.identifierSymbolTable()

		for p.lastToken() == scn.Comma {
			p.nextTokenDescription()
			p.identifierSymbolTable()
		}

		if p.lastToken() == scn.Semicolon {
			p.nextTokenDescription()
		} else {
			p.appendError(p.error(expectedSemicolon, p.lastTokenName()))
		}

		if p.lastToken() != scn.Identifier {
			break
		}
	}

	return p.lastError()
}

func (p *parser) procedureWord(expected scn.Tokens) error {
	for p.lastToken() == scn.ProcedureWord {
		p.nextTokenDescription()

		if p.lastToken() == scn.Identifier {
			p.addSymbol(p.lastTokenValue(), procedure, p.blockLevel, nil)
			p.nextTokenDescription()
		} else {
			p.appendError(p.error(expectedIdentifier, p.lastTokenName()))
		}

		if p.lastToken() == scn.Semicolon {
			p.nextTokenDescription()
		} else {
			p.appendError(p.error(expectedSemicolon, p.lastTokenName()))
		}

		p.block(set(expected, scn.Semicolon))

		if p.lastToken() == scn.Semicolon {
			p.nextTokenDescription()
			p.rebase(expectedStatementsIdentifiersProcedures, set(statements, scn.Identifier, scn.ProcedureWord), expected)
		} else {
			p.appendError(p.error(expectedSemicolon, p.lastTokenName()))
		}
	}

	return p.lastError()
}

func (p *parser) statement(expected scn.Tokens) error {
	// TODO: implement statement
	return p.lastError()
}

func (p *parser) identifierEqualNumber() error {
	if p.lastToken() != scn.Identifier {
		return p.appendError(p.error(expectedIdentifier, p.lastTokenName()))
	}

	constantName := p.lastTokenValue()
	p.nextTokenDescription()

	if p.lastToken().In(set(scn.Equal, scn.Becomes)) {
		if p.lastToken() == scn.Becomes {
			p.appendError(p.error(expectedEqual, p.lastTokenName()))
		}

		p.nextTokenDescription()

		if p.lastToken() != scn.Number {
			return p.appendError(p.error(expectedNumber, p.lastTokenName()))
		}

		p.addSymbol(constantName, constant, p.blockLevel, p.lastTokenValue())
		p.nextTokenDescription()
	} else {
		return p.appendError(p.error(expectedEqual, p.lastTokenName()))
	}

	return nil
}

func (p *parser) identifierSymbolTable() error {
	if p.lastToken() != scn.Identifier {
		return p.appendError(p.error(expectedIdentifier, p.lastTokenName()))
	}

	p.addSymbol(p.lastTokenValue(), variable, p.blockLevel, nil)
	p.nextTokenDescription()
	return nil
}
