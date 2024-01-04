package parser

import (
	emt "github.com/petersen65/PL0/emitter"
	scn "github.com/petersen65/PL0/scanner"
)

const (
	variableOffsetMin = 3      // start of variable offset within each block
	entryPointName    = "main" // name of the entry point procedure
)

type (
	parser struct {
		concreteSyntaxIndex       int
		concreteSyntax            scn.ConcreteSyntax
		emitter                   emt.Emitter
		lastTokenDescription, eof scn.TokenDescription
		blockLevel                int
		symbolTable               table
		errorReport               ErrorReport
	}
)

func (p *parser) reset(concreteSyntax scn.ConcreteSyntax, emitter emt.Emitter) error {
	p.concreteSyntaxIndex = 0
	p.concreteSyntax = concreteSyntax
	p.emitter = emitter
	p.blockLevel = 0
	p.symbolTable = make(table, 0)
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
		table[tx].adr:=cx; update adress of current procedure
		gen(jmp,0,0);
	*/

	blockProcedure, ok := p.findKind(procedure)

	if !ok || blockProcedure.level != p.blockLevel {
		p.appendError(p.error(blockProcedureNotFound, nil))
	}

	if p.blockLevel > blockNestingMax {
		p.appendError(p.error(maxBlockLevel, p.blockLevel))
	}

	p.blockLevel++
	var varOffset uint64 = variableOffsetMin

	for {
		switch p.lastToken() {
		case scn.ConstWord:
			p.constWord()

		case scn.VarWord:
			p.varWord(&varOffset)

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
	p.blockLevel--
}

func (p *parser) constWord() error {
	p.nextTokenDescription()

	for {
		p.constantIdentifier()

		for p.lastToken() == scn.Comma {
			p.nextTokenDescription()
			p.constantIdentifier()
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

func (p *parser) varWord(offset *uint64) error {
	p.nextTokenDescription()

	for {
		p.variableIdentifier(offset)

		for p.lastToken() == scn.Comma {
			p.nextTokenDescription()
			p.variableIdentifier(offset)
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
			p.addProcedure(p.lastTokenValue())
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

func (p *parser) constantIdentifier() error {
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

		p.addConstant(constantName, p.lastTokenValue())
		p.nextTokenDescription()
	} else {
		return p.appendError(p.error(expectedEqual, p.lastTokenName()))
	}

	return nil
}

func (p *parser) variableIdentifier(offset *uint64) error {
	if p.lastToken() != scn.Identifier {
		return p.appendError(p.error(expectedIdentifier, p.lastTokenName()))
	}

	p.addVariable(p.lastTokenValue(), offset)
	p.nextTokenDescription()
	return nil
}

func (p *parser) statement(expected scn.Tokens) error {
	// TODO: implement statement
	return p.lastError()
}

func (p *parser) expression(expected scn.Tokens) error {
	// TODO: implement expression
	return p.lastError()
}

func (p *parser) condition(expected scn.Tokens) error {
	// TODO: implement condition
	return p.lastError()
}

func (p *parser) term(expected scn.Tokens) error {
	// TODO: implement term
	return p.lastError()
}

func (p *parser) factor(expected scn.Tokens) error {
	// TODO: implement factor
	return p.lastError()
}
