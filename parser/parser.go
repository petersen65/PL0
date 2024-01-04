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
	blockProcedure, ok := p.findKind(procedure)

	if !ok || blockProcedure.level != p.blockLevel {
		p.appendError(p.error(blockProcedureNotFound, p.blockLevel))
	}

	if p.blockLevel > blockNestingMax {
		p.appendError(p.error(maxBlockLevel, p.blockLevel))
	}

	codeIndex, err := p.emitter.Emit(emt.Jmp, p.blockLevel, 0) // jump forward to block code beginning

	if err != nil {
		p.appendError(err)
	}

	var varOffset uint64 = variableOffsetMin // space for static link, dynamic link, and return address
	p.blockLevel++

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

	p.removeLevel(p.blockLevel)
	p.blockLevel--
	p.emitter.UpdateAddress(codeIndex, p.emitter.GetCurrentAddress()) // update jump address with block code beginning

	if _, err := p.emitter.Emit(emt.Inc, p.blockLevel, emt.Address(varOffset)); err != nil {
		p.appendError(err)
	}

	p.statement(set(expected, scn.Semicolon, scn.EndWord))

	if _, err := p.emitter.Emit(emt.Opr, p.blockLevel, 0); err != nil { // return from block code end
		p.appendError(err)
	}

	p.rebase(unexpectedTokens, expected, scn.Empty)
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
