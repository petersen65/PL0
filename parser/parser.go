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
		symbolTable               symbolTable
		errorReport               ErrorReport
	}
)

func (p *parser) parse(concreteSyntax scn.ConcreteSyntax, emitter emt.Emitter) (ErrorReport, error) {
	if err := p.reset(concreteSyntax, emitter); err != nil {
		return p.errorReport, err
	}

	p.symbolTable.addProcedure(entryPointName, 0)
	p.block(0, set(declarations, statements, scn.Period))

	if p.lastToken() != scn.Period {
		p.appendError(p.error(expectedPeriod, p.lastTokenDescription.TokenName))
	}

	if len(p.errorReport) > 0 {
		return p.errorReport, p.error(parsingErrors, len(p.errorReport))
	} else {
		return p.errorReport, nil
	}
}

func (p *parser) reset(concreteSyntax scn.ConcreteSyntax, emitter emt.Emitter) error {
	p.concreteSyntaxIndex = 0
	p.concreteSyntax = concreteSyntax
	p.emitter = emitter
	p.symbolTable = symbolTable{}
	p.errorReport = make(ErrorReport, 0)

	if len(p.concreteSyntax) == 0 || !p.nextTokenDescription() {
		return p.error(eofReached, nil)
	}

	return nil
}

func (p *parser) block(blockLevel int, expected scn.Tokens) {
	if blockLevel > blockNestingMax {
		p.appendError(p.error(maxBlockLevel, blockLevel))
	}

	var varOffset uint64 = variableOffsetMin
	jmpInstructionAddress, err := p.emitter.EmitInstruction(blockLevel, emt.Jmp, emt.NullAddress)
	p.appendError(err)

	for {
		if p.lastToken() == scn.ConstWord {
			p.constWord(blockLevel)
		}

		if p.lastToken() == scn.VarWord {
			p.varWord(blockLevel, &varOffset)
		}

		if p.lastToken() == scn.ProcedureWord {
			p.procedureWord(blockLevel, expected)
		}

		p.rebase(expectedStatementsIdentifiers, set(statements, scn.Identifier), declarations)

		if !p.lastToken().In(declarations) {
			break
		}
	}

	p.emitter.UpdateInstructionArgument(jmpInstructionAddress, p.emitter.GetNextInstructionAddress())
	_, err = p.emitter.EmitInstruction(blockLevel, emt.Inc, emt.Address(varOffset))
	p.appendError(err)

	p.statement(blockLevel, set(expected, scn.Semicolon, scn.EndWord))
	_, err = p.emitter.EmitInstruction(blockLevel, emt.Opr, emt.ReturnOperator)
	p.appendError(err)

	p.rebase(unexpectedTokens, expected, scn.Empty)
	p.symbolTable.removeLevel(blockLevel)
}

func (p *parser) constWord(level int) {
	p.nextTokenDescription()

	for {
		p.constantIdentifier(level)

		for p.lastToken() == scn.Comma {
			p.nextTokenDescription()
			p.constantIdentifier(level)
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
}

func (p *parser) varWord(level int, offset *uint64) {
	p.nextTokenDescription()

	for {
		p.variableIdentifier(level, offset)

		for p.lastToken() == scn.Comma {
			p.nextTokenDescription()
			p.variableIdentifier(level, offset)
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
}

func (p *parser) procedureWord(level int, expected scn.Tokens) {
	for p.lastToken() == scn.ProcedureWord {
		p.nextTokenDescription()

		if p.lastToken() == scn.Identifier {
			p.symbolTable.addProcedure(p.lastTokenValue(), level)
			p.nextTokenDescription()
		} else {
			p.appendError(p.error(expectedIdentifier, p.lastTokenName()))
		}

		if p.lastToken() == scn.Semicolon {
			p.nextTokenDescription()
		} else {
			p.appendError(p.error(expectedSemicolon, p.lastTokenName()))
		}

		p.block(level+1, set(expected, scn.Semicolon))

		if p.lastToken() == scn.Semicolon {
			p.nextTokenDescription()
			p.rebase(expectedStatementsIdentifiersProcedures, set(statements, scn.Identifier, scn.ProcedureWord), expected)
		} else {
			p.appendError(p.error(expectedSemicolon, p.lastTokenName()))
		}
	}
}

func (p *parser) ifWord(level int, expected scn.Tokens) {
	p.nextTokenDescription()
	p.condition(set(expected, scn.ThenWord, scn.DoWord))

	if p.lastToken() == scn.ThenWord {
		p.nextTokenDescription()
	} else {
		p.appendError(p.error(expectedThen, p.lastTokenName()))
	}

	jmpInstructionAddress, err := p.emitter.EmitInstruction(level, emt.Jmp, emt.NullAddress)
	p.appendError(err)
	p.statement(level, expected)
	p.emitter.UpdateInstructionArgument(jmpInstructionAddress, p.emitter.GetNextInstructionAddress())
}

func (p *parser) whileWord(level int, expected scn.Tokens) {
	currentAddress := p.emitter.GetCurrentAddress()
	p.nextTokenDescription()
	p.condition(set(expected, scn.DoWord))

	codeIndex, err := p.emitter.Emit(emt.Jpc, level, 0)

	/*
	   if sym = whilesym then
	         begin cx1 := cx; getsym; condition([dosym]+fsys);
	            cx2 := cx; gen(jpc, 0, 0);
	            if sym = dosym then getsym else error(18);
	            statement(fsys); gen(jmp, 0, cx1); code[cx2].a := cx
	         end
	*/
}

func (p *parser) constantIdentifier(level int) {
	if p.lastToken() != scn.Identifier {
		p.appendError(p.error(expectedIdentifier, p.lastTokenName()))
		return
	}

	constantName := p.lastTokenValue()
	p.nextTokenDescription()

	if p.lastToken().In(set(scn.Equal, scn.Becomes)) {
		if p.lastToken() == scn.Becomes {
			p.appendError(p.error(expectedEqual, p.lastTokenName()))
		}

		p.nextTokenDescription()

		if p.lastToken() != scn.Number {
			p.appendError(p.error(expectedNumber, p.lastTokenName()))
			return
		}

		p.symbolTable.addConstant(constantName, level, p.lastTokenValue())
		p.nextTokenDescription()
	} else {
		p.appendError(p.error(expectedEqual, p.lastTokenName()))
	}
}

func (p *parser) variableIdentifier(level int, offset *uint64) {
	if p.lastToken() != scn.Identifier {
		p.appendError(p.error(expectedIdentifier, p.lastTokenName()))
	} else {
		p.symbolTable.addVariable(p.lastTokenValue(), level, offset)
		p.nextTokenDescription()
	}
}

func (p *parser) statement(level int, expected scn.Tokens) {
	switch p.lastToken() {
	case scn.Identifier:

	case scn.BeginWord:

	case scn.CallWord:

	case scn.IfWord:
		p.ifWord(level, expected)

	case scn.WhileWord:
		p.whileWord(level, expected)
	}

	p.rebase(expectedStatement, expected, scn.Empty)
}

func (p *parser) expression(expected scn.Tokens) {
	// TODO: implement expression
}

func (p *parser) condition(expected scn.Tokens) {
	// TODO: implement condition
}

func (p *parser) term(expected scn.Tokens) {
	// TODO: implement term
}

func (p *parser) factor(expected scn.Tokens) {
	// TODO: implement factor
}
