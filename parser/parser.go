package parser

import (
	emt "github.com/petersen65/PL0/emitter"
	scn "github.com/petersen65/PL0/scanner"
)

const (
	variableOffsetStart = 3      // start offset of variable in its runtime procedure stack frame
	entryPointName      = "main" // name of the entry point procedure in the symbol table
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

	p.block(entryPointName, 0, set(declarations, statements, scn.Period))

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

func (p *parser) block(procdureName string, declarationDepth int, expected scn.Tokens) {
	// a program start with a block of name 'main' and declaration depth 0
	// a block is defined by its procedure name and its declaration depth
	// a block is a sequence of declarations followed by a statement

	// a declaration is a sequence of
	//   constant,
	//   variable,
	//   and procedure declarations

	// a statement is either
	//   an assignment statement,
	//   a procedure call,
	// 	 an if statement,
	//   a while statement,
	//   or a sequence of statements surrounded by begin and end

	var varOffset uint64 = variableOffsetStart

	if declarationDepth > blockNestingMax {
		p.appendError(p.error(maxBlockDepth, declarationDepth))
	}

	if len(procdureName) == 0 {
		p.appendError(p.error(emptyProcedureName, nil))
	} else {
		p.symbolTable.addProcedure(procdureName, declarationDepth, uint64(p.emitter.GetNextInstructionAddress()))
	}

	jmpInstructionAddress, err := p.emitter.EmitInstruction(declarationDepth, emt.Jmp, emt.NullAddress)
	p.appendError(err)

	for {
		if p.lastToken() == scn.ConstWord {
			p.constWord(declarationDepth)
		}

		if p.lastToken() == scn.VarWord {
			p.varWord(declarationDepth, &varOffset)
		}

		if p.lastToken() == scn.ProcedureWord {
			p.procedureWord(declarationDepth, expected)
		}

		p.rebase(expectedStatementsIdentifiers, set(statements, scn.Identifier), declarations)

		if !p.lastToken().In(declarations) {
			break
		}
	}

	// update the jump instruction address to the first instruction of the block
	p.emitter.UpdateInstructionArgument(jmpInstructionAddress, p.emitter.GetNextInstructionAddress())

	// update the code address of the block's procedure symbol to the first instruction of the block
	if procedureSymbol, ok := p.symbolTable.find(procdureName); ok {
		procedureSymbol.address = uint64(p.emitter.GetNextInstructionAddress())
		p.symbolTable.update(procedureSymbol)
	}

	// allocating stack space for block variables is the first code instruction of the block
	_, err = p.emitter.EmitInstruction(declarationDepth, emt.Inc, emt.Address(varOffset))
	p.appendError(err)

	// parse and emit all statement instructions which are defining the code logic of the block
	p.statement(declarationDepth, set(expected, scn.Semicolon, scn.EndWord))

	// emit a return instruction to return from the block
	_, err = p.emitter.EmitInstruction(declarationDepth, emt.Ret, 0)
	p.appendError(err)

	p.rebase(unexpectedTokens, expected, scn.Empty)
}

func (p *parser) constWord(declarationDepth int) {
	p.nextTokenDescription()

	for {
		p.constantIdentifier(declarationDepth)

		for p.lastToken() == scn.Comma {
			p.nextTokenDescription()
			p.constantIdentifier(declarationDepth)
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

func (p *parser) varWord(declarationDepth int, offset *uint64) {
	p.nextTokenDescription()

	for {
		p.variableIdentifier(declarationDepth, offset)

		for p.lastToken() == scn.Comma {
			p.nextTokenDescription()
			p.variableIdentifier(declarationDepth, offset)
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

func (p *parser) procedureWord(declarationDepth int, expected scn.Tokens) {
	for p.lastToken() == scn.ProcedureWord {
		p.nextTokenDescription()
		var procedureName string

		if p.lastToken() == scn.Identifier {
			procedureName = p.lastTokenValue()
			p.nextTokenDescription()
		} else {
			p.appendError(p.error(expectedIdentifier, p.lastTokenName()))
		}

		if p.lastToken() == scn.Semicolon {
			p.nextTokenDescription()
		} else {
			p.appendError(p.error(expectedSemicolon, p.lastTokenName()))
		}

		p.block(procedureName, declarationDepth+1, set(expected, scn.Semicolon))

		if p.lastToken() == scn.Semicolon {
			p.nextTokenDescription()
			p.rebase(expectedStatementsIdentifiersProcedures, set(statements, scn.Identifier, scn.ProcedureWord), expected)
		} else {
			p.appendError(p.error(expectedSemicolon, p.lastTokenName()))
		}
	}
}

func (p *parser) ifWord(depth int, expected scn.Tokens) {
	p.nextTokenDescription()
	p.condition(set(expected, scn.ThenWord, scn.DoWord))

	if p.lastToken() == scn.ThenWord {
		p.nextTokenDescription()
	} else {
		p.appendError(p.error(expectedThen, p.lastTokenName()))
	}

	jmpInstructionAddress, err := p.emitter.EmitInstruction(depth, emt.Jmp, emt.NullAddress)
	p.appendError(err)
	p.statement(depth, expected)
	p.emitter.UpdateInstructionArgument(jmpInstructionAddress, p.emitter.GetNextInstructionAddress())
}

func (p *parser) whileWord(depth int, expected scn.Tokens) {
	// currentAddress := p.emitter.GetCurrentAddress()
	// p.nextTokenDescription()
	// p.condition(set(expected, scn.DoWord))

	// codeIndex, err := p.emitter.Emit(emt.Jpc, depth, 0)

	/*
	   if sym = whilesym then
	         begin cx1 := cx; getsym; condition([dosym]+fsys);
	            cx2 := cx; gen(jpc, 0, 0);
	            if sym = dosym then getsym else error(18);
	            statement(fsys); gen(jmp, 0, cx1); code[cx2].a := cx
	         end
	*/
}

func (p *parser) constantIdentifier(depth int) {
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

		p.symbolTable.addConstant(constantName, depth, p.lastTokenValue())
		p.nextTokenDescription()
	} else {
		p.appendError(p.error(expectedEqual, p.lastTokenName()))
	}
}

func (p *parser) variableIdentifier(depth int, offset *uint64) {
	if p.lastToken() != scn.Identifier {
		p.appendError(p.error(expectedIdentifier, p.lastTokenName()))
	} else {
		p.symbolTable.addVariable(p.lastTokenValue(), depth, offset)
		p.nextTokenDescription()
	}
}

func (p *parser) statement(depth int, expected scn.Tokens) {
	switch p.lastToken() {
	case scn.Identifier:

	case scn.BeginWord:

	case scn.CallWord:

	case scn.IfWord:
		p.ifWord(depth, expected)

	case scn.WhileWord:
		p.whileWord(depth, expected)
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
