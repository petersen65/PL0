// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

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

	if len(p.errorReport) == 1 {
		return p.errorReport, p.error(parsingError, nil)
	} else if len(p.errorReport) > 1 {
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

func (p *parser) block(procdureName string, depth int32, expected scn.Tokens) {
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

	if depth > blockNestingMax {
		p.appendError(p.error(maxBlockDepth, depth))
	}

	if len(procdureName) == 0 {
		p.appendError(p.error(emptyProcedureName, nil))
	} else {
		p.symbolTable.addProcedure(procdureName, depth, uint64(p.emitter.GetNextAddress()))
	}

	// emit a jmp to the first instruction of the block which is not yet known
	jumpFirstBlockInstruction, err := p.emitter.Emit(depth, emt.Jmp, emt.NullAddress)
	p.appendError(err)

	for {
		if p.lastToken() == scn.ConstWord {
			p.constWord(depth)
		}

		if p.lastToken() == scn.VarWord {
			p.varWord(depth, &varOffset)
		}

		if p.lastToken() == scn.ProcedureWord {
			p.procedureWord(depth, expected)
		}

		p.rebase(expectedStatementsIdentifiers, set(statements, scn.Identifier), declarations)

		if !p.lastToken().In(declarations) {
			break
		}
	}

	// update the jump instruction address to the first instruction of the block
	p.emitter.UpdateArgument(jumpFirstBlockInstruction, p.emitter.GetNextAddress())

	// update the code address of the block's procedure symbol to the first instruction of the block
	if procedureSymbol, ok := p.symbolTable.find(procdureName); ok {
		procedureSymbol.address = uint64(p.emitter.GetNextAddress())
		p.symbolTable.update(procedureSymbol)
	}

	// allocating stack space for block variables is the first code instruction of the block
	_, err = p.emitter.Emit(depth, emt.Inc, emt.Address(varOffset))
	p.appendError(err)

	// parse and emit all statement instructions which are defining the code logic of the block
	p.statement(depth, set(expected, scn.Semicolon, scn.EndWord))

	// emit a return instruction to return from the block
	_, err = p.emitter.Emit(depth, emt.Ret, 0)
	p.appendError(err)

	p.rebase(unexpectedTokens, expected, scn.Empty)
}

func (p *parser) constWord(depth int32) {
	p.nextTokenDescription()

	for {
		p.constantIdentifier(depth)

		for p.lastToken() == scn.Comma {
			p.nextTokenDescription()
			p.constantIdentifier(depth)
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

func (p *parser) varWord(depth int32, offset *uint64) {
	p.nextTokenDescription()

	for {
		p.variableIdentifier(depth, offset)

		for p.lastToken() == scn.Comma {
			p.nextTokenDescription()
			p.variableIdentifier(depth, offset)
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

func (p *parser) procedureWord(depth int32, expected scn.Tokens) {
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

		p.block(procedureName, depth+1, set(expected, scn.Semicolon))

		if p.lastToken() == scn.Semicolon {
			p.nextTokenDescription()
			p.rebase(expectedStatementsIdentifiersProcedures, set(statements, scn.Identifier, scn.ProcedureWord), expected)
		} else {
			p.appendError(p.error(expectedSemicolon, p.lastTokenName()))
		}
	}
}

func (p *parser) assignment(depth int32, expected scn.Tokens) {
	symbol, ok := p.symbolTable.find(p.lastTokenValue())

	if !ok {
		p.appendError(p.error(identifierNotFound, p.lastTokenValue()))
	} else if symbol.kind != variable {
		p.appendError(p.error(expectedVariableIdentifier, p.lastTokenName()))
	}

	p.nextTokenDescription()

	if p.lastToken() == scn.Becomes {
		p.nextTokenDescription()
	} else {
		p.appendError(p.error(expectedEqual, p.lastTokenName()))
	}

	p.expression(depth, expected)

	if ok && symbol.kind == variable {
		p.emitter.Emit(depth-symbol.depth, emt.Sto, emt.Address(symbol.address))

	}
}

func (p *parser) beginWord(depth int32, expected scn.Tokens) {
	p.nextTokenDescription()
	p.statement(depth, set(expected, scn.EndWord, scn.Semicolon))

	for p.lastToken().In(set(statements, scn.Semicolon)) {
		if p.lastToken() == scn.Semicolon {
			p.nextTokenDescription()
		} else {
			p.appendError(p.error(expectedSemicolon, p.lastTokenName()))
		}

		p.statement(depth, set(expected, scn.EndWord, scn.Semicolon))

	}

	if p.lastToken() == scn.EndWord {
		p.nextTokenDescription()
	} else {
		p.appendError(p.error(expectedEnd, p.lastTokenName()))
	}
}

func (p *parser) callWord(depth int32, expected scn.Tokens) {
	p.nextTokenDescription()

	if p.lastToken() != scn.Identifier {
		p.appendError(p.error(expectedIdentifier, p.lastTokenName()))
	} else {
		if symbol, ok := p.symbolTable.find(p.lastTokenValue()); ok {
			if symbol.kind == procedure {
				p.emitter.Emit(depth-symbol.depth, emt.Cal, emt.Address(symbol.address))
			} else {
				p.appendError(p.error(expectedProcedureIdentifier, p.lastTokenName()))
			}
		} else {
			p.appendError(p.error(identifierNotFound, p.lastTokenValue()))
		}

		p.nextTokenDescription()
	}
}

func (p *parser) ifWord(depth int32, expected scn.Tokens) {
	p.nextTokenDescription()
	p.condition(depth, set(expected, scn.ThenWord, scn.DoWord))

	if p.lastToken() == scn.ThenWord {
		p.nextTokenDescription()
	} else {
		p.appendError(p.error(expectedThen, p.lastTokenName()))
	}

	ifDecision, err := p.emitter.Emit(depth, emt.Jpc, emt.NullAddress)
	p.appendError(err)
	p.statement(depth, expected)
	p.emitter.UpdateArgument(ifDecision, p.emitter.GetNextAddress())
}

func (p *parser) whileWord(depth int32, expected scn.Tokens) {
	p.nextTokenDescription()
	whileCondition := p.emitter.GetNextAddress()
	p.condition(depth, set(expected, scn.DoWord))
	whileDecision, err := p.emitter.Emit(depth, emt.Jpc, emt.NullAddress)
	p.appendError(err)

	if p.lastToken() == scn.DoWord {
		p.nextTokenDescription()
	} else {
		p.appendError(p.error(expectedDo, p.lastTokenName()))
	}

	p.statement(depth, expected)
	_, err = p.emitter.Emit(depth, emt.Jmp, whileCondition)
	p.appendError(err)
	p.emitter.UpdateArgument(whileDecision, p.emitter.GetNextAddress())
}

func (p *parser) constantIdentifier(depth int32) {
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

		p.symbolTable.addConstant(constantName, depth, p.lastTokenNumber())
		p.nextTokenDescription()
	} else {
		p.appendError(p.error(expectedEqual, p.lastTokenName()))
	}
}

func (p *parser) variableIdentifier(depth int32, offset *uint64) {
	if p.lastToken() != scn.Identifier {
		p.appendError(p.error(expectedIdentifier, p.lastTokenName()))
	} else {
		p.symbolTable.addVariable(p.lastTokenValue(), depth, offset)
		p.nextTokenDescription()
	}
}

func (p *parser) statement(depth int32, expected scn.Tokens) {
	// a statement is either
	//   an assignment statement,
	//   a procedure call,
	// 	 an if statement,
	//   a while statement,
	//   or a sequence of statements surrounded by begin and end

	switch p.lastToken() {
	case scn.Identifier:
		p.assignment(depth, expected)

	case scn.CallWord:
		p.callWord(depth, expected)

	case scn.IfWord:
		p.ifWord(depth, expected)

	case scn.WhileWord:
		p.whileWord(depth, expected)

	case scn.BeginWord:
		p.beginWord(depth, expected)
	}

	p.rebase(expectedStatement, expected, scn.Empty)
}

func (p *parser) expression(depth int32, expected scn.Tokens) {
	// an expression is a sequence of terms separated by plus or minus

	// handle leading plus or minus sign of a term
	if p.lastToken() == scn.Plus || p.lastToken() == scn.Minus {
		plusOrMinus := p.lastToken()
		p.nextTokenDescription()

		// handle left term of a plus or minus operator
		p.term(depth, set(expected, scn.Plus, scn.Minus))

		if plusOrMinus == scn.Minus {
			p.emitter.Emit(0, emt.Opr, emt.Neg)
		}
	} else {
		// handle left term of a plus or minus operator
		p.term(depth, set(expected, scn.Plus, scn.Minus))
	}

	for p.lastToken() == scn.Plus || p.lastToken() == scn.Minus {
		plusOrMinus := p.lastToken()
		p.nextTokenDescription()

		// handle right term of a plus or minus operator
		p.term(depth, set(expected, scn.Plus, scn.Minus))

		if plusOrMinus == scn.Plus {
			p.emitter.Emit(0, emt.Opr, emt.Add)
		} else {
			p.emitter.Emit(0, emt.Opr, emt.Sub)
		}
	}
}

func (p *parser) condition(depth int32, expected scn.Tokens) {
	if p.lastToken() == scn.OddWord {
		p.nextTokenDescription()
		p.expression(depth, expected)
		p.emitter.Emit(0, emt.Opr, emt.Odd)
	} else {
		p.expression(depth, set(expected, scn.Equal, scn.NotEqual, scn.Less, scn.LessEqual, scn.Greater, scn.GreaterEqual))

		if !p.lastToken().In(set(scn.Equal, scn.NotEqual, scn.Less, scn.LessEqual, scn.Greater, scn.GreaterEqual)) {
			p.appendError(p.error(expectedRelationalOperator, p.lastTokenName()))
		} else {
			relationalOperator := p.lastToken()
			p.nextTokenDescription()
			p.expression(depth, expected)

			switch relationalOperator {
			case scn.Equal:
				p.emitter.Emit(0, emt.Opr, emt.Eq)

			case scn.NotEqual:
				p.emitter.Emit(0, emt.Opr, emt.Neq)

			case scn.Less:
				p.emitter.Emit(0, emt.Opr, emt.Lss)

			case scn.LessEqual:
				p.emitter.Emit(0, emt.Opr, emt.Leq)

			case scn.Greater:
				p.emitter.Emit(0, emt.Opr, emt.Gtr)

			case scn.GreaterEqual:
				p.emitter.Emit(0, emt.Opr, emt.Geq)
			}
		}
	}
}

func (p *parser) term(depth int32, expected scn.Tokens) {
	// a term is a sequence of factors separated by times or divide

	// handle left factor of a times or divide operator
	p.factor(depth, set(expected, scn.Times, scn.Divide))

	for p.lastToken() == scn.Times || p.lastToken() == scn.Divide {
		timesOrDevide := p.lastToken()
		p.nextTokenDescription()

		// handle right factor of a times or divide operator
		p.factor(depth, set(expected, scn.Times, scn.Divide))

		if timesOrDevide == scn.Times {
			p.emitter.Emit(0, emt.Opr, emt.Mul)
		} else {
			p.emitter.Emit(0, emt.Opr, emt.Div)
		}
	}
}

func (p *parser) factor(depth int32, expected scn.Tokens) {
	// a factor is either an identifier, a number, or an expression surrounded by parentheses
	p.rebase(expectedIdentifiersNumbersExpressions, factors, expected)

	for p.lastToken().In(factors) {
		if p.lastToken() == scn.Identifier {
			if symbol, ok := p.symbolTable.find(p.lastTokenValue()); ok {
				switch symbol.kind {
				case constant:
					p.emitter.Emit(0, emt.Lit, emt.Address(symbol.value))

				case variable:
					p.emitter.Emit(depth-symbol.depth, emt.Lod, emt.Address(symbol.offset))

				case procedure:
					p.appendError(p.error(expectedConstantsVariables, p.lastTokenName()))
				}
			} else {
				p.appendError(p.error(identifierNotFound, p.lastTokenValue()))
			}

			p.nextTokenDescription()
		} else if p.lastToken() == scn.Number {
			p.emitter.Emit(0, emt.Lit, emt.Address(p.lastTokenNumber()))
			p.nextTokenDescription()
		} else if p.lastToken() == scn.LeftParenthesis {
			p.nextTokenDescription()
			p.expression(depth, set(expected, scn.RightParenthesis))

			if p.lastToken() == scn.RightParenthesis {
				p.nextTokenDescription()
			} else {
				p.appendError(p.error(expectedRightParenthesis, p.lastTokenName()))
			}
		}

		p.rebase(unexpectedTokens, expected, set(scn.LeftParenthesis))
	}
}
