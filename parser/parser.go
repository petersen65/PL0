// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

// Package parser implements the PL/0 parser that performs a syntactical analysis of the concrete syntax.
package parser

import (
	emt "github.com/petersen65/PL0/emitter"
	scn "github.com/petersen65/PL0/scanner"
)

// Private implementation of the recursive descent parser.
type parser struct {
	concreteSyntaxIndex       int                  // index of the current token in the concrete syntax
	concreteSyntax            scn.ConcreteSyntax   // concrete syntax to parse
	emitter                   emt.Emitter          // emitter that emits the code
	declarationDepth 		  int32                // declaration depth of nested blocks
	memoryLocation            int32                // memory location of the current expression
	lastTokenDescription, eof scn.TokenDescription // description of the last token that was read
	symbolTable               *symbolTable         // symbol table that stores all symbols of the program
	errorReport               ErrorReport          // error report that stores all errors that occured during parsing
}

// Return the public interface of the private parser implementation.
func newParser() Parser {
	return &parser{}
}

// Run the recursive descent parser to map the concrete syntax to its corresponding emitted code.
func (p *parser) Parse(concreteSyntax scn.ConcreteSyntax, emitter emt.Emitter) (ErrorReport, error) {
	if err := p.reset(concreteSyntax, emitter); err != nil {
		return p.errorReport, err
	}

	// a program starts with a block of declaration depth 0
	p.symbolTable.addProcedure(emt.EntryPointName, 0, 0)
	p.block(emt.EntryPointName, set(declarations, statements, scn.Period))

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

// Reset the parser to its initial state so that it can be reused.
func (p *parser) reset(concreteSyntax scn.ConcreteSyntax, emitter emt.Emitter) error {
	p.concreteSyntaxIndex = 0
	p.concreteSyntax = concreteSyntax
	p.emitter = emitter
	p.declarationDepth = 0
	p.memoryLocation = 0
	p.symbolTable = newSymbolTable()
	p.errorReport = make(ErrorReport, 0)

	if len(p.concreteSyntax) == 0 || !p.nextTokenDescription() {
		return p.error(eofReached, nil)
	}

	return nil
}

// A block is a sequence of declarations followed by a statement.
func (p *parser) block(name string, expected scn.Tokens) {
	var entryPointInstruction emt.Address
	var varOffset uint64 = emt.VariableOffsetStart

	if depth > blockNestingMax {
		p.appendError(p.error(maxBlockDepth, depth))
	}

	// emit a jump to the first instruction of the entrypoint block whose address is not yet known
	if depth == 0 {
		entryPointInstruction = p.emitter.Jump(emt.NullAddress)
	}

	// declare all constants, variables and procedures of the block to fill up the symbol table
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

	// update the jump instruction address to the first instruction of the entrypoint block
	if depth == 0 {
		p.emitter.Update(entryPointInstruction, p.emitter.GetNextAddress())
	}

	// update the code address of the block's procedure symbol to the first instruction of the block
	procdureSymbol, _ := p.symbolTable.find(name)
	procdureSymbol.address = uint64(p.emitter.GetNextAddress())
	p.symbolTable.update(procdureSymbol)

	// allocating stack space for block variables is the first code instruction of the block
	p.emitter.AllocateStackSpace(emt.Offset(varOffset))

	// parse and emit all statement instructions which are defining the code logic of the block
	p.statement(depth, set(expected, scn.Semicolon, scn.EndWord))

	// emit a return instruction to return from the block
	p.emitter.Return()

	// at the end of the block, remove all symbols with its declaration depth from the symbol table
	// statements of a block are only allowed to reference symbols with a lower or equal declaration depth
	p.symbolTable.remove(depth)

	p.rebase(unexpectedTokens, expected, scn.Empty)
}

// Sequence of constants declarations.
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

// Sequence of variable declarations.
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

// Sequence of procedure declarations.
func (p *parser) procedureWord(depth int32, expected scn.Tokens) {
	for p.lastToken() == scn.ProcedureWord {
		p.nextTokenDescription()
		procedureName := p.procedureIdentifier(depth)

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

// An assignment is an identifier followed by becomes followed by an expression.
func (p *parser) assignment(depth int32, expected scn.Tokens) {
	symbol, ok := p.symbolTable.find(p.lastTokenValue().(string))

	if !ok {
		p.appendError(p.error(identifierNotFound, p.lastTokenValue().(string)))
	} else if symbol.kind != variable {
		p.appendError(p.error(expectedVariableIdentifier, kindNames[symbol.kind]))
	}

	p.nextTokenDescription()

	if p.lastToken() == scn.Becomes {
		p.nextTokenDescription()
	} else {
		p.appendError(p.error(expectedBecomes, p.lastTokenName()))
	}

	p.memoryLocation = int32(emt.M1)
	p.expression(depth, expected)

	if ok && symbol.kind == variable {
		p.emitter.Variable(emt.None, emt.Offset(symbol.offset), depth-symbol.depth, true)

	}
}

// A read statement is the read operator followed by an identifier that must be a variable.
func (p *parser) read(depth int32, expected scn.Tokens) {
	p.nextTokenDescription()

	if p.lastToken() != scn.Identifier {
		p.appendError(p.error(expectedIdentifier, p.lastTokenName()))
	} else {
		if symbol, ok := p.symbolTable.find(p.lastTokenValue().(string)); ok {
			if symbol.kind == variable {
				p.emitter.System(emt.Read)
				p.emitter.Variable(emt.None, emt.Offset(symbol.offset), depth-symbol.depth, true)
			} else {
				p.appendError(p.error(expectedVariableIdentifier, kindNames[symbol.kind]))
			}
		} else {
			p.appendError(p.error(identifierNotFound, p.lastTokenValue().(string)))
		}

	}

	p.nextTokenDescription()
}

// A write statement is the write operator followed by an expression.
func (p *parser) write(depth int32, expected scn.Tokens) {
	p.nextTokenDescription()
	p.memoryLocation = int32(emt.M1)
	p.expression(depth, expected)
	p.emitter.System(emt.Write)
}

// A begin-end statement is the begin word followed by a statements with semicolons followed by the end word.
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

// A call statement is the call word followed by a procedure identifier.
func (p *parser) callWord(depth int32, expected scn.Tokens) {
	p.nextTokenDescription()

	if p.lastToken() != scn.Identifier {
		p.appendError(p.error(expectedIdentifier, p.lastTokenName()))
	} else {
		if symbol, ok := p.symbolTable.find(p.lastTokenValue().(string)); ok {
			if symbol.kind == procedure {
				p.emitter.Call(emt.Address(symbol.address), depth-symbol.depth)
			} else {
				p.appendError(p.error(expectedProcedureIdentifier, kindNames[symbol.kind]))
			}
		} else {
			p.appendError(p.error(identifierNotFound, p.lastTokenValue().(string)))
		}

		p.nextTokenDescription()
	}
}

// An if statement is the if word followed by a condition followed by the then word followed by a statement.
func (p *parser) ifWord(depth int32, expected scn.Tokens) {
	p.nextTokenDescription()
	relationalOperator := p.condition(depth, set(expected, scn.ThenWord, scn.DoWord))

	if p.lastToken() == scn.ThenWord {
		p.nextTokenDescription()
	} else {
		p.appendError(p.error(expectedThen, p.lastTokenName()))
	}

	// jump over statement if the condition is false and remember the address of the jump instruction
	ifDecision := p.JumpConditional(relationalOperator, false)

	// parse and emit the statement which is executed if the condition is true
	p.statement(depth, expected)

	// update the conditional jump instruction address to the first instruction after the if statement
	p.emitter.Update(ifDecision, p.emitter.GetNextAddress())
}

// A while statement is the while word followed by a condition followed by the do word followed by a statement.
func (p *parser) whileWord(depth int32, expected scn.Tokens) {
	p.nextTokenDescription()
	whileCondition := p.emitter.GetNextAddress()
	relationalOperator := p.condition(depth, set(expected, scn.DoWord))

	// jump over statement if the condition is false and remember the address of the jump instruction
	whileDecision := p.JumpConditional(relationalOperator, false)

	if p.lastToken() == scn.DoWord {
		p.nextTokenDescription()
	} else {
		p.appendError(p.error(expectedDo, p.lastTokenName()))
	}

	// parse and emit the statement which is executed as long as the condition is true
	p.statement(depth, expected)

	// uncnoditional jump back to the condition evaluation
	p.emitter.Jump(whileCondition)

	// update the conditional jump instruction address to the first instruction after the while statement
	p.emitter.Update(whileDecision, p.emitter.GetNextAddress())
}

// A constant identifier is an identifier followed by an equal sign followed by a number to be stored in the symbol table.
func (p *parser) constantIdentifier(depth int32) {
	if p.lastToken() != scn.Identifier {
		p.appendError(p.error(expectedIdentifier, p.lastTokenName()))
		return
	}

	constantName := p.lastTokenValue().(string)
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

		if _, ok := p.symbolTable.find(constantName); ok {
			p.appendError(p.error(identifierAlreadyDeclared, constantName))
		} else {
			p.symbolTable.addConstant(constantName, depth, p.lastTokenValue())
		}

		p.nextTokenDescription()
	} else {
		p.appendError(p.error(expectedEqual, p.lastTokenName()))
	}
}

// A variable identifier is an identifier to be stored in the symbol table.
func (p *parser) variableIdentifier(depth int32, offset *uint64) {
	if p.lastToken() != scn.Identifier {
		p.appendError(p.error(expectedIdentifier, p.lastTokenName()))
	} else {
		if _, ok := p.symbolTable.find(p.lastTokenValue().(string)); ok {
			p.appendError(p.error(identifierAlreadyDeclared, p.lastTokenValue().(string)))
		} else {
			p.symbolTable.addVariable(p.lastTokenValue().(string), depth, offset)
		}

		p.nextTokenDescription()
	}
}

// A procedure identifier is an identifier to be stored in the symbol table.
func (p *parser) procedureIdentifier(depth int32) string {
	var procedureName string

	if p.lastToken() != scn.Identifier {
		p.appendError(p.error(expectedIdentifier, p.lastTokenName()))
	} else {
		if _, ok := p.symbolTable.find(p.lastTokenValue().(string)); ok {
			p.appendError(p.error(identifierAlreadyDeclared, p.lastTokenValue().(string)))
		} else {
			procedureName = p.lastTokenValue().(string)
			p.symbolTable.addProcedure(procedureName, depth, uint64(p.emitter.GetNextAddress()))
		}

		p.nextTokenDescription()
	}

	return procedureName
}

// A statement is either
//
//	an assignment statement,
//	a read statement,
//	a write statement,
//	a procedure call,
//	an if statement,
//	a while statement,
//	or a sequence of statements surrounded by begin and end.
func (p *parser) statement(depth int32, expected scn.Tokens) {

	switch p.lastToken() {
	case scn.Identifier:
		p.assignment(depth, expected)

	case scn.Read:
		p.read(depth, expected)

	case scn.Write:
		p.write(depth, expected)

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

// Emit a conditional jump instruction based on the relational operator of a condition.
func (p *parser) JumpConditional(relationalOperator scn.Token, condition bool) emt.Address {
	var address emt.Address

	if condition {
		// jump if the condition is true and remember the address of the jump instruction
		switch relationalOperator {
		case scn.Equal:
			address = p.emitter.JumpEqual(emt.NullAddress)

		case scn.NotEqual:
			address = p.emitter.JumpNotEqual(emt.NullAddress)

		case scn.Less:
			address = p.emitter.JumpLess(emt.NullAddress)

		case scn.LessEqual:
			address = p.emitter.JumpLessEqual(emt.NullAddress)

		case scn.Greater:
			address = p.emitter.JumpGreater(emt.NullAddress)

		case scn.GreaterEqual:
			address = p.emitter.JumpGreaterEqual(emt.NullAddress)
		}
	} else {
		// jump if the condition is false and remember the address of the jump instruction
		switch relationalOperator {
		case scn.Equal:
			address = p.emitter.JumpNotEqual(emt.NullAddress)

		case scn.NotEqual:
			address = p.emitter.JumpEqual(emt.NullAddress)

		case scn.Less:
			address = p.emitter.JumpGreaterEqual(emt.NullAddress)

		case scn.LessEqual:
			address = p.emitter.JumpGreater(emt.NullAddress)

		case scn.Greater:
			address = p.emitter.JumpLessEqual(emt.NullAddress)

		case scn.GreaterEqual:
			address = p.emitter.JumpLess(emt.NullAddress)
		}
	}

	return address
}

// A condition is either an odd expression or two expressions separated by a relational operator.
func (p *parser) condition(depth int32, expected scn.Tokens) scn.Token {
	var relationalOperator scn.Token

	if p.lastToken() == scn.OddWord {
		relationalOperator = p.lastToken()
		p.nextTokenDescription()
		p.expression(depth, expected)
		p.emitter.Odd(emt.MemoryLocation(p.memoryLocation))
	} else {
		p.expression(depth, set(expected, scn.Equal, scn.NotEqual, scn.Less, scn.LessEqual, scn.Greater, scn.GreaterEqual))

		if !p.lastToken().In(set(scn.Equal, scn.NotEqual, scn.Less, scn.LessEqual, scn.Greater, scn.GreaterEqual)) {
			p.appendError(p.error(expectedRelationalOperator, p.lastTokenName()))
		} else {
			relationalOperator = p.lastToken()
			p.nextTokenDescription()
			p.expression(depth, expected)

			switch relationalOperator {
			case scn.Equal:
				p.emitter.Equal()

			case scn.NotEqual:
				p.emitter.NotEqual()

			case scn.Less:
				p.emitter.Less()

			case scn.LessEqual:
				p.emitter.LessEqual()

			case scn.Greater:
				p.emitter.Greater()

			case scn.GreaterEqual:
				p.emitter.GreaterEqual()
			}
		}
	}

	return relationalOperator
}

// An expression is a sequence of terms separated by plus or minus.
func (p *parser) expression(depth int32, expected scn.Tokens) {
	// handle leading plus or minus sign of a term
	if p.lastToken() == scn.Plus || p.lastToken() == scn.Minus {
		plusOrMinus := p.lastToken()
		p.nextTokenDescription()

		// handle left term of a plus or minus operator
		p.term(depth, set(expected, scn.Plus, scn.Minus))

		if plusOrMinus == scn.Minus {
			p.emitter.Negate(emt.MemoryLocation(p.memoryLocation))
		}
	} else {
		// handle left term of a plus or minus operator
		p.term(depth, set(expected, scn.Plus, scn.Minus))
	}

	for p.lastToken() == scn.Plus || p.lastToken() == scn.Minus {
		plusOrMinus := p.lastToken()
		p.nextTokenDescription()

		// handle right term of a plus or minus operator
		p.memoryLocation++
		p.term(depth, set(expected, scn.Plus, scn.Minus))
		p.memoryLocation--

		if plusOrMinus == scn.Plus {
			p.emitter.Add(emt.MemoryLocation(p.memoryLocation))
		} else {
			p.emitter.Subtract(emt.MemoryLocation(p.memoryLocation))
		}
	}
}

// A term is a sequence of factors separated by times or divide.
func (p *parser) term(depth int32, expected scn.Tokens) {

	// handle left factor of a times or divide operator
	p.factor(depth, set(expected, scn.Times, scn.Divide))

	for p.lastToken() == scn.Times || p.lastToken() == scn.Divide {
		timesOrDevide := p.lastToken()
		p.nextTokenDescription()

		// handle right factor of a times or divide operator
		p.memoryLocation++
		p.factor(depth, set(expected, scn.Times, scn.Divide))
		p.memoryLocation--

		if timesOrDevide == scn.Times {
			p.emitter.Multiply(emt.MemoryLocation(p.memoryLocation))

		} else {
			p.emitter.Divide(emt.MemoryLocation(p.memoryLocation))
		}
	}
}

// A factor is either an identifier, a number, or an expression surrounded by parentheses.
func (p *parser) factor(depth int32, expected scn.Tokens) {
	p.rebase(expectedIdentifiersNumbersExpressions, factors, expected)

	for p.lastToken().In(factors) {
		if p.lastToken() == scn.Identifier {
			if symbol, ok := p.symbolTable.find(p.lastTokenValue().(string)); ok {
				switch symbol.kind {
				case constant:
					p.emitter.Constant(emt.MemoryLocation(p.memoryLocation), symbol.value)

				case variable:
					p.emitter.Variable(emt.MemoryLocation(p.memoryLocation), emt.Offset(symbol.offset), depth-symbol.depth, false)

				case procedure:
					p.appendError(p.error(expectedConstantsVariables, kindNames[symbol.kind]))
				}
			} else {
				p.appendError(p.error(identifierNotFound, p.lastTokenValue()))
			}

			p.nextTokenDescription()
		} else if p.lastToken() == scn.Number {
			p.emitter.Constant(emt.MemoryLocation(p.memoryLocation), p.lastTokenValue())
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
