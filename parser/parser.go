// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package parser

import (
	emt "github.com/petersen65/PL0/emitter"
	ast "github.com/petersen65/PL0/parser/ast"
	scn "github.com/petersen65/PL0/scanner"
)

// Private implementation of the recursive descent PL/0 parser.
type parser struct {
	emitter          emt.Emitter       // emitter that emits the code
	declarationDepth int32             // declaration depth of nested blocks
	tokenHandler     *tokenHandler     // token handler that manages the tokens of the token stream
	symbolTable      *symbolTable      // symbol table that stores all symbols of the program
	expressionParser *expressionParser // parse expressions that are part of statements
	abstractSyntax   ast.Block         // abstract syntax tree of the program
}

// Return the public interface of the private parser implementation.
func newParser() Parser {
	return &parser{}
}

// Run the recursive descent parser to map the token stream to its corresponding emitted code.
func (p *parser) Parse(tokenStream scn.TokenStream, emitter emt.Emitter) (ErrorReport, error) {
	if err := p.reset(tokenStream, emitter); err != nil {
		return p.tokenHandler.getErrorReport(), err
	}

	// a program starts with a block of declaration depth 0 and an entrypoint address 0
	p.symbolTable.addProcedure(emt.EntryPointName, 0, 0)

	// the main block starts with the
	//   declaration of constants, variables and procedures
	//   followed by a statement
	//   and ends with the program-end
	p.abstractSyntax = p.block(emt.EntryPointName, set(declarations, statements, scn.ProgramEnd))

	// the program must end with a specific token
	if p.lastToken() != scn.ProgramEnd {
		p.appendError(expectedPeriod, p.lastTokenName())
	}

	// the program must comply with the syntax rules of the programming language
	if !p.tokenHandler.isFullyParsed() {
		p.tokenHandler.setFullyParsed()
		p.appendError(notFullyParsed, nil)
	}

	// collect all errors from the parser and return the error report
	errorReport := p.tokenHandler.getErrorReport()

	if len(errorReport) == 1 {
		return errorReport, p.tokenHandler.error(parsingError, nil)
	} else if len(errorReport) > 1 {
		return errorReport, p.tokenHandler.error(parsingErrors, len(errorReport))
	} else {
		return errorReport, nil
	}
}

// Reset the parser to its initial state so that it can be reused.
func (p *parser) reset(tokenStream scn.TokenStream, emitter emt.Emitter) error {
	p.emitter = emitter
	p.declarationDepth = 0
	p.tokenHandler = newTokenHandler(tokenStream)
	p.symbolTable = newSymbolTable()
	p.expressionParser = newExpressionParser(p.tokenHandler, p.symbolTable, p.emitter)
	p.abstractSyntax = nil

	if len(tokenStream) == 0 || !p.nextToken() {
		return p.tokenHandler.error(eofReached, nil)
	}

	return nil
}

// A block is a sequence of declarations followed by a statement. The statement runs within its own stack frame.
func (p *parser) block(name string, expected scn.Tokens) ast.Block {
	var varOffset uint64 = emt.VariableOffsetStart

	// a block can contain a sequence of procedures, so the list of procedures is initialized
	procedures := make([]ast.Block, 0)

	if p.declarationDepth > blockNestingMax {
		p.appendError(maxBlockDepth, p.declarationDepth)
	}

	// emit a jump to the first instruction of the block whose address is not yet known
	// the address of the jump instruction itself was already stored in the symbol table as part of the block's procedure symbol
	//
	// for declaration depth
	// 	 0: jump is always used to start the program
	//   1 and above: jump is only used for forward calls to procedures with a lower declaration depth (block not emitted yet)
	firstInstruction := p.emitter.Jump(emt.NullAddress)

	// declare all constants, variables and procedures of the block to fill up the symbol table
	for {
		if p.lastToken() == scn.ConstWord {
			p.constWord()
		}

		if p.lastToken() == scn.VarWord {
			p.varWord(&varOffset)
		}

		if p.lastToken() == scn.ProcedureWord {
			procedures = p.procedureWord(expected)
		}

		// after declarations, the block expects
		//   a statement which also can be an assignment starting with an identifier
		//   or the parser would fall back to declarations as anchor in the case of a syntax error
		p.tokenHandler.rebase(expectedStatementsIdentifiers, set(statements, scn.Identifier), declarations)

		if !p.lastToken().In(declarations) {
			break
		}
	}

	// update the jump instruction address to the first instruction of the block
	p.emitter.Update(firstInstruction, p.emitter.GetNextAddress(), nil)

	// update the code address of the block's procedure symbol to the first instruction of the block
	procedureSymbol, _ := p.symbolTable.find(name)
	procedureSymbol.Address = uint64(p.emitter.GetNextAddress())
	p.symbolTable.update(procedureSymbol)

	// allocating stack space for block variables is the first code instruction of the block
	p.emitter.AllocateStackSpace(emt.Offset(varOffset))

	// parse and emit all statement instructions which are defining the code logic of the block
	//   or the parser forwards to all expected tokens as anchors in the case of a syntax error
	from := p.gatherSourceDescription()
	statement := p.statement(set(expected, scn.Semicolon, scn.EndWord))
	source := p.collectSourceDescription(from)

	// emit a return instruction to return from the block
	p.emitter.Return()

	// at the end of the block, remove all symbols with its declaration depth from the symbol table
	// statements of a block are only allowed to reference symbols with a lower or equal declaration depth
	declarations := p.symbolTable.remove(p.declarationDepth)

	// after the block ends
	//   a semicolon is expected to separate the block from the parent block
	//   or a program-end is expected to end the program
	//   or the parser would forward to all expected tokens as anchors in the case of a syntax error
	p.tokenHandler.rebase(unexpectedTokens, expected, scn.Empty)

	// return a new block node in the abstract syntax tree
	return ast.NewBlock(procedureSymbol, p.declarationDepth, declarations, procedures, statement, source)
}

// Sequence of constants declarations.
func (p *parser) constWord() {
	p.nextToken()

	for {
		p.constantIdentifier()

		for p.lastToken() == scn.Comma {
			p.nextToken()
			p.constantIdentifier()
		}

		if p.lastToken() == scn.Semicolon {
			p.nextToken()
		} else {
			p.appendError(expectedSemicolon, p.lastTokenName())
		}

		if p.lastToken() != scn.Identifier {
			break
		}
	}
}

// Sequence of variable declarations.
func (p *parser) varWord(offset *uint64) {
	p.nextToken()

	for {
		p.variableIdentifier(offset)

		for p.lastToken() == scn.Comma {
			p.nextToken()
			p.variableIdentifier(offset)
		}

		if p.lastToken() == scn.Semicolon {
			p.nextToken()
		} else {
			p.appendError(expectedSemicolon, p.lastTokenName())
		}

		if p.lastToken() != scn.Identifier {
			break
		}
	}
}

// Sequence of procedure declarations.
func (p *parser) procedureWord(anchors scn.Tokens) []ast.Block {
	procedures := make([]ast.Block, 0)

	for p.lastToken() == scn.ProcedureWord {
		p.nextToken()
		procedureName := p.procedureIdentifier()

		if p.lastToken() == scn.Semicolon {
			p.nextToken()
		} else {
			p.appendError(expectedSemicolon, p.lastTokenName())
		}

		// the procedure block gets anchor tokens from the parent block and starts with the
		//   declaration of constants, variables and procedures
		//   followed by a statement
		//   and ends with a semicolon
		p.declarationDepth++
		block := p.block(procedureName, set(anchors, scn.Semicolon))
		p.declarationDepth--

		// after the procedure block ends a semicolon is expected to separate
		//   the block from the parent block
		//   or from the next procedure declaration
		if p.lastToken() == scn.Semicolon {
			p.nextToken()

			// after the procedure block, the parser expects
			//   a statement which also can be an assignment starting with an identifier
			//   the beginning of a new procedure declaration
			//   or the parser would fall back to parent tokens as anchors in the case of a syntax error
			p.tokenHandler.rebase(expectedStatementsIdentifiersProcedures, set(statements, scn.Identifier, scn.ProcedureWord), anchors)
		} else {
			p.appendError(expectedSemicolon, p.lastTokenName())
		}

		// add the procedure block to the list of procedures for the parent block
		procedures = append(procedures, block)
	}

	return procedures
}

// An assignment is an identifier followed by becomes followed by an expression.
func (p *parser) assignment(anchors scn.Tokens) ast.Statement {
	symbol, ok := p.symbolTable.find(p.lastTokenValue())

	if !ok {
		p.appendError(identifierNotFound, p.lastTokenValue())
	} else if symbol.Kind != ast.Variable {
		p.appendError(expectedVariableIdentifier, kindNames[symbol.Kind])
	}

	from := p.gatherSourceDescription()
	p.nextToken()

	if p.lastToken() == scn.Becomes {
		p.nextToken()
	} else {
		p.appendError(expectedBecomes, p.lastTokenName())
	}

	right := p.expression(anchors)
	source := p.collectSourceDescription(from)

	if !ok || symbol.Kind != ast.Variable {
		return nil
	}

	p.emitter.StoreVariable(emt.Offset(symbol.Offset), p.declarationDepth-symbol.Depth)
	return ast.NewAssignmentStatement(symbol, right, source)
}

// A read statement is the read operator followed by an identifier that must be a variable.
func (p *parser) read() ast.Statement {
	var statement ast.Statement

	from := p.gatherSourceDescription()
	p.nextToken()

	if p.lastToken() != scn.Identifier {
		p.appendError(expectedIdentifier, p.lastTokenName())
	} else {
		if symbol, ok := p.symbolTable.find(p.lastTokenValue()); ok {
			if symbol.Kind == ast.Variable {
				p.emitter.System(emt.Read)
				p.emitter.StoreVariable(emt.Offset(symbol.Offset), p.declarationDepth-symbol.Depth)
				statement = ast.NewReadStatement(symbol, p.collectSourceDescription(from))
			} else {
				p.appendError(expectedVariableIdentifier, kindNames[symbol.Kind])
			}
		} else {
			p.appendError(identifierNotFound, p.lastTokenValue())
		}
	}

	p.nextToken()
	return statement
}

// A write statement is the write operator followed by an expression.
func (p *parser) write(anchors scn.Tokens) ast.Statement {
	from := p.gatherSourceDescription()
	p.nextToken()
	expression := p.expression(anchors)
	p.emitter.System(emt.Write)
	return ast.NewWriteStatement(expression, p.collectSourceDescription(from))
}

// A call statement is the call word followed by a procedure identifier.
func (p *parser) callWord() ast.Statement {
	var statement ast.Statement
	p.nextToken()

	if p.lastToken() != scn.Identifier {
		p.appendError(expectedIdentifier, p.lastTokenName())
	} else {
		if symbol, ok := p.symbolTable.find(p.lastTokenValue()); ok {
			if symbol.Kind == ast.Procedure {
				p.emitter.Call(emt.Address(symbol.Address), p.declarationDepth-symbol.Depth)
				statement = ast.NewCallStatement(symbol, p.lastTokenSource())
			} else {
				p.appendError(expectedProcedureIdentifier, kindNames[symbol.Kind])
			}
		} else {
			p.appendError(identifierNotFound, p.lastTokenValue())
		}

		p.nextToken()
	}

	return statement
}

// An if statement is the if word followed by a condition followed by the then word followed by a statement.
func (p *parser) ifWord(anchors scn.Tokens) ast.Statement {
	p.nextToken()
	relationalOperator, condition := p.condition(set(anchors, scn.ThenWord, scn.DoWord))

	if p.lastToken() == scn.ThenWord {
		p.nextToken()
	} else {
		p.appendError(expectedThen, p.lastTokenName())
	}

	// jump over statement if the condition is false and remember the address of the jump instruction
	ifDecision := p.jumpConditional(relationalOperator, false)

	// parse and emit the statement which is executed if the condition is true
	statement := p.statement(anchors)

	// update the conditional jump instruction address to the first instruction after the if statement
	p.emitter.Update(ifDecision, p.emitter.GetNextAddress(), nil)
	return ast.NewIfStatement(condition, statement, p.lastTokenSource())
}

// A while statement is the while word followed by a condition followed by the do word followed by a statement.
func (p *parser) whileWord(anchors scn.Tokens) ast.Statement {
	p.nextToken()
	whileCondition := p.emitter.GetNextAddress()
	relationalOperator, condition := p.condition(set(anchors, scn.DoWord))

	// jump over statement if the condition is false and remember the address of the jump instruction
	whileDecision := p.jumpConditional(relationalOperator, false)

	if p.lastToken() == scn.DoWord {
		p.nextToken()
	} else {
		p.appendError(expectedDo, p.lastTokenName())
	}

	// parse and emit the statement which is executed as long as the condition is true
	statement := p.statement(anchors)

	// uncnoditional jump back to the condition evaluation
	p.emitter.Jump(whileCondition)

	// update the conditional jump instruction address to the first instruction after the while statement
	p.emitter.Update(whileDecision, p.emitter.GetNextAddress(), nil)
	return ast.NewWhileStatement(condition, statement, p.lastTokenSource())
}

// A begin-end statement is the begin word followed by a statements with semicolons followed by the end word.
func (p *parser) beginWord(anchors scn.Tokens) ast.Statement {
	compound := make([]ast.Statement, 0)
	p.nextToken()

	// the first statement of a begin-end block
	if statement := p.statement(set(anchors, scn.EndWord, scn.Semicolon)); statement != nil {
		compound = append(compound, statement)
	}

	for p.lastToken().In(set(statements, scn.Semicolon)) {
		if p.lastToken() == scn.Semicolon {
			p.nextToken()
		} else {
			p.appendError(expectedSemicolon, p.lastTokenName())
		}

		// the next statement of a begin-end block
		if statement := p.statement(set(anchors, scn.EndWord, scn.Semicolon)); statement != nil {
			compound = append(compound, statement)
		}
	}

	if p.lastToken() == scn.EndWord {
		p.nextToken()
	} else {
		p.appendError(expectedEnd, p.lastTokenName())
	}

	return ast.NewCompoundStatement(compound, p.lastTokenSource())
}

// A constant identifier is an identifier followed by an equal sign followed by a number to be stored in the symbol table.
func (p *parser) constantIdentifier() {
	if p.lastToken() != scn.Identifier {
		p.appendError(expectedIdentifier, p.lastTokenName())
		return
	}

	constantName := p.lastTokenValue()

	if _, ok := p.symbolTable.find(constantName); ok {
		p.appendError(identifierAlreadyDeclared, constantName)
	}

	p.nextToken()

	if p.lastToken().In(set(scn.Equal, scn.Becomes)) {
		if p.lastToken() == scn.Becomes {
			p.appendError(expectedEqual, p.lastTokenName())
		}

		p.nextToken()
		var sign scn.Token

		if p.lastToken() == scn.Plus || p.lastToken() == scn.Minus {
			sign = p.lastToken()
			p.nextToken()
		}

		if p.lastToken() != scn.Number {
			p.appendError(expectedNumber, p.lastTokenName())
		} else {
			p.symbolTable.addConstant(constantName, p.declarationDepth, p.numberValue(sign, p.lastTokenValue()))
			p.nextToken()
		}
	} else {
		p.appendError(expectedEqual, p.lastTokenName())
	}
}

// A variable identifier is an identifier to be stored in the symbol table.
func (p *parser) variableIdentifier(offset *uint64) {
	if p.lastToken() != scn.Identifier {
		p.appendError(expectedIdentifier, p.lastTokenName())
	} else {
		if _, ok := p.symbolTable.find(p.lastTokenValue()); ok {
			p.appendError(identifierAlreadyDeclared, p.lastTokenValue())
		} else {
			p.symbolTable.addVariable(p.lastTokenValue(), p.declarationDepth, offset)
		}

		p.nextToken()
	}
}

// A procedure identifier is an identifier to be stored in the symbol table.
func (p *parser) procedureIdentifier() string {
	var procedureName string

	if p.lastToken() != scn.Identifier {
		p.appendError(expectedIdentifier, p.lastTokenName())
	} else {
		if _, ok := p.symbolTable.find(p.lastTokenValue()); ok {
			p.appendError(identifierAlreadyDeclared, p.lastTokenValue())
		} else {
			procedureName = p.lastTokenValue()
			p.symbolTable.addProcedure(procedureName, p.declarationDepth, uint64(p.emitter.GetNextAddress()))
		}

		p.nextToken()
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
func (p *parser) statement(anchors scn.Tokens) ast.Statement {
	var statement ast.Statement

	switch p.lastToken() {
	case scn.Identifier:
		statement = p.assignment(anchors)

	case scn.Read:
		statement = p.read()

	case scn.Write:
		statement = p.write(anchors)

	case scn.CallWord:
		statement = p.callWord()

	case scn.IfWord:
		statement = p.ifWord(anchors)

	case scn.WhileWord:
		statement = p.whileWord(anchors)

	case scn.BeginWord:
		statement = p.beginWord(anchors)
	}

	// after a statement, the parser expects
	//   a semicolon to separate the statement from the next statement
	//   or the end of the parent block
	//   or the parser would forward to all block-tokens as anchors in the case of a syntax error
	p.tokenHandler.rebase(expectedStatement, anchors, scn.Empty)
	return statement
}

// Emit a conditional jump instruction based on the relational operator of a condition.
func (p *parser) jumpConditional(relationalOperator scn.Token, condition bool) emt.Address {
	var address emt.Address

	if condition {
		// jump if the condition is true and remember the address of the jump instruction
		switch relationalOperator {
		case scn.OddWord:
			address = p.emitter.JumpNotEqual(emt.NullAddress)

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
		case scn.OddWord:
			address = p.emitter.JumpEqual(emt.NullAddress)

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

// Return the next token description from the token handler.
func (p *parser) nextToken() bool {
	return p.tokenHandler.nextTokenDescription()
}

// Wrapper to get token from the last token description.
func (p *parser) lastToken() scn.Token {
	return p.tokenHandler.lastToken()
}

// Wrapper to get the token name from the last token description.
func (p *parser) lastTokenName() string {
	return p.tokenHandler.lastTokenName()
}

// Wrapper to get the token value from the last token description.
func (p *parser) lastTokenValue() string {
	return p.tokenHandler.lastTokenValue()
}

// Wrapper to get the source description from the last token description.
func (p *parser) lastTokenSource() ast.SourceDescription {
	return ast.SourceDescription{}
}

// Wrapper to gather source description from the current token index.
func (p *parser) gatherSourceDescription() int {
	return p.tokenHandler.gatherSourceDescription()
}

// Wrapper to collect source description from the given index to the current token index.
func (p *parser) collectSourceDescription(from int) ast.SourceDescription {
	return p.tokenHandler.collectSourceDescription(from)
}

// Append parser error to the error report of the token handler.
func (p *parser) appendError(code failure, value any) {
	p.tokenHandler.appendError(p.tokenHandler.error(code, value))
}

// Start the expression parser and parse a condition.
func (p *parser) condition(anchors scn.Tokens) (scn.Token, ast.Expression) {
	return p.expressionParser.condition(p.declarationDepth, anchors)
}

// Start the expression parser and parse an expression.
func (p *parser) expression(anchors scn.Tokens) ast.Expression {
	return p.expressionParser.expression(p.declarationDepth, anchors)
}

// Wrapper to get the number value from the expression parser that automatically appends an error if the number is illegal.
func (p *parser) numberValue(sign scn.Token, number string) int64 {
	return p.expressionParser.numberValue(sign, number)
}
