// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package parser

import (
	"strconv"

	ast "github.com/petersen65/PL0/ast"
	scn "github.com/petersen65/PL0/scanner"
	tok "github.com/petersen65/PL0/token"
)

// Number of bits of a signed integer.
const integerBitSize = 64

// Private implementation of the recursive descent PL/0 parser.
type parser struct {
	declarationDepth int32            // declaration depth of nested blocks
	errorHandler     tok.ErrorHandler // error handler that is used to handle errors that occurred during parsing
	tokenHandler     tok.TokenHandler // token handler that manages the tokens of the token stream
	abstractSyntax   ast.Block        // abstract syntax tree of the program
}

var (
	// Tokens that are used to begin constants, variables, and procedures declarations.
	declarations = tok.Tokens{
		scn.ConstWord,
		scn.VarWord,
		scn.ProcedureWord,
	}

	// Tokens that are used to begin statements within a block.
	statements = tok.Tokens{
		scn.Read,
		scn.Write,
		scn.BeginWord,
		scn.CallWord,
		scn.IfWord,
		scn.WhileWord,
	}

	// Tokens that are used to begin factors in expressions.
	factors = tok.Tokens{
		scn.Identifier,
		scn.Number,
		scn.LeftParenthesis,
	}
)

// Return the public interface of the private parser implementation.
func newParser() Parser {
	return &parser{}
}

// Run the recursive descent parser to map the token stream to its corresponding abstract syntax tree.
func (p *parser) Parse(tokenStream tok.TokenStream, errorHandler tok.ErrorHandler) (ast.Block, tok.TokenHandler, error) {
	// an existing error handler can have errors from other compiler components
	startErrorCount := errorHandler.Count()

	p.declarationDepth = 0
	p.errorHandler = errorHandler
	p.tokenHandler = tok.NewTokenHandler(tokenStream, errorHandler, tok.Parser, failureMap)
	p.abstractSyntax = nil

	// the parser expects a token stream to be available
	if len(tokenStream) == 0 || !p.nextToken() {
		return nil, nil, tok.NewGeneralError(tok.Parser, failureMap, tok.Error, eofReached, nil)
	}

	// the main block starts with the
	//   declaration of constants, variables and procedures
	//   followed by a statement
	//   and ends with the program-end
	p.abstractSyntax = p.block(EntryPointName, nil, set(declarations, statements, scn.ProgramEnd))

	// the program must end with a specific token
	if p.lastToken() != scn.ProgramEnd {
		p.appendError(expectedPeriod, p.lastTokenName())
	}

	// the program must comply with the syntax rules of the programming language
	if !p.tokenHandler.IsFullyParsed() {
		p.tokenHandler.SetFullyParsed()
		p.appendError(notFullyParsed, nil)
	}

	// number of errors that occurred during parsing
	parserErrorCount := errorHandler.Count() - startErrorCount

	// return the abstract syntax tree of the program and the error handler
	if parserErrorCount == 1 {
		return nil, nil, tok.NewGeneralError(tok.Parser, failureMap, tok.Error, parsingError, nil)
	} else if parserErrorCount > 1 {
		return nil, nil, tok.NewGeneralError(tok.Parser, failureMap, tok.Error, parsingErrors, parserErrorCount)
	} else {
		return p.abstractSyntax, p.tokenHandler, nil
	}
}

// A block is a sequence of declarations followed by a statement. The statement runs within its own stack frame.
func (p *parser) block(name string, outer *ast.Scope, expected tok.Tokens) ast.Block {
	var scope = ast.NewScope(outer)

	// block of main program with outermost scope that has no further outer scope
	if outer == nil {
		// a program starts with a block of declaration depth 0 and an entrypoint address 0
		scope.Insert(&ast.Symbol{
			Name:    name,
			Kind:    ast.Procedure,
			Depth:   0,
			Address: 0, // entrypoint address is set to 0 and will be updated by the code generator
		})
	}

	// a block can contain a sequence of declarations, so lists for all declarations are initialized
	constants := make([]ast.Declaration, 0)
	variables := make([]ast.Declaration, 0)
	procedures := make([]ast.Declaration, 0)
	all := make([]ast.Declaration, 0)

	// it is essential that the block procedure declaration is the first entry in the list of its nested procedures
	procedures = append(procedures, ast.NewProcedureDeclaration(name, ast.NewEmptyBlock(), scope, p.lastTokenIndex()))

	// the parser does not support more than 'blockNestingMax' nested blocks
	if p.declarationDepth > blockNestingMax {
		p.appendError(maxBlockDepth, p.declarationDepth)
	}

	// declare all constants, variables and procedures of the block
	for {
		if p.lastToken() == scn.ConstWord {
			constants = append(constants, p.constWord(scope)...)
		}

		if p.lastToken() == scn.VarWord {
			variables = append(variables, p.varWord(scope)...)
		}

		if p.lastToken() == scn.ProcedureWord {
			procedures = append(procedures, p.procedureWord(scope, expected)...)
		}

		// after declarations, the block expects
		//   a statement which also can be an assignment starting with an identifier
		//   or the parser would fall back to declarations as anchor in the case of a syntax error
		p.tokenHandler.Rebase(expectedStatementsIdentifiers, set(statements, scn.Identifier), declarations)

		if !p.lastToken().In(declarations) {
			break
		}
	}

	// parse all statement instructions which are defining the code logic of the block
	//   or the parser forwards to all expected tokens as anchors in the case of a syntax error
	statement, err := p.statement(scope, set(expected, scn.Semicolon, scn.EndWord))

	// replace nil statement with an empty statement (nil means "no statement" like a single semicolon)
	if !err && statement == nil {
		statement = ast.NewEmptyStatement()
	}

	// after the block ends
	//   a semicolon is expected to separate the block from the parent block
	//   or a program-end is expected to end the program
	//   or the parser would forward to all expected tokens as anchors in the case of a syntax error
	p.tokenHandler.Rebase(unexpectedTokens, expected, scn.Empty)

	// return a new block node in the abstract syntax tree
	all = append(append(append(all, constants...), variables...), procedures...)
	block := ast.NewBlock(name, p.declarationDepth, scope, all, statement)
	
	// set the block of the procedure declaration to the block node
	procedures[0].(*ast.ProcedureDeclarationNode).Block = block
	return block
}

// Sequence of constants declarations.
func (p *parser) constWord(scope *ast.Scope) []ast.Declaration {
	declarations := make([]ast.Declaration, 0)
	p.nextToken()

	for {
		declarations = append(declarations, p.constantIdentifier(scope))

		for p.lastToken() == scn.Comma {
			p.nextToken()
			declarations = append(declarations, p.constantIdentifier(scope))
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

	return declarations
}

// Sequence of variable declarations.
func (p *parser) varWord(scope *ast.Scope) []ast.Declaration {
	declarations := make([]ast.Declaration, 0)
	p.nextToken()

	for {
		declarations = append(declarations, p.variableIdentifier(scope))

		for p.lastToken() == scn.Comma {
			p.nextToken()
			declarations = append(declarations, p.variableIdentifier(scope))
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

	return declarations
}

// Sequence of procedure declarations.
func (p *parser) procedureWord(outer *ast.Scope, anchors tok.Tokens) []ast.Declaration {
	declarations := make([]ast.Declaration, 0)

	for p.lastToken() == scn.ProcedureWord {
		p.nextToken()
		procedureName, declaration := p.procedureIdentifier(outer)

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
		block := p.block(procedureName, outer, set(anchors, scn.Semicolon))
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
			p.tokenHandler.Rebase(expectedStatementsIdentifiersProcedures, set(statements, scn.Identifier, scn.ProcedureWord), anchors)
		} else {
			p.appendError(expectedSemicolon, p.lastTokenName())
		}

		// add the procedure declaration to the list of declarations for the parent block node
		declaration.(*ast.ProcedureDeclarationNode).Block = block
		declarations = append(declarations, declaration)
	}

	return declarations
}

// An assignment is an identifier followed by becomes followed by an expression.
func (p *parser) assignment(scope *ast.Scope, anchors tok.Tokens) ast.Statement {
	symbol := scope.Lookup(p.lastTokenValue())

	if symbol == nil {
		p.appendError(identifierNotFound, p.lastTokenValue())
	} else if symbol.Kind != ast.Variable {
		p.appendError(expectedVariableIdentifier, ast.KindNames[symbol.Kind])
	}

	p.nextToken()

	if p.lastToken() == scn.Becomes {
		p.nextToken()
	} else {
		p.appendError(expectedBecomes, p.lastTokenName())

		if p.lastToken() == scn.Equal {
			p.nextToken()
		}
	}

	right := p.expression(scope, anchors)

	if symbol == nil || symbol.Kind != ast.Variable {
		// in case of a parsing error, return an empty statement
		return ast.NewEmptyStatement()
	}

	left := ast.NewVariableReference(symbol.Declaration)
	return ast.NewAssignmentStatement(left, right)
}

// A read statement is the read operator followed by an identifier that must be a variable.
func (p *parser) read(scope *ast.Scope) ast.Statement {
	var symbol *ast.Symbol
	p.nextToken()

	if p.lastToken() != scn.Identifier {
		p.appendError(expectedIdentifier, p.lastTokenName())
	} else {
		if entry := scope.Lookup(p.lastTokenValue()); entry != nil {
			if entry.Kind == ast.Variable {
				symbol = entry
			} else {
				p.appendError(expectedVariableIdentifier, ast.KindNames[entry.Kind])
			}
		} else {
			p.appendError(identifierNotFound, p.lastTokenValue())
		}
	}

	p.nextToken()

	if symbol != nil {
		return ast.NewReadStatement(ast.NewVariableReference(symbol.Declaration))
	}

	// in case of a parsing error, return an empty statement
	return ast.NewEmptyStatement()
}

// A write statement is the write operator followed by an expression.
func (p *parser) write(scope *ast.Scope, anchors tok.Tokens) ast.Statement {
	p.nextToken()
	expression := p.expression(scope, anchors)
	return ast.NewWriteStatement(expression)
}

// A call statement is the call word followed by a procedure identifier.
func (p *parser) callWord(scope *ast.Scope) ast.Statement {
	var symbol *ast.Symbol
	p.nextToken()

	if p.lastToken() != scn.Identifier {
		p.appendError(expectedIdentifier, p.lastTokenName())
	} else {
		if entry := scope.Lookup(p.lastTokenValue()); entry != nil {
			if entry.Kind == ast.Procedure {
				symbol = entry
			} else {
				p.appendError(expectedProcedureIdentifier, ast.KindNames[entry.Kind])
			}
		} else {
			p.appendError(identifierNotFound, p.lastTokenValue())
		}

		p.nextToken()
	}

	if symbol != nil {
		return ast.NewCallStatement(ast.NewProcedureReference(symbol.Declaration))
	}

	// in case of a parsing error, return an empty statement
	return ast.NewEmptyStatement()
}

// An if statement is the if word followed by a condition followed by the then word followed by a statement.
func (p *parser) ifWord(scope *ast.Scope, anchors tok.Tokens) ast.Statement {
	p.nextToken()
	condition := p.condition(scope, set(anchors, scn.ThenWord, scn.DoWord))

	if p.lastToken() == scn.ThenWord {
		p.nextToken()
	} else {
		p.appendError(expectedThen, p.lastTokenName())
	}

	// parse the statement which is executed if the condition is true
	statement, err := p.statement(scope, anchors)

	// replace nil statement with an empty statement (nil means "no statement" like a single semicolon)
	if !err && statement == nil {
		statement = ast.NewEmptyStatement()
	}

	return ast.NewIfStatement(condition, statement)
}

// A while statement is the while word followed by a condition followed by the do word followed by a statement.
func (p *parser) whileWord(scope *ast.Scope, anchors tok.Tokens) ast.Statement {
	p.nextToken()
	condition := p.condition(scope, set(anchors, scn.DoWord))

	if p.lastToken() == scn.DoWord {
		p.nextToken()
	} else {
		p.appendError(expectedDo, p.lastTokenName())
	}

	// parse the statement which is executed as long as the condition is true
	statement, err := p.statement(scope, anchors)

	// replace nil statement with an empty statement (nil means "no statement" like a single semicolon)
	if !err && statement == nil {
		statement = ast.NewEmptyStatement()
	}

	return ast.NewWhileStatement(condition, statement)
}

// A begin-end compound statement is the begin word followed by a statements with semicolons followed by the end word.
func (p *parser) beginWord(scope *ast.Scope, anchors tok.Tokens) ast.Statement {
	compound := make([]ast.Statement, 0)
	p.nextToken()

	// the first statement of a begin-end compound (only if the compound is not empty and there is no parsing error)
	if statement, err := p.statement(scope, set(anchors, scn.EndWord, scn.Semicolon)); !err && statement != nil {
		compound = append(compound, statement)
	}

	for p.lastToken().In(set(statements, scn.Semicolon)) {
		if p.lastToken() == scn.Semicolon {
			p.nextToken()
		} else {
			p.appendError(expectedSemicolon, p.lastTokenName())
		}

		// the next statement of a begin-end compound (only if a statement is available and there is no parsing error)
		if statement, err := p.statement(scope, set(anchors, scn.EndWord, scn.Semicolon)); !err && statement != nil {
			compound = append(compound, statement)
		}
	}

	if p.lastToken() == scn.EndWord {
		p.nextToken()
	} else {
		p.appendError(expectedEnd, p.lastTokenName())
	}

	return ast.NewCompoundStatement(compound)
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
func (p *parser) statement(scope *ast.Scope, anchors tok.Tokens) (ast.Statement, bool) {
	var statement ast.Statement

	switch p.lastToken() {
	case scn.Identifier:
		statement = p.assignment(scope, anchors)

	case scn.Read:
		statement = p.read(scope)

	case scn.Write:
		statement = p.write(scope, anchors)

	case scn.CallWord:
		statement = p.callWord(scope)

	case scn.IfWord:
		statement = p.ifWord(scope, anchors)

	case scn.WhileWord:
		statement = p.whileWord(scope, anchors)

	case scn.BeginWord:
		statement = p.beginWord(scope, anchors)

	default:
		// intentionally do nothing because the current token is not a statement anymore (not necessarily an error)
		// this is also the case when an empty compound statement is parsed or there is no statement anymore in a compound because the last one was already parsed
	}

	// after a statement, the parser expects
	//   a semicolon to separate the statement from the next statement
	//   or the end of the program
	//   or the end of the parent block
	//   or the parser would forward to all block-tokens as anchors in the case of a syntax error
	if p.tokenHandler.Rebase(unexpectedTokensAfterStatement, anchors, scn.Empty) {
		// in case of a parsing error, return an empty statement
		if statement == nil {
			statement = ast.NewEmptyStatement()
		}

		return statement, true
	}

	return statement, false
}

// A constant identifier is an identifier followed by an equal sign followed by a number to be stored in the symbol table.
func (p *parser) constantIdentifier(scope *ast.Scope) ast.Declaration {
	// in case of a parsing error, return an empty declaration
	declaration := ast.NewEmptyDeclaration()

	if p.lastToken() != scn.Identifier {
		p.appendError(expectedIdentifier, p.lastTokenName())
		return declaration
	}

	constantName := p.lastTokenValue()
	constantNameIndex := p.lastTokenIndex()

	if scope.Lookup(constantName) != nil {
		p.appendError(identifierAlreadyDeclared, constantName)
	}

	p.nextToken()

	if p.lastToken().In(set(scn.Equal, scn.Becomes)) {
		if p.lastToken() == scn.Becomes {
			p.appendError(expectedEqual, p.lastTokenName())
		}

		p.nextToken()
		var sign tok.Token

		if p.lastToken() == scn.Plus || p.lastToken() == scn.Minus {
			sign = p.lastToken()
			p.nextToken()
		}

		if p.lastToken() != scn.Number {
			p.appendError(expectedNumber, p.lastTokenName())
		} else {
			declaration = ast.NewConstantDeclaration(
				constantName,
				p.numberValue(sign, p.lastTokenValue()),
				ast.Integer64,
				scope,
				constantNameIndex)

			scope.Insert(&ast.Symbol{
				Name:        constantName,
				Kind:        ast.Constant,
				Declaration: declaration,
				Depth:       p.declarationDepth,
				Value:       p.numberValue(sign, p.lastTokenValue()),
			})

			p.nextToken()
		}
	} else {
		p.appendError(expectedEqual, p.lastTokenName())
	}

	return declaration
}

// A variable identifier is an identifier to be stored in the symbol table.
func (p *parser) variableIdentifier(scope *ast.Scope) ast.Declaration {
	// in case of a parsing error, return an empty declaration
	declaration := ast.NewEmptyDeclaration()

	if p.lastToken() != scn.Identifier {
		p.appendError(expectedIdentifier, p.lastTokenName())
	} else {
		if scope.Lookup(p.lastTokenValue()) != nil {
			p.appendError(identifierAlreadyDeclared, p.lastTokenValue())
		} else {
			declaration = ast.NewVariableDeclaration(p.lastTokenValue(), ast.Integer64, scope, p.lastTokenIndex())

			scope.Insert(&ast.Symbol{
				Name:        p.lastTokenValue(),
				Kind:        ast.Variable,
				Declaration: declaration,
				Depth:       p.declarationDepth,
				Offset:      0, // variable offset is set to 0 and will be updated by the code generator
			})
		}

		p.nextToken()
	}

	return declaration
}

// A procedure identifier is an identifier to be stored in the symbol table.
func (p *parser) procedureIdentifier(scope *ast.Scope) (string, ast.Declaration) {
	var procedureName string

	// in case of a parsing error, return an empty declaration
	declaration := ast.NewEmptyDeclaration()

	if p.lastToken() != scn.Identifier {
		p.appendError(expectedIdentifier, p.lastTokenName())
	} else {
		if scope.Lookup(p.lastTokenValue()) != nil {
			p.appendError(identifierAlreadyDeclared, p.lastTokenValue())
		} else {
			procedureName = p.lastTokenValue()

			// the procedure block is not yet known and will be set after the block is parsed
			declaration = ast.NewProcedureDeclaration(procedureName, ast.NewEmptyBlock(), scope, p.lastTokenIndex())

			scope.Insert(&ast.Symbol{
				Name:        procedureName,
				Kind:        ast.Procedure,
				Declaration: declaration,
				Depth:       p.declarationDepth,
				Address:     0, // procedure address is set to 0 and will be updated by the code generator
			})
		}

		p.nextToken()
	}

	return procedureName, declaration
}

// A condition is either an odd expression or two expressions separated by a relational operator.
func (p *parser) condition(scope *ast.Scope, anchors tok.Tokens) ast.Expression {
	// in case of a parsing error, return an empty declaration
	operation := ast.NewEmptyExpression()

	if p.lastToken() == scn.OddWord {
		p.nextToken()
		operand := p.expression(scope, anchors)
		operation = ast.NewUnaryOperation(ast.Odd, operand)
	} else {
		// handle left expression of a relational operator
		left := p.expression(scope, set(anchors, scn.Equal, scn.NotEqual, scn.Less, scn.LessEqual, scn.Greater, scn.GreaterEqual))

		if !p.lastToken().In(set(scn.Equal, scn.NotEqual, scn.Less, scn.LessEqual, scn.Greater, scn.GreaterEqual)) {
			p.appendError(expectedRelationalOperator, p.lastTokenName())
		} else {
			relationalOperator := p.lastToken()
			p.nextToken()

			// handle right expression of a relational operator
			right := p.expression(scope, anchors)

			switch relationalOperator {
			case scn.Equal:
				operation = ast.NewConditionalOperation(ast.Equal, left, right)

			case scn.NotEqual:
				operation = ast.NewConditionalOperation(ast.NotEqual, left, right)

			case scn.Less:
				operation = ast.NewConditionalOperation(ast.Less, left, right)

			case scn.LessEqual:
				operation = ast.NewConditionalOperation(ast.LessEqual, left, right)

			case scn.Greater:
				operation = ast.NewConditionalOperation(ast.Greater, left, right)

			case scn.GreaterEqual:
				operation = ast.NewConditionalOperation(ast.GreaterEqual, left, right)

			default:
				p.appendError(expectedRelationalOperator, p.lastTokenName())
				operation = ast.NewEmptyExpression()
			}
		}
	}

	return operation
}

// An expression is a sequence of terms separated by plus or minus.
func (p *parser) expression(scope *ast.Scope, anchors tok.Tokens) ast.Expression {
	var operation ast.Expression

	// handle left term of a plus or minus operator
	left := p.term(scope, set(anchors, scn.Plus, scn.Minus))

	for p.lastToken() == scn.Plus || p.lastToken() == scn.Minus {
		plusOrMinus := p.lastToken()
		p.nextToken()

		// handle right term of a plus or minus operator
		right := p.term(scope, set(anchors, scn.Plus, scn.Minus))

		if plusOrMinus == scn.Plus {
			operation = ast.NewBinaryOperation(ast.Plus, left, right)
		} else {
			operation = ast.NewBinaryOperation(ast.Minus, left, right)
		}

		left = operation
	}

	// expression is the left term if no plus or minus operator is present
	if operation == nil {
		operation = left
	}

	return operation
}

// A term is a sequence of factors separated by times or divide.
func (p *parser) term(scope *ast.Scope, anchors tok.Tokens) ast.Expression {
	var operation ast.Expression

	// handle left factor of a times or divide operator
	left := p.factor(scope, set(anchors, scn.Times, scn.Divide))

	for p.lastToken() == scn.Times || p.lastToken() == scn.Divide {
		timesOrDevide := p.lastToken()
		p.nextToken()

		// handle right factor of a times or divide operator
		right := p.factor(scope, set(anchors, scn.Times, scn.Divide))

		if timesOrDevide == scn.Times {
			operation = ast.NewBinaryOperation(ast.Times, left, right)

		} else {
			operation = ast.NewBinaryOperation(ast.Divide, left, right)
		}

		left = operation
	}

	// term is the left factor if no times or divide operator is present
	if operation == nil {
		operation = left
	}

	return operation
}

// A factor is either an identifier, a number, or an expression surrounded by parentheses.
func (p *parser) factor(scope *ast.Scope, anchors tok.Tokens) ast.Expression {
	var sign tok.Token

	// in case of a parsing error, return an empty declaration
	operand := ast.NewEmptyExpression()

	// handle leading plus or minus sign of a factor
	if p.lastToken() == scn.Plus || p.lastToken() == scn.Minus {
		sign = p.lastToken()
		p.nextToken()
	}

	// at the beginning of a factor
	//   the expected tokens are identifiers, numbers, and left parentheses
	//   or the parser would fall back to all block-tokens as anchors in the case of a syntax error
	p.tokenHandler.Rebase(expectedIdentifiersNumbersExpressions, factors, anchors)

	for p.lastToken().In(factors) {
		if p.lastToken() == scn.Identifier {
			if symbol := scope.Lookup(p.lastTokenValue()); symbol != nil {
				switch symbol.Kind {
				case ast.Constant:
					operand = ast.NewConstantReference(symbol.Declaration)

				case ast.Variable:
					operand = ast.NewVariableReference(symbol.Declaration)

				default:
					p.appendError(expectedConstantsVariables, ast.KindNames[symbol.Kind])
				}
			} else {
				p.appendError(identifierNotFound, p.lastTokenValue())
			}

			p.nextToken()
		} else if p.lastToken() == scn.Number {
			operand = ast.NewLiteral(p.numberValue(sign, p.lastTokenValue()), ast.Integer64)
			sign = scn.Unknown
			p.nextToken()
		} else if p.lastToken() == scn.LeftParenthesis {
			p.nextToken()
			operand = p.expression(scope, set(anchors, scn.RightParenthesis))

			if p.lastToken() == scn.RightParenthesis {
				p.nextToken()
			} else {
				p.appendError(expectedRightParenthesis, p.lastTokenName())
			}
		}

		// after a factor, the parser expects
		//   a times or divide operator
		//   a plus or minus operator
		//   a relational operator
		//   a right parenthesis
		//   or the parser would fall back to all block-tokens as anchors in the case of a syntax error
		p.tokenHandler.Rebase(unexpectedTokens, anchors, set(scn.LeftParenthesis))
	}

	// negate the factor if a leading minus sign is present
	if sign == scn.Minus {
		operand = ast.NewUnaryOperation(ast.Negate, operand)
	}

	return operand
}

// Return the next token description from the token handler.
func (p *parser) nextToken() bool {
	return p.tokenHandler.NextTokenDescription()
}

// Wrapper to get token from the last token description.
func (p *parser) lastToken() tok.Token {
	return p.tokenHandler.LastToken()
}

// Wrapper to get the token name from the last token description.
func (p *parser) lastTokenName() string {
	return p.tokenHandler.LastTokenName()
}

// Wrapper to get the token value from the last token description.
func (p *parser) lastTokenValue() string {
	return p.tokenHandler.LastTokenValue()
}

// Wrapper to get the index of the last token in the token stream from the token handler.
func (p *parser) lastTokenIndex() int {
	return p.tokenHandler.LastTokenIndex()
}

// Append parser error to the error handler.
func (p *parser) appendError(code tok.Failure, value any) {
	p.tokenHandler.AppendError(p.tokenHandler.NewError(code, value))
}

// Wrapper to get joined slice of all tokens within the given TokenSet interfaces.
func set(tss ...tok.TokenSet) tok.Tokens {
	return tok.Set(tss...)
}

// Analyze a number and convert it to an Integer64 value (-9223372036854775808 to 9223372036854775807).
func (e *parser) numberValue(sign tok.Token, number string) int64 {
	if sign == scn.Minus {
		number = "-" + number
	}

	value, err := strconv.ParseInt(number, 10, integerBitSize)

	if err != nil {
		e.appendError(illegalInteger, number)
	}

	return value
}
