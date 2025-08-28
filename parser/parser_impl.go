// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package parser

import (
	"strconv"

	ast "github.com/petersen65/pl0/v3/ast"
	cor "github.com/petersen65/pl0/v3/core"
)

// Number of bits of a signed integer.
const integerBitSize = 64

// Implementation of the recursive descent parser.
type parser struct {
	uniqueScopeId  int              // the parser is required to provide a unique number for each scope it creates
	errorHandler   cor.ErrorHandler // error handler that is used to handle errors that occurred during parsing
	tokenHandler   cor.TokenHandler // token handler that manages the tokens of the token stream
	abstractSyntax ast.Block        // abstract syntax tree of the program
}

var (
	// Tokens that are used to begin constants, variables, and procedures declarations.
	declarations = cor.Tokens{
		cor.ConstWord,
		cor.VarWord,
		cor.ProcedureWord,
	}

	// Tokens that are used to begin statements within a block.
	statements = cor.Tokens{
		cor.Read,
		cor.Write,
		cor.BeginWord,
		cor.CallWord,
		cor.IfWord,
		cor.WhileWord,
	}

	// Tokens that are used to begin factors in expressions.
	factors = cor.Tokens{
		cor.Identifier,
		cor.Number,
		cor.LeftParenthesis,
	}
)

// Return the interface of the parser implementation.
func newParser(tokenStream cor.TokenStream, errorHandler cor.ErrorHandler) Parser {
	return &parser{
		errorHandler: errorHandler,
		tokenHandler: cor.NewTokenHandler(tokenStream, errorHandler, cor.Parser, failureMap),
	}
}

// Run the recursive descent parser to map the token stream to its corresponding abstract syntax tree.
func (p *parser) Parse() (ast.Block, cor.TokenHandler) {
	// check if the parser is in a valid state to start parsing
	if p.errorHandler == nil || p.tokenHandler == nil || p.abstractSyntax != nil {
		panic(cor.NewGeneralError(cor.Parser, failureMap, cor.Fatal, invalidParserState, nil, nil))
	}

	// the parser expects a token stream to be available
	if !p.nextToken() {
		p.errorHandler.AppendError(cor.NewGeneralError(cor.Parser, failureMap, cor.Error, eofReached, nil, nil))
		return nil, p.tokenHandler
	}

	// the main block starts with the
	//   declaration of constants, variables and procedures
	//   followed by a statement
	//   and ends with the program-end
	p.abstractSyntax = p.block(0, nil, set(declarations, statements, cor.ProgramEnd))

	// the program must end with a specific token
	if p.lastToken() != cor.ProgramEnd {
		p.appendError(expectedPeriod, p.lastTokenName())
	}

	// the program must comply with the syntax rules of the programming language
	if !p.tokenHandler.IsFullyParsed() {
		p.tokenHandler.SetFullyParsed()
		p.appendError(notFullyParsed, nil)
	}

	// return the abstract syntax tree of the program and the token handler
	return p.abstractSyntax, p.tokenHandler
}

// A block is a sequence of declarations followed by a statement.
func (p *parser) block(blockNestingDepth int32, outer ast.Scope[ast.Declaration], expected cor.Tokens) ast.Block {
	// generate a scope number that must be unique accross compilation
	p.uniqueScopeId++

	// a block has its own scope to manage its symbols
	var scope = ast.NewScope(p.uniqueScopeId, outer)

	// a block can contain a sequence of declarations, so lists for all declarations are initialized
	constants := make([]ast.Declaration, 0)
	variables := make([]ast.Declaration, 0)
	procedures := make([]ast.Declaration, 0)
	all := make([]ast.Declaration, 0)

	// the parser does not support more than 'blockNestingMax' nested blocks
	if blockNestingDepth > blockNestingMax {
		p.appendError(maxBlockDepth, blockNestingDepth)
	}

	// declare all constants, variables and procedures of the block
	for {
		if p.lastToken() == cor.ConstWord {
			constants = append(constants, p.constWord(scope)...)
		}

		if p.lastToken() == cor.VarWord {
			variables = append(variables, p.varWord(scope)...)
		}

		if p.lastToken() == cor.ProcedureWord {
			procedures = append(procedures, p.procedureWord(blockNestingDepth, scope, expected)...)
		}

		// after declarations, the block expects
		//   a statement which also can be an assignment starting with an identifier
		//   or the parser would fall back to declarations as anchor in the case of a syntax error
		p.tokenHandler.Recover(expectedStatementsIdentifiers, set(statements, cor.Identifier), declarations)

		if !p.lastToken().In(declarations) {
			break
		}
	}

	// parse all statement instructions which are defining the code logic of the block
	//   or the parser forwards to all expected tokens as anchors in the case of a syntax error
	statement, err := p.statement(scope, set(expected, cor.Semicolon, cor.EndWord))

	// replace nil statement with an empty statement (nil means "no statement" like a single semicolon)
	if !err && statement == nil {
		statement = ast.NewEmptyStatement()
	}

	// after the block ends
	//   a semicolon is expected to separate the block from the parent block
	//   or a program-end is expected to end the program
	//   or the parser would forward to all expected tokens as anchors in the case of a syntax error
	p.tokenHandler.Recover(unexpectedTokens, expected, cor.Tokens{})

	// return a new block node in the abstract syntax tree
	all = append(append(append(all, constants...), variables...), procedures...)
	return ast.NewBlock(blockNestingDepth, scope, all, statement)
}

// Sequence of constants declarations.
func (p *parser) constWord(scope ast.Scope[ast.Declaration]) []ast.Declaration {
	declarations := make([]ast.Declaration, 0)
	p.nextToken()

	// all constants are declared in a sequence of identifier equal number
	for {
		// first constant declaration
		declarations = append(declarations, p.constantIdentifier(scope))

		// a comma separates constant declarations which are grouped by a semicolon
		for p.lastToken() == cor.Comma {
			p.nextToken()

			// next constant declaration
			declarations = append(declarations, p.constantIdentifier(scope))
		}

		// a semicolon separates constant declaration groups from each other
		if p.lastToken() == cor.Semicolon {
			p.nextToken()
		} else {
			p.appendError(expectedSemicolon, p.lastTokenName())
		}

		// check for more constant declaration groups
		if p.lastToken() != cor.Identifier {
			break
		}
	}

	return declarations
}

// Sequence of variable declarations.
func (p *parser) varWord(scope ast.Scope[ast.Declaration]) []ast.Declaration {
	declarations := make([]ast.Declaration, 0)
	p.nextToken()

	// all variables are declared in a sequence of identifiers
	for {
		// first variable declaration
		declarations = append(declarations, p.variableIdentifier(scope))

		// a comma separates variable declarations which are grouped by a semicolon
		for p.lastToken() == cor.Comma {
			p.nextToken()
			declarations = append(declarations, p.variableIdentifier(scope))
		}

		// a semicolon separates variable declaration groups from each other
		if p.lastToken() == cor.Semicolon {
			p.nextToken()
		} else {
			p.appendError(expectedSemicolon, p.lastTokenName())
		}

		// check for more variable declaration groups
		if p.lastToken() != cor.Identifier {
			break
		}
	}

	return declarations
}

// Sequence of procedure declarations.
func (p *parser) procedureWord(blockNestingDepth int32, scope ast.Scope[ast.Declaration], anchors cor.Tokens) []ast.Declaration {
	declarations := make([]ast.Declaration, 0)

	// all procedures are declared in a sequence of procedure identifiers with each having a block
	for p.lastToken() == cor.ProcedureWord {
		p.nextToken()

		// procedure declaration
		declaration := p.procedureIdentifier(scope)

		// after the procedure identifier, a semicolon is expected to separate the identifier from the block
		if p.lastToken() == cor.Semicolon {
			p.nextToken()
		} else {
			p.appendError(expectedSemicolon, p.lastTokenName())
		}

		// the procedure block gets anchor tokens from the parent block and starts with the
		//   declaration of constants, variables and procedures
		//   followed by a statement
		//   and ends with a semicolon
		block := p.block(blockNestingDepth+1, scope, set(anchors, cor.Semicolon))

		// after the procedure block ends a semicolon is expected to separate
		//   the block from the parent block
		//   or from the next procedure declaration
		if p.lastToken() == cor.Semicolon {
			p.nextToken()

			// after the procedure block, the parser expects
			//   a statement which also can be an assignment starting with an identifier
			//   the beginning of a new procedure declaration
			//   or the parser would fall back to parent tokens as anchors in the case of a syntax error
			p.tokenHandler.Recover(expectedStatementsIdentifiersProcedures, set(statements, cor.Identifier, cor.ProcedureWord), anchors)
		} else {
			p.appendError(expectedSemicolon, p.lastTokenName())
		}

		// the parent of a block needs to point to its procedure declaration
		block.SetParent(declaration)

		// set the block of the procedure declaration because it was not known before the block was parsed
		declaration.(*ast.ProcedureDeclarationNode).Block = block

		// add the procedure declaration to the list of declarations for the parent block node
		declarations = append(declarations, declaration)
	}

	return declarations
}

// An assignment is an identifier followed by becomes followed by an expression.
func (p *parser) assignment(scope ast.Scope[ast.Declaration], anchors cor.Tokens) ast.Statement {
	var becomesIndex int
	name := p.lastTokenValue()
	nameIndex := p.lastTokenIndex()

	p.nextToken()

	if p.lastToken() == cor.Becomes {
		becomesIndex = p.lastTokenIndex()
		p.nextToken()
	} else {
		p.appendError(expectedBecomes, p.lastTokenName())

		// skip the next token if it is an equal sign to continue parsing
		if p.lastToken() == cor.Equal {
			p.nextToken()
		}
	}

	// the right side of an assignment is an expression
	right := p.expression(scope, anchors)

	// the left side of an assignment is an identifier
	left := ast.NewIdentifierUse(name, scope, ast.VariableEntry, nameIndex)

	endIndex := p.lastTokenIndex()
	return ast.NewAssignmentStatement(left, right, becomesIndex, endIndex)
}

// A read statement is the read operator followed by an identifier that must be a variable.
func (p *parser) read(scope ast.Scope[ast.Declaration]) ast.Statement {
	var name string
	var nameIndex int

	readIndex := p.lastTokenIndex()
	p.nextToken()

	if p.lastToken() != cor.Identifier {
		p.appendError(expectedIdentifier, p.lastTokenName())
	} else {
		name = p.lastTokenValue()
		nameIndex = p.lastTokenIndex()
	}

	p.nextToken()

	if len(name) == 0 {
		return ast.NewEmptyStatement() // in case of a parsing error, return an empty statement
	}

	// a read statement that uses a variable identifier
	endIndex := p.lastTokenIndex()
	return ast.NewReadStatement(ast.NewIdentifierUse(name, scope, ast.VariableEntry, nameIndex), readIndex, endIndex)
}

// A write statement is the write operator followed by an expression.
func (p *parser) write(scope ast.Scope[ast.Declaration], anchors cor.Tokens) ast.Statement {
	writeIndex := p.lastTokenIndex()
	p.nextToken()
	expression := p.expression(scope, anchors)

	endIndex := p.lastTokenIndex()
	return ast.NewWriteStatement(expression, writeIndex, endIndex)
}

// A call statement is the call word followed by a procedure identifier.
func (p *parser) callWord(scope ast.Scope[ast.Declaration]) ast.Statement {
	var name string
	var nameIndex int

	callIndex := p.lastTokenIndex()
	p.nextToken()

	if p.lastToken() != cor.Identifier {
		p.appendError(expectedIdentifier, p.lastTokenName())
	} else {
		name = p.lastTokenValue()
		nameIndex = p.lastTokenIndex()
		p.nextToken()
	}

	if len(name) == 0 {
		return ast.NewEmptyStatement() // in case of a parsing error, return an empty statement
	}

	// a call statement that uses a procedure identifier
	endIndex := p.lastTokenIndex()
	return ast.NewCallStatement(ast.NewIdentifierUse(name, scope, ast.ProcedureEntry, nameIndex), callIndex, endIndex)
}

// An if statement is the if word followed by a condition followed by the then word followed by a statement.
func (p *parser) ifWord(scope ast.Scope[ast.Declaration], anchors cor.Tokens) ast.Statement {
	ifIndex := p.lastTokenIndex()
	p.nextToken()

	// parse the condition which is evaluated to true or false
	condition := p.condition(scope, set(anchors, cor.ThenWord, cor.DoWord))

	if p.lastToken() == cor.ThenWord {
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

	endIndex := p.lastTokenIndex()
	return ast.NewIfStatement(condition, statement, ifIndex, endIndex)
}

// A while statement is the while word followed by a condition followed by the do word followed by a statement.
func (p *parser) whileWord(scope ast.Scope[ast.Declaration], anchors cor.Tokens) ast.Statement {
	whileIndex := p.lastTokenIndex()
	p.nextToken()

	// parse the condition which is evaluated to true or false
	condition := p.condition(scope, set(anchors, cor.DoWord))

	if p.lastToken() == cor.DoWord {
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

	endIndex := p.lastTokenIndex()
	return ast.NewWhileStatement(condition, statement, whileIndex, endIndex)
}

// A begin-end compound statement is the begin word followed by a statements with semicolons followed by the end word.
func (p *parser) beginWord(scope ast.Scope[ast.Declaration], anchors cor.Tokens) ast.Statement {
	beginIndex := p.lastTokenIndex()
	p.nextToken()

	// all statements of the begin-end compound
	compound := make([]ast.Statement, 0)

	// the first statement of a begin-end compound (only if the compound is not empty and there is no parsing error)
	if statement, err := p.statement(scope, set(anchors, cor.EndWord, cor.Semicolon)); !err && statement != nil {
		compound = append(compound, statement)
	}

	for p.lastToken().In(set(statements, cor.Semicolon)) {
		if p.lastToken() == cor.Semicolon {
			p.nextToken()
		} else {
			p.appendError(expectedSemicolon, p.lastTokenName())
		}

		// the next statement of a begin-end compound (only if a statement is available and there is no parsing error)
		if statement, err := p.statement(scope, set(anchors, cor.EndWord, cor.Semicolon)); !err && statement != nil {
			compound = append(compound, statement)
		}
	}

	if p.lastToken() == cor.EndWord {
		p.nextToken()
	} else {
		p.appendError(expectedEnd, p.lastTokenName())
	}

	endIndex := p.lastTokenIndex()
	return ast.NewCompoundStatement(compound, beginIndex, endIndex)
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
func (p *parser) statement(scope ast.Scope[ast.Declaration], anchors cor.Tokens) (ast.Statement, bool) {
	var statement ast.Statement

	switch p.lastToken() {
	case cor.Identifier:
		statement = p.assignment(scope, anchors)

	case cor.Read:
		statement = p.read(scope)

	case cor.Write:
		statement = p.write(scope, anchors)

	case cor.CallWord:
		statement = p.callWord(scope)

	case cor.IfWord:
		statement = p.ifWord(scope, anchors)

	case cor.WhileWord:
		statement = p.whileWord(scope, anchors)

	case cor.BeginWord:
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
	if p.tokenHandler.Recover(unexpectedTokensAfterStatement, anchors, cor.Tokens{}) {
		// in case of a parsing error, return an empty statement
		if statement == nil {
			statement = ast.NewEmptyStatement()
		}

		return statement, true
	}

	return statement, false
}

// A constant identifier is an identifier followed by an equal sign followed by a number and is stored in a constant declaration of the abstract syntax tree
func (p *parser) constantIdentifier(scope ast.Scope[ast.Declaration]) ast.Declaration {
	// in case of a parsing error, return an empty declaration
	declaration := ast.NewEmptyDeclaration()

	if p.lastToken() != cor.Identifier {
		p.appendError(expectedIdentifier, p.lastTokenName())
		return declaration
	}

	constantName := p.lastTokenValue()
	constantNameIndex := p.lastTokenIndex()
	p.nextToken()

	// parse the equal token and the number of the constant, accept non-valid becomes token as equal token
	if p.lastToken().In(set(cor.Equal, cor.Becomes)) {
		if p.lastToken() == cor.Becomes {
			p.appendError(expectedEqual, p.lastTokenName())
		}

		p.nextToken()
		var sign cor.Token

		// support leading plus or minus sign of a number
		if p.lastToken() == cor.Plus || p.lastToken() == cor.Minus {
			sign = p.lastToken()
			p.nextToken()
		}

		if p.lastToken() != cor.Number {
			p.appendError(expectedNumber, p.lastTokenName())

			// skip the next token if it is an identifier to continue parsing
			if p.lastToken() == cor.Identifier {
				p.nextToken()
			}
		} else {
			// create a new constant declaration with the identifier name, the number value, and the scope of the block
			declaration = ast.NewConstantDeclaration(
				constantName,
				p.numberValue(sign, p.lastTokenValue()),
				ast.Integer64,
				scope,
				constantNameIndex)

			p.nextToken()
		}
	} else {
		p.appendError(expectedEqual, p.lastTokenName())
	}

	return declaration
}

// A variable identifier is stored in a variable declaration of the abstract syntax tree
func (p *parser) variableIdentifier(scope ast.Scope[ast.Declaration]) ast.Declaration {
	if p.lastToken() != cor.Identifier {
		p.appendError(expectedIdentifier, p.lastTokenName())
		return ast.NewEmptyDeclaration() // in case of a parsing error, return an empty declaration
	}

	// create a new variable declaration with the identifier name and the scope of the block
	declaration := ast.NewVariableDeclaration(p.lastTokenValue(), ast.Integer64, scope, p.lastTokenIndex())

	p.nextToken()
	return declaration
}

// A procedure identifier is stored in a procedure declaration of the abstract syntax tree
func (p *parser) procedureIdentifier(scope ast.Scope[ast.Declaration]) ast.Declaration {
	if p.lastToken() != cor.Identifier {
		p.appendError(expectedIdentifier, p.lastTokenName())
		return ast.NewEmptyDeclaration() // in case of a parsing error, return an empty declaration
	}

	// the procedure block is not yet known and will be set after the block is parsed
	declaration := ast.NewProcedureDeclaration(p.lastTokenValue(), nil, scope, p.lastTokenIndex())

	p.nextToken()
	return declaration
}

// A condition is either an odd expression or two expressions separated by a comparison operator.
func (p *parser) condition(scope ast.Scope[ast.Declaration], anchors cor.Tokens) ast.Expression {
	// in case of a parsing error, return an empty declaration
	operation := ast.NewEmptyExpression()

	if p.lastToken() == cor.OddWord {
		oddIndex := p.lastTokenIndex()
		p.nextToken()
		operand := p.expression(scope, anchors)
		operation = ast.NewUnaryOperation(ast.Odd, operand, oddIndex)
	} else {
		// handle left expression of a comparison operator
		left := p.expression(scope, set(anchors, cor.Equal, cor.NotEqual, cor.Less, cor.LessEqual, cor.Greater, cor.GreaterEqual))

		if !p.lastToken().In(set(cor.Equal, cor.NotEqual, cor.Less, cor.LessEqual, cor.Greater, cor.GreaterEqual)) {
			p.appendError(expectedComparisonOperator, p.lastTokenName())
		} else {
			comparisonOperator := p.lastToken()
			comparisonOperatorIndex := p.lastTokenIndex()
			p.nextToken()

			// handle right expression of a comparison operator
			right := p.expression(scope, anchors)

			switch comparisonOperator {
			case cor.Equal:
				operation = ast.NewComparisonOperation(ast.Equal, left, right, comparisonOperatorIndex)

			case cor.NotEqual:
				operation = ast.NewComparisonOperation(ast.NotEqual, left, right, comparisonOperatorIndex)

			case cor.Less:
				operation = ast.NewComparisonOperation(ast.Less, left, right, comparisonOperatorIndex)

			case cor.LessEqual:
				operation = ast.NewComparisonOperation(ast.LessEqual, left, right, comparisonOperatorIndex)

			case cor.Greater:
				operation = ast.NewComparisonOperation(ast.Greater, left, right, comparisonOperatorIndex)

			case cor.GreaterEqual:
				operation = ast.NewComparisonOperation(ast.GreaterEqual, left, right, comparisonOperatorIndex)

			default:
				p.appendError(expectedComparisonOperator, p.lastTokenName())
				operation = ast.NewEmptyExpression()
			}
		}
	}

	return operation
}

// An expression is a sequence of terms separated by plus or minus.
func (p *parser) expression(scope ast.Scope[ast.Declaration], anchors cor.Tokens) ast.Expression {
	var operation ast.Expression

	// handle left term of a plus or minus operator
	left := p.term(scope, set(anchors, cor.Plus, cor.Minus))

	for p.lastToken() == cor.Plus || p.lastToken() == cor.Minus {
		plusOrMinus := p.lastToken()
		plusOrMinusIndex := p.lastTokenIndex()
		p.nextToken()

		// handle right term of a plus or minus operator
		right := p.term(scope, set(anchors, cor.Plus, cor.Minus))

		if plusOrMinus == cor.Plus {
			operation = ast.NewBinaryOperation(ast.Plus, left, right, plusOrMinusIndex)
		} else {
			operation = ast.NewBinaryOperation(ast.Minus, left, right, plusOrMinusIndex)
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
func (p *parser) term(scope ast.Scope[ast.Declaration], anchors cor.Tokens) ast.Expression {
	var operation ast.Expression

	// handle left factor of a times or divide operator
	left := p.factor(scope, set(anchors, cor.Times, cor.Divide))

	for p.lastToken() == cor.Times || p.lastToken() == cor.Divide {
		timesOrDevide := p.lastToken()
		timesOrDevideIndex := p.lastTokenIndex()
		p.nextToken()

		// handle right factor of a times or divide operator
		right := p.factor(scope, set(anchors, cor.Times, cor.Divide))

		if timesOrDevide == cor.Times {
			operation = ast.NewBinaryOperation(ast.Times, left, right, timesOrDevideIndex)

		} else {
			operation = ast.NewBinaryOperation(ast.Divide, left, right, timesOrDevideIndex)
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
func (p *parser) factor(scope ast.Scope[ast.Declaration], anchors cor.Tokens) ast.Expression {
	var sign cor.Token
	var signIndex int

	// in case of a parsing error, return an empty declaration
	operand := ast.NewEmptyExpression()

	// handle leading plus or minus sign of a factor
	if p.lastToken() == cor.Plus || p.lastToken() == cor.Minus {
		sign = p.lastToken()
		signIndex = p.lastTokenIndex()
		p.nextToken()
	}

	// at the beginning of a factor
	//   the expected tokens are identifiers, numbers, and left parentheses
	//   or the parser would fall back to all block-tokens as anchors in the case of a syntax error
	p.tokenHandler.Recover(expectedIdentifiersNumbersExpressions, factors, anchors)

	for p.lastToken().In(factors) {
		if p.lastToken() == cor.Identifier {
			// the factor can be a constant or a variable
			operand = ast.NewIdentifierUse(p.lastTokenValue(), scope, ast.ConstantEntry|ast.VariableEntry, p.lastTokenIndex())
			p.nextToken()
		} else if p.lastToken() == cor.Number {
			operand = ast.NewLiteral(p.numberValue(sign, p.lastTokenValue()), ast.Integer64, scope, p.lastTokenIndex())
			sign = cor.Unknown
			p.nextToken()
		} else if p.lastToken() == cor.LeftParenthesis {
			p.nextToken()
			operand = p.expression(scope, set(anchors, cor.RightParenthesis))

			if p.lastToken() == cor.RightParenthesis {
				p.nextToken()
			} else {
				p.appendError(expectedRightParenthesis, p.lastTokenName())
			}
		}

		// after a factor, the parser expects
		//   a times or divide operator
		//   a plus or minus operator
		//   a comparison operator
		//   a right parenthesis
		//   or the parser would fall back to all block-tokens as anchors in the case of a syntax error
		p.tokenHandler.Recover(unexpectedTokens, anchors, set(cor.LeftParenthesis))
	}

	// negate the factor if a leading minus sign is present
	if sign == cor.Minus {
		operand = ast.NewUnaryOperation(ast.Negate, operand, signIndex)
	}

	return operand
}

// Return the next token description from the token handler.
func (p *parser) nextToken() bool {
	return p.tokenHandler.NextTokenDescription()
}

// Wrapper to get token from the last token description.
func (p *parser) lastToken() cor.Token {
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

// Append parser error to the token handler.
func (p *parser) appendError(code cor.Failure, value any) {
	p.tokenHandler.AppendError(p.tokenHandler.NewError(cor.Error, code, value))
}

// Wrapper to get joined slice of all tokens within the given TokenSet interfaces.
func set(tss ...cor.TokenSet) cor.Tokens {
	return cor.Set(tss...)
}

// Analyze a number and convert it to an Integer64 value (-9223372036854775808 to 9223372036854775807).
func (e *parser) numberValue(sign cor.Token, number string) int64 {
	if sign == cor.Minus {
		number = "-" + number
	}

	value, err := strconv.ParseInt(number, 10, integerBitSize)

	if err != nil {
		e.appendError(illegalInteger, number)
	}

	return value
}
