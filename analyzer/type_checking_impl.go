// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package analyzer

import (
	ast "github.com/petersen65/pl0/v3/ast"
	eh "github.com/petersen65/pl0/v3/errors"
	tok "github.com/petersen65/pl0/v3/token"
)

// Type checking validates the type correctness of all expressions and statements in the abstract syntax tree.
// It verifies that operators are applied to compatible types, assignments match declared types, and function calls provide correct argument types.
type typeChecking struct {
	abstractSyntax ast.Block        // abstract syntax tree to run semantic analysis on
	tokenHandler   tok.TokenHandler // token handler that manages the tokens of the token stream
}

// Return the interface of the type checking implementation.
func newTypeChecking(abstractSyntax ast.Block, tokenHandler tok.TokenHandler) *typeChecking {
	return &typeChecking{
		abstractSyntax: abstractSyntax,
		tokenHandler:   tokenHandler,
	}
}

// Analyze the abstract syntax tree for type correctness of all expressions and statements.
func (c *typeChecking) Accept() {
	c.abstractSyntax.Accept(c)
}

// Walk the block abstract syntax tree by visiting all declarations and the block statement.
func (c *typeChecking) VisitBlock(b ast.Block) {
	for _, decl := range b.Declarations() {
		decl.Accept(c)
	}

	b.Statement().Accept(c)
}

// Enter the constant declaration as a symbol into the block's scope and check for redeclaration.
func (c *typeChecking) VisitConstantDeclaration(cd ast.ConstantDeclaration) {
}

// Enter the variable declaration as a symbol into the block's scope and check for redeclaration.
func (c *typeChecking) VisitVariableDeclaration(vd ast.VariableDeclaration) {
}

// Enter the function declaration as a symbol into the block's scope and check for redeclaration.
func (c *typeChecking) VisitFunctionDeclaration(fd ast.FunctionDeclaration) {
	// visit the block of the function or procedure declaration
	fd.Block().Accept(c)
}

// Visit the literal-use node.
func (c *typeChecking) VisitLiteralUse(lu ast.LiteralUse) {
}

// Check if the used identifier is declared and if it is used correctly according to its symbol kind. Record the usage of the identifier in its declaration.
func (c *typeChecking) VisitIdentifierUse(iu ast.IdentifierUse) {
}

// Visit the unary operation node and set the usage mode bit to read for all constants and variables in the operand expression.
func (c *typeChecking) VisitUnaryOperation(uo ast.UnaryOperation) {
	uo.Operand().Accept(c)
}

// Visit the arithmetic operation node and set the usage mode bit to read for all constants and variables in the left and right expressions.
func (c *typeChecking) VisitArithmeticOperation(bo ast.ArithmeticOperation) {
	bo.Left().Accept(c)
	bo.Right().Accept(c)
}

// Visit the comparison operation node and set the usage mode bit to read for all constants and variables in the left and right expressions.
func (c *typeChecking) VisitComparisonOperation(co ast.ComparisonOperation) {
	co.Left().Accept(c)
	co.Right().Accept(c)
}

// Visit the assignment statement node and set the usage mode bit to write for the variable that is assigned to.
func (c *typeChecking) VisitAssignmentStatement(as ast.AssignmentStatement) {
	as.Variable().Accept(c)
	as.Expression().Accept(c)
}

// Visit the read statement node and set the usage mode bit to write for the variable that is read into.
func (c *typeChecking) VisitReadStatement(rs ast.ReadStatement) {
	rs.Variable().Accept(c)
}

// Visit the write statement node and set the usage mode bit to read for all constants and variables in the write expression.
func (c *typeChecking) VisitWriteStatement(ws ast.WriteStatement) {
	ws.Expression().Accept(c)
}

// Visit the call statement node and set the usage mode bit to execute for the called function or procedure.
func (c *typeChecking) VisitCallStatement(cs ast.CallStatement) {
	cs.Function().Accept(c)
}

// Visit the if statement node and set the usage mode bit to read for all constants and variables in the condition.
func (c *typeChecking) VisitIfStatement(is ast.IfStatement) {
	is.Condition().Accept(c)
	is.Statement().Accept(c)
}

// Visit the while statement node and set the usage mode bit to read for all constants and variables in the condition.
func (c *typeChecking) VisitWhileStatement(ws ast.WhileStatement) {
	ws.Condition().Accept(c)
	ws.Statement().Accept(c)
}

// Visit the compound statement node by visiting all its statements.
func (c *typeChecking) VisitCompoundStatement(cs ast.CompoundStatement) {
	for _, statement := range cs.Statements() {
		statement.Accept(c)
	}
}

// Append an error from the type checking to the token handler's error list.
func (c *typeChecking) appendError(code eh.Failure, value any, index int) {
	c.tokenHandler.AppendError(c.tokenHandler.NewErrorOnIndex(eh.Error, code, value, index))
}
