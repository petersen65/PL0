// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package analyzer

import (
	ast "github.com/petersen65/pl0/v3/ast"
	eh "github.com/petersen65/pl0/v3/errors"
	tok "github.com/petersen65/pl0/v3/token"
)

// Constant folding evaluates compile-time constant expressions in the abstract syntax tree and replaces them with their computed values.
// It optimizes the code by pre-computing arithmetic operations, comparisons, and other expressions where all operands are known constants.
type constantFolding struct {
	abstractSyntax ast.Block        // abstract syntax tree to run semantic analysis on
	tokenHandler   tok.TokenHandler // token handler that manages the tokens of the token stream
}

// Return the interface of the constant folding implementation.
func newConstantFolding(abstractSyntax ast.Block, tokenHandler tok.TokenHandler) *constantFolding {
	return &constantFolding{
		abstractSyntax: abstractSyntax,
		tokenHandler:   tokenHandler,
	}
}

// Analyze the abstract syntax tree for constant expressions and fold them where possible.
func (c *constantFolding) Accept() {
	c.abstractSyntax.Accept(c)
}

// Walk the block abstract syntax tree by visiting all declarations and the block statement.
func (c *constantFolding) VisitBlock(b ast.Block) {
	for _, decl := range b.Declarations() {
		decl.Accept(c)
	}

	b.Statement().Accept(c)
}

// x
func (c *constantFolding) VisitConstantDeclaration(cd ast.ConstantDeclaration) {
}

// x
func (c *constantFolding) VisitVariableDeclaration(vd ast.VariableDeclaration) {
}

// x
func (c *constantFolding) VisitFunctionDeclaration(fd ast.FunctionDeclaration) {
	// visit the block of the function or procedure declaration
	fd.Block().Accept(c)
}

// x
func (c *constantFolding) VisitLiteralUse(lu ast.LiteralUse) {
}

// x
func (c *constantFolding) VisitIdentifierUse(iu ast.IdentifierUse) {
}

// x
func (c *constantFolding) VisitUnaryOperation(uo ast.UnaryOperation) {
	uo.Operand().Accept(c)
}

// x
func (c *constantFolding) VisitArithmeticOperation(bo ast.ArithmeticOperation) {
	bo.Left().Accept(c)
	bo.Right().Accept(c)
}

// x
func (c *constantFolding) VisitComparisonOperation(co ast.ComparisonOperation) {
	co.Left().Accept(c)
	co.Right().Accept(c)
}

// x
func (c *constantFolding) VisitAssignmentStatement(as ast.AssignmentStatement) {
	as.Variable().Accept(c)
	as.Expression().Accept(c)
}

// x
func (c *constantFolding) VisitReadStatement(rs ast.ReadStatement) {
	rs.Variable().Accept(c)
}

// x
func (c *constantFolding) VisitWriteStatement(ws ast.WriteStatement) {
	ws.Expression().Accept(c)
}

// x
func (c *constantFolding) VisitCallStatement(cs ast.CallStatement) {
	cs.Procedure().Accept(c)
}

// x
func (c *constantFolding) VisitIfStatement(is ast.IfStatement) {
	is.Condition().Accept(c)
	is.Statement().Accept(c)
}

// x
func (c *constantFolding) VisitWhileStatement(ws ast.WhileStatement) {
	ws.Condition().Accept(c)
	ws.Statement().Accept(c)
}

// x
func (c *constantFolding) VisitCompoundStatement(cs ast.CompoundStatement) {
	for _, statement := range cs.Statements() {
		statement.Accept(c)
	}
}

// Append an error from the constant folding to the token handler's error list.
func (c *constantFolding) appendError(code eh.Failure, index int, values ...any) {
	c.tokenHandler.AppendError(c.tokenHandler.NewErrorOnIndex(eh.Error, code, index, values...))
}
