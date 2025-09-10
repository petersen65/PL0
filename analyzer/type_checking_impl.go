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
func (tc *typeChecking) Accept() {
	tc.abstractSyntax.Accept(tc)
}

// Walk the block abstract syntax tree by visiting all declarations and the block statement.
func (tc *typeChecking) VisitBlock(b ast.Block) {
	for _, decl := range b.Declarations() {
		decl.Accept(tc)
	}

	b.Statement().Accept(tc)
}

// x
func (tc *typeChecking) VisitConstantDeclaration(cd ast.ConstantDeclaration) {
}

// x
func (tc *typeChecking) VisitVariableDeclaration(vd ast.VariableDeclaration) {
}

// x
func (tc *typeChecking) VisitFunctionDeclaration(fd ast.FunctionDeclaration) {
	// visit the block of the function or procedure declaration
	fd.Block().Accept(tc)
}

// x
func (tc *typeChecking) VisitLiteralUse(lu ast.LiteralUse) {
}

// x
func (tc *typeChecking) VisitIdentifierUse(iu ast.IdentifierUse) {
}

// Visit the unary operation node and verify that the operand type supports the unary operator.
// Checks that the operand's data type has the required capabilities for the specific operation (negate or odd).
func (tc *typeChecking) VisitUnaryOperation(uo ast.UnaryOperation) {
	uo.Operand().Accept(tc)

	// get data type of the operand
	operandType := uo.Operand().DataType()

	// if the data type is nil, errors have already been reported so just return
	if operandType == nil {
		return
	}

	switch uo.Operation() {
	case ast.Negate:
		// check if the data type of the unary operation operand meets all requirements of the negate operation
		if !operandType.HasAllCapabilities(uo.Requirements()) {
			tc.appendError(dataTypeCannotBeUsedInNegateOperation, uo.Index(), uo, uo.Requirements(), operandType)
		}

	case ast.Odd:
		// check if the data type of the unary operation operand meets all requirements of the odd operation
		if !operandType.HasAllCapabilities(uo.Requirements()) {
			tc.appendError(dataTypeCannotBeUsedInOddOperation, uo.Index(), uo, uo.Requirements(), operandType)
		}

	default:
		panic(eh.NewGeneralError(eh.Analyzer, failureMap, eh.Fatal, unknownUnaryOperation, nil))
	}
}

// Visit the arithmetic operation node and verify type compatibility for binary arithmetic operators.
// Ensures both operands have matching types and that the type supports numeric operations.
func (tc *typeChecking) VisitArithmeticOperation(ao ast.ArithmeticOperation) {
	ao.Left().Accept(tc)
	ao.Right().Accept(tc)

	// get data types of both operands
	leftType := ao.Left().DataType()
	rightType := ao.Right().DataType()

	// if either data type is nil, errors have already been reported so just return
	if leftType == nil || rightType == nil {
		return
	}

	// check if the data types of both arithmetic operation operands are equal
	if !leftType.Equal(rightType) {
		tc.appendError(incompatibleDataTypesInArithmeticOperation, ao.Index(), ao, leftType, rightType)
		return
	}

	// check if the data type of the left operand meets all requirements of the arithmetic operation
	if !leftType.HasAllCapabilities(ao.Requirements()) {
		tc.appendError(dataTypeCannotBeUsedInArithmeticOperation, ao.Index(), ao, ao.Requirements(), leftType)
	}
}

// Visit the comparison operation node and verify type compatibility for comparison operators.
// Ensures both operands have matching types and that the type supports the specific comparison operation (equality for == and !=, ordering for <, <=, >, >=).
func (tc *typeChecking) VisitComparisonOperation(co ast.ComparisonOperation) {
	co.Left().Accept(tc)
	co.Right().Accept(tc)

	// get data types of both operands
	leftType := co.Left().DataType()
	rightType := co.Right().DataType()

	// if either data type is nil, errors have already been reported so just return
	if leftType == nil || rightType == nil {
		return
	}

	// check if the data types of both comparison operation operands are equal
	if !leftType.Equal(rightType) {
		tc.appendError(incompatibleDataTypesInComparisonOperation, co.Index(), co, leftType, rightType)
		return
	}

	// check if the data type of the left operand meets all requirements of the comparison operation
	if !leftType.HasAllCapabilities(co.Requirements()) {
		tc.appendError(dataTypeCannotBeUsedInComparisonOperation, co.Index(), co, co.Requirements(), leftType)
	}
}

// x
func (tc *typeChecking) VisitAssignmentStatement(as ast.AssignmentStatement) {
	as.Variable().Accept(tc)
	as.Expression().Accept(tc)
}

// x
func (tc *typeChecking) VisitReadStatement(rs ast.ReadStatement) {
	rs.Variable().Accept(tc)
}

// x
func (tc *typeChecking) VisitWriteStatement(ws ast.WriteStatement) {
	ws.Expression().Accept(tc)
}

// x
func (tc *typeChecking) VisitCallStatement(cs ast.CallStatement) {
	cs.Function().Accept(tc)
}

// x
func (tc *typeChecking) VisitIfStatement(is ast.IfStatement) {
	is.Condition().Accept(tc)
	is.Statement().Accept(tc)
}

// x
func (tc *typeChecking) VisitWhileStatement(ws ast.WhileStatement) {
	ws.Condition().Accept(tc)
	ws.Statement().Accept(tc)
}

// x
func (tc *typeChecking) VisitCompoundStatement(cs ast.CompoundStatement) {
	for _, statement := range cs.Statements() {
		statement.Accept(tc)
	}
}

// Append an error from the type checking to the token handler's error list.
func (tc *typeChecking) appendError(code eh.Failure, index int, values ...any) {
	tc.tokenHandler.AppendError(tc.tokenHandler.NewErrorOnIndex(eh.Error, code, index, values...))
}
