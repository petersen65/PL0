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

// Enter the constant declaration as a symbol into the block's scope and check for redeclaration.
func (tc *typeChecking) VisitConstantDeclaration(cd ast.ConstantDeclaration) {
}

// Enter the variable declaration as a symbol into the block's scope and check for redeclaration.
func (tc *typeChecking) VisitVariableDeclaration(vd ast.VariableDeclaration) {
}

// Enter the function declaration as a symbol into the block's scope and check for redeclaration.
func (tc *typeChecking) VisitFunctionDeclaration(fd ast.FunctionDeclaration) {
	// visit the block of the function or procedure declaration
	fd.Block().Accept(tc)
}

// Visit the literal-use node.
func (tc *typeChecking) VisitLiteralUse(lu ast.LiteralUse) {
}

// Check if the used identifier is declared and if it is used correctly according to its symbol kind. Record the usage of the identifier in its declaration.
func (tc *typeChecking) VisitIdentifierUse(iu ast.IdentifierUse) {
}

// Visit the unary operation node and set the usage mode bit to read for all constants and variables in the operand expression.
func (tc *typeChecking) VisitUnaryOperation(uo ast.UnaryOperation) {
	uo.Operand().Accept(tc)
}

// Visit the arithmetic operation node and set the usage mode bit to read for all constants and variables in the left and right expressions.
func (tc *typeChecking) VisitArithmeticOperation(ao ast.ArithmeticOperation) {
	ao.Left().Accept(tc)
	ao.Right().Accept(tc)

    // get types of both operands
    leftType := ao.Left().DataType()
    rightType := ao.Right().DataType()

	// if either type is nil, errors have already been reported so just return
    if leftType == nil || rightType == nil {
        return
    }

	// check if types match
    if !leftType.Equal(rightType) {
		// errorMessage := fmt.Sprintf(incompatibleTypesInArithmeticOperation, ao, leftType, rightType)

		// 	tc.appendError(expectedVariableIdentifier, left, iu.Index())


        // tc.appendError(eh.TypeMismatch, 
        //     fmt.Sprintf("incompatible types: %s and %s", leftType.String(), rightType.String()), 
        //     ao.TokenIndex())
        // return
    }
}

// Visit the comparison operation node and set the usage mode bit to read for all constants and variables in the left and right expressions.
func (tc *typeChecking) VisitComparisonOperation(co ast.ComparisonOperation) {
	co.Left().Accept(tc)
	co.Right().Accept(tc)
}

// Visit the assignment statement node and set the usage mode bit to write for the variable that is assigned to.
func (tc *typeChecking) VisitAssignmentStatement(as ast.AssignmentStatement) {
	as.Variable().Accept(tc)
	as.Expression().Accept(tc)
}

// Visit the read statement node and set the usage mode bit to write for the variable that is read into.
func (tc *typeChecking) VisitReadStatement(rs ast.ReadStatement) {
	rs.Variable().Accept(tc)
}

// Visit the write statement node and set the usage mode bit to read for all constants and variables in the write expression.
func (tc *typeChecking) VisitWriteStatement(ws ast.WriteStatement) {
	ws.Expression().Accept(tc)
}

// Visit the call statement node and set the usage mode bit to execute for the called function or procedure.
func (tc *typeChecking) VisitCallStatement(cs ast.CallStatement) {
	cs.Function().Accept(tc)
}

// Visit the if statement node and set the usage mode bit to read for all constants and variables in the condition.
func (tc *typeChecking) VisitIfStatement(is ast.IfStatement) {
	is.Condition().Accept(tc)
	is.Statement().Accept(tc)
}

// Visit the while statement node and set the usage mode bit to read for all constants and variables in the condition.
func (tc *typeChecking) VisitWhileStatement(ws ast.WhileStatement) {
	ws.Condition().Accept(tc)
	ws.Statement().Accept(tc)
}

// Visit the compound statement node by visiting all its statements.
func (tc *typeChecking) VisitCompoundStatement(cs ast.CompoundStatement) {
	for _, statement := range cs.Statements() {
		statement.Accept(tc)
	}
}

// Append an error from the type checking to the token handler's error list.
func (tc *typeChecking) appendError(code eh.Failure, index int, values ...any) {
	tc.tokenHandler.AppendError(tc.tokenHandler.NewErrorOnIndex(eh.Error, code, index, values...))
}
