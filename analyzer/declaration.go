// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package analyzer

import (
	ast "github.com/petersen65/PL0/ast"
	tok "github.com/petersen65/PL0/token"
)

type declarationAnalysis struct {
	abstractSyntax ast.Block        // abstract syntax tree to run declaration analysis on
	tokenHandler   tok.TokenHandler // token handler that manages the tokens of the token stream
}

// Create new declaration analyzer with the given abstract syntax tree, error handler, and token handler.
func newDeclarationAnalysis(abstractSyntax ast.Block, tokenHandler tok.TokenHandler) DeclarationAnalysis {
	return &declarationAnalysis{abstractSyntax: abstractSyntax, tokenHandler: tokenHandler}
}

func (a *declarationAnalysis) Analyze() {
	a.abstractSyntax.Accept(a)
}

func (a *declarationAnalysis) VisitBlock(block *ast.BlockNode) {
	panic("unimplemented")
}

func (a *declarationAnalysis) VisitConstantDeclaration(constant *ast.ConstantDeclarationNode) {
	panic("unimplemented")
}

func (a *declarationAnalysis) VisitVariableDeclaration(variable *ast.VariableDeclarationNode) {
	panic("unimplemented")
}

func (a *declarationAnalysis) VisitProcedureDeclaration(procedure *ast.ProcedureDeclarationNode) {
	panic("unimplemented")
}

func (a *declarationAnalysis) VisitLiteral(literal *ast.LiteralNode) {
	panic("unimplemented")
}

func (a *declarationAnalysis) VisitConstantReference(constant *ast.ConstantReferenceNode) {
	panic("unimplemented")
}

func (a *declarationAnalysis) VisitVariableReference(variable *ast.VariableReferenceNode) {
	panic("unimplemented")
}

func (a *declarationAnalysis) VisitProcedureReference(procedure *ast.ProcedureReferenceNode) {
	panic("unimplemented")
}

func (a *declarationAnalysis) VisitUnaryOperation(operation *ast.UnaryOperationNode) {
	panic("unimplemented")
}

func (a *declarationAnalysis) VisitBinaryOperation(operation *ast.BinaryOperationNode) {
	panic("unimplemented")
}

func (a *declarationAnalysis) VisitConditionalOperation(operation *ast.ConditionalOperationNode) {
	panic("unimplemented")
}

func (a *declarationAnalysis) VisitAssignmentStatement(assignment *ast.AssignmentStatementNode) {
	panic("unimplemented")
}

func (a *declarationAnalysis) VisitReadStatement(read *ast.ReadStatementNode) {
	panic("unimplemented")
}

func (a *declarationAnalysis) VisitWriteStatement(write *ast.WriteStatementNode) {
	panic("unimplemented")
}

func (a *declarationAnalysis) VisitCallStatement(call *ast.CallStatementNode) {
	panic("unimplemented")
}	

func (a *declarationAnalysis) VisitIfStatement(ifStmt *ast.IfStatementNode) {
	panic("unimplemented")
}	

func (a *declarationAnalysis) VisitWhileStatement(whileStmt *ast.WhileStatementNode) {
	panic("unimplemented")
}

func (a *declarationAnalysis) VisitCompoundStatement(compound *ast.CompoundStatementNode) {
	panic("unimplemented")
}	
