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

// Create new declaration analyzer with the given abstract syntax tree and token handler.
func newDeclarationAnalysis(abstractSyntax ast.Block, tokenHandler tok.TokenHandler) DeclarationAnalysis {
	return &declarationAnalysis{abstractSyntax: abstractSyntax, tokenHandler: tokenHandler}
}

// Analyze the abstract syntax tree for declaration and reference errors and fill in the symbol table.
// Declaration analysis itself is performing a top down, left to right, and leftmost derivation walk on the abstract syntax tree.
func (a *declarationAnalysis) Analyze() {
	a.abstractSyntax.Accept(a)
}

func (a *declarationAnalysis) VisitBlock(bn *ast.BlockNode) {
}

func (a *declarationAnalysis) VisitConstantDeclaration(cd *ast.ConstantDeclarationNode) {
}

func (a *declarationAnalysis) VisitVariableDeclaration(vd *ast.VariableDeclarationNode) {
}

func (a *declarationAnalysis) VisitProcedureDeclaration(pd *ast.ProcedureDeclarationNode) {
}

func (a *declarationAnalysis) VisitLiteral(ln *ast.LiteralNode) {
}

func (a *declarationAnalysis) VisitConstantReference(cr *ast.ConstantReferenceNode) {
}

func (a *declarationAnalysis) VisitVariableReference(vr *ast.VariableReferenceNode) {
}

func (a *declarationAnalysis) VisitProcedureReference(pr *ast.ProcedureReferenceNode) {
}

func (a *declarationAnalysis) VisitUnaryOperation(uo *ast.UnaryOperationNode) {
}

func (a *declarationAnalysis) VisitBinaryOperation(bo *ast.BinaryOperationNode) {
}

func (a *declarationAnalysis) VisitConditionalOperation(co *ast.ConditionalOperationNode) {
}

func (a *declarationAnalysis) VisitAssignmentStatement(as *ast.AssignmentStatementNode) {
}

func (a *declarationAnalysis) VisitReadStatement(rs *ast.ReadStatementNode) {
}

func (a *declarationAnalysis) VisitWriteStatement(ws *ast.WriteStatementNode) {
}

func (a *declarationAnalysis) VisitCallStatement(cs *ast.CallStatementNode) {
}	

func (a *declarationAnalysis) VisitIfStatement(is *ast.IfStatementNode) {
}	

func (a *declarationAnalysis) VisitWhileStatement(ws *ast.WhileStatementNode) {
}

func (a *declarationAnalysis) VisitCompoundStatement(cs *ast.CompoundStatementNode) {
}	
