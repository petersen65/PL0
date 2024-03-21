// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package analyzer

import (
	ast "github.com/petersen65/PL0/ast"
	tok "github.com/petersen65/PL0/token"
)

// Private implementation of the declaration analysis.
type declarationAnalysis struct {
	abstractSyntax ast.Block        // abstract syntax tree to run declaration analysis on
	errorHandler   tok.ErrorHandler // error handler that is used to handle errors that occurred during semantic analysis
	tokenHandler   tok.TokenHandler // token handler that manages the tokens of the token stream
}

// Return the public interface of the private declaration analysis implementation.
func newDeclarationAnalysis(abstractSyntax ast.Block, errorHandler tok.ErrorHandler, tokenHandler tok.TokenHandler) DeclarationAnalysis {
	return &declarationAnalysis{
		abstractSyntax: abstractSyntax,
		errorHandler:   errorHandler,
		tokenHandler:   tokenHandler,
	}
}

// Analyze the abstract syntax tree for declaration and reference errors and fill in the symbol table.
// Declaration analysis itself is performing a top down, left to right, and leftmost derivation walk on the abstract syntax tree.
func (a *declarationAnalysis) Analyze() error {
	if a.abstractSyntax == nil || a.errorHandler == nil || a.tokenHandler == nil {
		return tok.NewGeneralError(tok.Analyzer, failureMap, tok.Error, invalidDeclarationAnalysisState, nil)
	}

	// an existing error handler can have errors from other compiler components
	startErrorCount := a.errorHandler.Count()

	// start the declaration analysis by visiting the abstract syntax tree
	a.abstractSyntax.Accept(a)

	// number of errors that occurred during declaration analysis
	analyzerErrorCount := a.errorHandler.Count() - startErrorCount

	if analyzerErrorCount == 1 {
		return tok.NewGeneralError(tok.Analyzer, failureMap, tok.Error, declarationAnalysisError, nil)
	} else if analyzerErrorCount > 1 {
		return tok.NewGeneralError(tok.Analyzer, failureMap, tok.Error, declarationAnalysisErrors, analyzerErrorCount)
	}	

	return nil
}

func (a *declarationAnalysis) VisitBlock(bn *ast.BlockNode) {
	for _, declaration := range bn.Declarations {
		declaration.Accept(a)
	}

	bn.Statement.Accept(a)
}

func (a *declarationAnalysis) VisitConstantDeclaration(cd *ast.ConstantDeclarationNode) {
	if cd.Scope.Lookup(cd.Name) != nil {
		a.appendError(identifierAlreadyDeclared, cd.Name, cd.TokenStreamIndex)
	} else {
		cd.Scope.Insert(&ast.Symbol{
			Name:        cd.Name,
			Kind:        ast.Constant,
			Declaration: cd,
		})
	}
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

// Append analyzer error to the token handler.
func (a *declarationAnalysis) appendError(code tok.Failure, value any, index int) {
	a.tokenHandler.AppendError(a.tokenHandler.NewErrorOnIndex(code, value, index))
}
