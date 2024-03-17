// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package analyzer

import ast "github.com/petersen65/PL0/ast"

type nameAnalysis struct {
	abstractSyntax ast.Block // abstract syntax tree to run semantic analysis on
}

// Create new name analyzer with the given abstract syntax tree.
func newNameAnalysis(abstractSyntax ast.Block) NameAnalysis {
	return &nameAnalysis{abstractSyntax: abstractSyntax}
}

// Analyze the abstract syntax tree for semantic errors.
func (a *nameAnalysis) Analyze() {
	a.abstractSyntax.Accept(a)
}

// VisitAssignmentStatement implements ast.Visitor.
func (a *nameAnalysis) VisitAssignmentStatement(assignment *ast.AssignmentStatementNode) {
	panic("unimplemented")
}

// VisitBinaryOperation implements ast.Visitor.
func (a *nameAnalysis) VisitBinaryOperation(operation *ast.BinaryOperationNode) {
	panic("unimplemented")
}

// VisitBlock implements ast.Visitor.
func (a *nameAnalysis) VisitBlock(block *ast.BlockNode) {
	panic("unimplemented")
}

// VisitCallStatement implements ast.Visitor.
func (a *nameAnalysis) VisitCallStatement(call *ast.CallStatementNode) {
	panic("unimplemented")
}

// VisitCompoundStatement implements ast.Visitor.
func (a *nameAnalysis) VisitCompoundStatement(compound *ast.CompoundStatementNode) {
	panic("unimplemented")
}

// VisitConditionalOperation implements ast.Visitor.
func (a *nameAnalysis) VisitConditionalOperation(operation *ast.ConditionalOperationNode) {
	panic("unimplemented")
}

// VisitConstantReference implements ast.Visitor.
func (a *nameAnalysis) VisitConstantReference(constant *ast.ConstantReferenceNode) {
	panic("unimplemented")
}

// VisitIfStatement implements ast.Visitor.
func (a *nameAnalysis) VisitIfStatement(ifStmt *ast.IfStatementNode) {
	panic("unimplemented")
}

// VisitLiteral implements ast.Visitor.
func (a *nameAnalysis) VisitLiteral(literal *ast.LiteralNode) {
	panic("unimplemented")
}

// VisitReadStatement implements ast.Visitor.
func (a *nameAnalysis) VisitReadStatement(read *ast.ReadStatementNode) {
	panic("unimplemented")
}

// VisitSymbol implements ast.Visitor.
func (a *nameAnalysis) VisitSymbol(symbol *ast.Symbol) {
	panic("unimplemented")
}

// VisitUnaryOperation implements ast.Visitor.
func (a *nameAnalysis) VisitUnaryOperation(operation *ast.UnaryOperationNode) {
	panic("unimplemented")
}

// VisitVariableReference implements ast.Visitor.
func (a *nameAnalysis) VisitVariableReference(variable *ast.VariableReferenceNode) {
	panic("unimplemented")
}

// VisitWhileStatement implements ast.Visitor.
func (a *nameAnalysis) VisitWhileStatement(whileStmt *ast.WhileStatementNode) {
	panic("unimplemented")
}

// VisitWriteStatement implements ast.Visitor.
func (a *nameAnalysis) VisitWriteStatement(write *ast.WriteStatementNode) {
	panic("unimplemented")
}
