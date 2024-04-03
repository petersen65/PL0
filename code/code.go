// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package code

import "github.com/petersen65/PL0/v2/ast"

type intermediateCode struct {
	abstractSyntax ast.Block
}

func newIntermediateCode(abstractSyntax ast.Block) IntermediateCode {
	return &intermediateCode{abstractSyntax: abstractSyntax}
}

func (i *intermediateCode) Generate() {
	i.abstractSyntax.Accept(i)
}

func (i *intermediateCode) VisitBlock(bn *ast.BlockNode) {
	for _, declaration := range bn.Declarations {
		declaration.Accept(i)
	}

	bn.Statement.Accept(i)
}

func (i *intermediateCode) VisitConstantDeclaration(declaration *ast.ConstantDeclarationNode) {
}

func (i *intermediateCode) VisitVariableDeclaration(declaration *ast.VariableDeclarationNode) {
}

func (i *intermediateCode) VisitProcedureDeclaration(declaration *ast.ProcedureDeclarationNode) {
}

func (i *intermediateCode) VisitLiteral(literal *ast.LiteralNode) {
}

func (i *intermediateCode) VisitIdentifierUse(use *ast.IdentifierUseNode) {
}

func (i *intermediateCode) VisitUnaryOperation(operation *ast.UnaryOperationNode) {
}

func (i *intermediateCode) VisitBinaryOperation(operation *ast.BinaryOperationNode) {
}

func (i *intermediateCode) VisitConditionalOperation(operation *ast.ConditionalOperationNode) {
}

func (i *intermediateCode) VisitAssignmentStatement(assignment *ast.AssignmentStatementNode) {
}

func (i *intermediateCode) VisitReadStatement(read *ast.ReadStatementNode) {
}

func (i *intermediateCode) VisitWriteStatement(write *ast.WriteStatementNode) {
}

func (i *intermediateCode) VisitCallStatement(call *ast.CallStatementNode) {
}

func (i *intermediateCode) VisitIfStatement(ifStmt *ast.IfStatementNode) {
}

func (i *intermediateCode) VisitWhileStatement(whileStmt *ast.WhileStatementNode) {
}

func (i *intermediateCode) VisitCompoundStatement(compound *ast.CompoundStatementNode) {
}
