// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package code

import "github.com/petersen65/PL0/v2/ast"

type blockMetaData struct {
	stackOffset uint64 // offset counter for all variables in the stack frame of the block
}

type intermediateCode struct {
	abstractSyntax ast.Block
	metaData       map[int32]*blockMetaData
	module         *Module
}

func newIntermediateCode(abstractSyntax ast.Block) IntermediateCode {
	return &intermediateCode{abstractSyntax: abstractSyntax, metaData: make(map[int32]*blockMetaData), module: new(Module)}
}

func (i *intermediateCode) Generate() {
	i.abstractSyntax.Accept(i)
}

func (i *intermediateCode) VisitBlock(bn *ast.BlockNode) {
	i.metaData[bn.UniqueId] = new(blockMetaData)

	for _, declaration := range bn.Declarations {
		declaration.Accept(i)
	}

	bn.Statement.Accept(i)
}

func (i *intermediateCode) VisitConstantDeclaration(declaration *ast.ConstantDeclarationNode) {
}

func (i *intermediateCode) VisitVariableDeclaration(vd *ast.VariableDeclarationNode) {
	block := ast.SearchBlock(ast.CurrentBlock, vd)
	vd.Offset = i.metaData[block.UniqueId].stackOffset
	i.metaData[block.UniqueId].stackOffset++
}

func (i *intermediateCode) VisitProcedureDeclaration(pd *ast.ProcedureDeclarationNode) {
	pd.Block.Accept(i)
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
