// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package parser

import (
	ast "github.com/petersen65/PL0/ast"
	emt "github.com/petersen65/PL0/emitter"
)

// Generator is the parser pass for code generation. It implements the Visitor interface to traverse the AST and generate code.
type generator struct {
	emitter emt.Emitter // emitter that emits the code
}

// Create new code generator with the given emitter and abstract syntax tree.
func newGenerator() *generator {
	return &generator{emitter: emt.NewEmitter()}
}

// Generate code for each symbol (not required for code generation).
func (g *generator) VisitSymbol(symbol *ast.Symbol) {
}

// Generate code for each source description (not required for code generation).
func (g *generator) VisitSourceDescription(source *ast.SourceDescription) {
}

func (g *generator) VisitBlock(block *ast.BlockNode) {
}

func (g *generator) VisitLiteral(l *ast.LiteralNode) {
	g.emitter.Constant(l.Value)
}

func (g *generator) VisitConstant(cr *ast.ConstantReferenceNode) {
	g.emitter.Constant(cr.Symbol.Value)
}

func (g *generator) VisitVariable(vr *ast.VariableReferenceNode) {
	referenceDeclarationDepth := ast.SearchBlock(ast.CurrentBlock, vr).Depth
	g.emitter.LoadVariable(emt.Offset(vr.Symbol.Offset), referenceDeclarationDepth-vr.Symbol.Depth)
}

func (g *generator) VisitUnaryOperation(uo *ast.UnaryOperationNode) {
	switch uo.Operation {
	case ast.Odd:
		g.emitter.Odd()

	case ast.Negate:
		g.emitter.Negate()
	}
}

func (g *generator) VisitBinaryOperation(bo *ast.BinaryOperationNode) {
	switch bo.Operation {
	case ast.Plus:
		g.emitter.Add()

	case ast.Minus:
		g.emitter.Subtract()

	case ast.Times:
		g.emitter.Multiply()

	case ast.Divide:
		g.emitter.Divide()
	}
}

func (g *generator) VisitConditionalOperation(co *ast.ConditionalOperationNode) {
	switch co.Operation {
	case ast.Equal:
		g.emitter.Equal()

	case ast.NotEqual:
		g.emitter.NotEqual()

	case ast.Less:
		g.emitter.Less()

	case ast.LessEqual:
		g.emitter.LessEqual()

	case ast.Greater:
		g.emitter.Greater()

	case ast.GreaterEqual:
		g.emitter.GreaterEqual()
	}
}

func (g *generator) VisitAssignmentStatement(statement *ast.AssignmentStatementNode) {
}

func (g *generator) VisitReadStatement(rs *ast.ReadStatementNode) {
	referenceDeclarationDepth := ast.SearchBlock(ast.CurrentBlock, rs).Depth
	g.emitter.System(emt.Read)
	g.emitter.StoreVariable(emt.Offset(rs.Symbol.Offset), referenceDeclarationDepth-rs.Symbol.Depth)
}

func (g *generator) VisitWriteStatement(ws *ast.WriteStatementNode) {
	g.emitter.System(emt.Write)
}

func (g *generator) VisitCallStatement(statement *ast.CallStatementNode) {
}

func (g *generator) VisitIfStatement(statement *ast.IfStatementNode) {
}

func (g *generator) VisitWhileStatement(statement *ast.WhileStatementNode) {
}

func (g *generator) VisitCompoundStatement(statement *ast.CompoundStatementNode) {
}
