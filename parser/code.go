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
	emitter        emt.Emitter // emitter that emits the code
	abstractSyntax ast.Block   // abstract syntax tree of the program
}

// Create new code generator with the given emitter and abstract syntax tree.
func newGenerator(emitter emt.Emitter, abstractSyntax ast.Block) *generator {
	return &generator{
		emitter:        emitter,
		abstractSyntax: abstractSyntax,
	}
}

// Generate code for each symbol (not required for code generation).
func (g *generator) VisitSymbol(symbol *ast.Symbol) {
}

// Generate code for each source description (not required for code generation).
func (g *generator) VisitSourceDescription(source *ast.SourceDescription) {
}

func (g *generator) VisitBlock(block *ast.BlockNode) {
}

func (g *generator) VisitLiteral(literal *ast.LiteralNode) {
	g.emitter.Constant(literal.Value)
}

func (g *generator) VisitConstant(constant *ast.ConstantNode) {
	g.emitter.Constant(constant.Symbol.Value)
}

func (g *generator) VisitVariable(variable *ast.VariableNode) {
	g.emitter.LoadVariable(emt.Offset(variable.Symbol.Offset), depth-variable.Symbol.Depth)
}

func (g *generator) VisitUnaryOperation(operation *ast.UnaryOperationNode) {
}

func (g *generator) VisitBinaryOperation(operation *ast.BinaryOperationNode) {
}

func (g *generator) VisitConditionalOperation(operation *ast.ConditionalOperationNode) {
}

func (g *generator) VisitAssignmentStatement(statement *ast.AssignmentStatementNode) {
}

func (g *generator) VisitReadStatement(statement *ast.ReadStatementNode) {
}

func (g *generator) VisitWriteStatement(statement *ast.WriteStatementNode) {
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
