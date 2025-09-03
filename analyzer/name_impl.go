// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package analyzer

import ast "github.com/petersen65/pl0/v3/ast"

// Implementation of the name analysis phase of the semantic analyzer.
type nameAnalyzer struct {
	abstractSyntax ast.Block // abstract syntax tree to run semantic analysis on
}

// Return the interface of the name analyzer implementation.
func newNameAnalyzer(abstractSyntax ast.Block) ast.Visitor {
	return &nameAnalyzer{
		abstractSyntax: abstractSyntax,
	}
}

// Walk the block abstract syntax tree.
func (a *nameAnalyzer) VisitBlock(b ast.Block) {}

// Enter the constant declaration as a symbol into the block's scope and check for redeclaration.
func (a *nameAnalyzer) VisitConstantDeclaration(cd ast.ConstantDeclaration) {}

// Enter the variable declaration as a symbol into the block's scope and check for redeclaration.
func (a *nameAnalyzer) VisitVariableDeclaration(vd ast.VariableDeclaration) {}

// Enter the function declaration as a symbol into the block's scope and check for redeclaration.
func (a *nameAnalyzer) VisitFunctionDeclaration(fd ast.FunctionDeclaration) {}

// Walk the literal abstract syntax tree.
func (a *nameAnalyzer) VisitLiteral(ln *ast.LiteralNode) {}

// Check if the used identifier is declared and if it is used in the correct context.
func (a *nameAnalyzer) VisitIdentifierUse(iu ast.IdentifierUse) {}

// Walk the unary operation abstract syntax tree.
func (a *nameAnalyzer) VisitUnaryOperation(uo *ast.UnaryOperationNode) {}

// Walk the binary operation abstract syntax tree.
func (a *nameAnalyzer) VisitBinaryOperation(bo *ast.BinaryOperationNode) {}

// Walk the comparison operation abstract syntax tree.
func (a *nameAnalyzer) VisitComparisonOperation(co *ast.ComparisonOperationNode) {}

// Walk the assignment statement abstract syntax tree.
func (a *nameAnalyzer) VisitAssignmentStatement(as *ast.AssignmentStatementNode) {}

// Walk the read statement abstract syntax tree.
func (a *nameAnalyzer) VisitReadStatement(rs *ast.ReadStatementNode) {}

// Walk the write statement abstract syntax tree.
func (a *nameAnalyzer) VisitWriteStatement(ws *ast.WriteStatementNode) {}

// Walk the call statement abstract syntax tree.
func (a *nameAnalyzer) VisitCallStatement(cs *ast.CallStatementNode) {}

// Walk the if statement abstract syntax tree.
func (a *nameAnalyzer) VisitIfStatement(is *ast.IfStatementNode) {}

// Walk the while statement abstract syntax tree.
func (a *nameAnalyzer) VisitWhileStatement(ws *ast.WhileStatementNode) {}

// Walk the compound statement abstract syntax tree.
func (a *nameAnalyzer) VisitCompoundStatement(cs *ast.CompoundStatementNode) {}
