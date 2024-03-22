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

// Analyze the abstract syntax tree for declaration and use errors and fill in the symbol table.
// Declaration analysis itself is performing a top down, left to right, and leftmost derivation walk on the abstract syntax tree.
func (a *declarationAnalysis) Analyze() error {
	if a.abstractSyntax == nil || a.errorHandler == nil || a.tokenHandler == nil {
		return tok.NewGeneralError(tok.Analyzer, failureMap, tok.Error, invalidDeclarationAnalysisState, nil)
	}

	// an existing error handler can have errors from other compiler components
	startErrorCount := a.errorHandler.Count()

	// start the declaration analysis by visiting the abstract syntax tree in pre-order
	ast.Walk(a.abstractSyntax, ast.PreOrder, a, nil)

	// number of errors that occurred during declaration analysis
	analyzerErrorCount := a.errorHandler.Count() - startErrorCount

	if analyzerErrorCount == 1 {
		return tok.NewGeneralError(tok.Analyzer, failureMap, tok.Error, declarationAnalysisError, nil)
	} else if analyzerErrorCount > 1 {
		return tok.NewGeneralError(tok.Analyzer, failureMap, tok.Error, declarationAnalysisErrors, analyzerErrorCount)
	}

	return nil
}

// Walk the block abstract syntax tree.
func (a *declarationAnalysis) VisitBlock(bn *ast.BlockNode) {
	// nothing to do because of an external pre-order walk
}

// Enter constant declaration into the symbol table and check for redeclaration.
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

// Enter variable declaration into the symbol table and check for redeclaration.
func (a *declarationAnalysis) VisitVariableDeclaration(vd *ast.VariableDeclarationNode) {
	if vd.Scope.Lookup(vd.Name) != nil {
		a.appendError(identifierAlreadyDeclared, vd.Name, vd.TokenStreamIndex)
	} else {
		vd.Scope.Insert(&ast.Symbol{
			Name:        vd.Name,
			Kind:        ast.Variable,
			Declaration: vd,
		})
	}
}

// Enter procedure declaration into the symbol table and check for redeclaration.
func (a *declarationAnalysis) VisitProcedureDeclaration(pd *ast.ProcedureDeclarationNode) {
	if pd.Scope.Lookup(pd.Name) != nil {
		a.appendError(identifierAlreadyDeclared, pd.Name, pd.TokenStreamIndex)
	} else {
		pd.Scope.Insert(&ast.Symbol{
			Name:        pd.Name,
			Kind:        ast.Procedure,
			Declaration: pd,
		})
	}

	pd.Block.Accept(a)
}

// Walk the literal abstract syntax tree.
func (a *declarationAnalysis) VisitLiteral(ln *ast.LiteralNode) {
	// nothing to do
}

// Check if the used identifier is declared and if it is used in the correct context.
func (a *declarationAnalysis) VisitIdentifierUse(iu *ast.IdentifierUseNode) {
	if symbol := iu.Scope.Lookup(iu.Name); symbol == nil {
		a.appendError(identifierNotFound, iu.Name, iu.TokenStreamIndex)
	} else {
		switch symbol.Kind {
		case ast.Constant:
			// make the identifier a constant because its symbol is a constant and it is used in a constant context
			if iu.Context&ast.Constant != 0 {
				iu.Context = ast.Constant
			} else {
				a.appendError(expectedConstantIdentifier, iu.Name, iu.TokenStreamIndex)
			}

		case ast.Variable:
			// make the identifier a variable because its symbol is a variable and it is used in a variable context
			if iu.Context&ast.Variable != 0 {
				iu.Context = ast.Variable
			} else {
				a.appendError(expectedVariableIdentifier, iu.Name, iu.TokenStreamIndex)
			}

		case ast.Procedure:
			// make the identifier a procedure because its symbol is a procedure and it is used in a procedure context
			if iu.Context&ast.Procedure != 0 {
				iu.Context = ast.Procedure
			} else {
				a.appendError(expectedProcedureIdentifier, iu.Name, iu.TokenStreamIndex)
			}

		default:
			panic("declaration analysis error: unknown symbol kind")
		}
	}
}

// Walk the unary operation abstract syntax tree.
func (a *declarationAnalysis) VisitUnaryOperation(uo *ast.UnaryOperationNode) {
	// set the usage mode bit to read for all constants and variables in the expression
	ast.Walk(uo.Operand, ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Walk the binary operation abstract syntax tree.
func (a *declarationAnalysis) VisitBinaryOperation(bo *ast.BinaryOperationNode) {
	// set the usage mode bit to read for all constants and variables in the left and right operand
	ast.Walk(bo.Left, ast.PreOrder, nil, setConstantVariableUsageAsRead)
	ast.Walk(bo.Right, ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Walk the conditional operation abstract syntax tree.
func (a *declarationAnalysis) VisitConditionalOperation(co *ast.ConditionalOperationNode) {
	// set the usage mode bit to read for all constants and variables in the left and right operand
	ast.Walk(co.Left, ast.PreOrder, nil, setConstantVariableUsageAsRead)
	ast.Walk(co.Right, ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Walk the assignment statement abstract syntax tree.
func (a *declarationAnalysis) VisitAssignmentStatement(as *ast.AssignmentStatementNode) {
	// set the usage mode bit to write for the variable that is assigned to
	as.Variable.(*ast.IdentifierUseNode).Use |= ast.Write
}

// Walk the read statement abstract syntax tree.
func (a *declarationAnalysis) VisitReadStatement(rs *ast.ReadStatementNode) {
	// set the usage mode bit to write for the variable that is read into
	rs.Variable.(*ast.IdentifierUseNode).Use |= ast.Write
}

// Walk the write statement abstract syntax tree.
func (a *declarationAnalysis) VisitWriteStatement(ws *ast.WriteStatementNode) {
	// set the usage mode bit to read for all constants and variables in the expression
	ast.Walk(ws.Expression, ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Walk the call statement abstract syntax tree.
func (a *declarationAnalysis) VisitCallStatement(cs *ast.CallStatementNode) {
	// set the usage mode bit to execute for the procedure that is called
	cs.Procedure.(*ast.IdentifierUseNode).Use |= ast.Execute
}

// Walk the if statement abstract syntax tree.
func (a *declarationAnalysis) VisitIfStatement(is *ast.IfStatementNode) {
	// set the usage mode bit to read for all constants and variables in the condition
	ast.Walk(is.Condition, ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Walk the while statement abstract syntax tree.
func (a *declarationAnalysis) VisitWhileStatement(ws *ast.WhileStatementNode) {
	// set the usage mode bit to read for all constants and variables in the condition
	ast.Walk(ws.Condition, ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Walk the compound statement abstract syntax tree.
func (a *declarationAnalysis) VisitCompoundStatement(cs *ast.CompoundStatementNode) {
	// nothing to do because of an external pre-order walk
}

// For all occurrences of a constant or variable usage, set the usage mode bit to read.
func setConstantVariableUsageAsRead(node ast.Node, visitor any) {
	if iu, ok := node.(*ast.IdentifierUseNode); ok {
		if symbol := iu.Scope.Lookup(iu.Name); symbol != nil {
			if symbol.Kind == ast.Constant || symbol.Kind == ast.Variable {
				iu.Use |= ast.Read
			}
		}
	}
}

// Append analyzer error to the token handler.
func (a *declarationAnalysis) appendError(code tok.Failure, value any, index int) {
	a.tokenHandler.AppendError(a.tokenHandler.NewErrorOnIndex(code, value, index))
}
