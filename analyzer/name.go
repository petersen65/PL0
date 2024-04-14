// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package analyzer

import (
	"slices"

	ast "github.com/petersen65/PL0/v2/ast"
	cor "github.com/petersen65/PL0/v2/core"
)

// Private implementation of the name analysis.
type nameAnalysis struct {
	abstractSyntax ast.Block        // abstract syntax tree to run name analysis on
	errorHandler   cor.ErrorHandler // error handler that is used to handle errors that occurred during semantic analysis
	tokenHandler   cor.TokenHandler // token handler that manages the tokens of the token stream
}

// Return the public interface of the private name analysis implementation.
func newNameAnalysis(abstractSyntax ast.Block, errorHandler cor.ErrorHandler, tokenHandler cor.TokenHandler) NameAnalysis {
	return &nameAnalysis{
		abstractSyntax: abstractSyntax,
		errorHandler:   errorHandler,
		tokenHandler:   tokenHandler,
	}
}

// Analyze the abstract syntax tree for declaration and use errors and fill in the symbol table.
// Name analysis itself is performing a top down, left to right, and leftmost derivation walk on the abstract syntax tree.
func (a *nameAnalysis) Analyze() {
	if a.abstractSyntax == nil || a.errorHandler == nil || a.tokenHandler == nil {
		panic(cor.NewGeneralError(cor.Analyzer, failureMap, cor.Fatal, invalidNameAnalysisState, nil, nil))
	}

	// validate the declaration of all identifiers
	if err := ast.Walk(a.abstractSyntax, ast.PreOrder, a, nil); err != nil {
		panic(cor.NewGeneralError(cor.Analyzer, failureMap, cor.Fatal, declarationValidationFailed, nil, err))
	}

	// validate the usage of all identifiers
	if err := ast.Walk(a.abstractSyntax, ast.PreOrder, a.tokenHandler, validateIdentifierUsage); err != nil {
		panic(cor.NewGeneralError(cor.Analyzer, failureMap, cor.Fatal, usageValidationFailed, nil, err))
	}

	// determine the closure of all blocks
	if err := ast.Walk(a.abstractSyntax, ast.PreOrder, nil, addVariableToBlockClosure); err != nil {
		panic(cor.NewGeneralError(cor.Analyzer, failureMap, cor.Fatal, closureDeterminationFailed, nil, err))
	}
}

// Walk the block abstract syntax tree.
func (a *nameAnalysis) VisitBlock(bn *ast.BlockNode) {
	// nothing to do because of an external pre-order walk
}

// Enter constant declaration into the symbol table and check for redeclaration.
func (a *nameAnalysis) VisitConstantDeclaration(cd *ast.ConstantDeclarationNode) {
	if cd.Scope.Lookup(cd.Name) != nil {
		a.appendError(identifierAlreadyDeclared, cd.Name, cd.TokenStreamIndex)
	} else {
		cd.Scope.Insert(ast.NewSymbol(cd.Name, ast.Constant, cd))
	}
}

// Enter variable declaration into the symbol table and check for redeclaration.
func (a *nameAnalysis) VisitVariableDeclaration(vd *ast.VariableDeclarationNode) {
	if vd.Scope.Lookup(vd.Name) != nil {
		a.appendError(identifierAlreadyDeclared, vd.Name, vd.TokenStreamIndex)
	} else {
		vd.Scope.Insert(ast.NewSymbol(vd.Name, ast.Variable, vd))
	}
}

// Enter procedure declaration into the symbol table and check for redeclaration.
func (a *nameAnalysis) VisitProcedureDeclaration(pd *ast.ProcedureDeclarationNode) {
	if pd.Scope.Lookup(pd.Name) != nil {
		a.appendError(identifierAlreadyDeclared, pd.Name, pd.TokenStreamIndex)
	} else {
		pd.Scope.Insert(ast.NewSymbol(pd.Name, ast.Procedure, pd))
	}

	pd.Block.Accept(a)
}

// Walk the literal abstract syntax tree.
func (a *nameAnalysis) VisitLiteral(ln *ast.LiteralNode) {
	// nothing to do
}

// Check if the used identifier is declared and if it is used in the correct context.
func (a *nameAnalysis) VisitIdentifierUse(iu *ast.IdentifierUseNode) {
	if symbol := iu.Scope.Lookup(iu.Name); symbol == nil {
		a.appendError(identifierNotFound, iu.Name, iu.TokenStreamIndex)
	} else {
		switch symbol.Kind {
		case ast.Constant:
			// make the identifier a constant because its symbol is a constant and it is used in a constant context
			if iu.Context&ast.Constant != 0 {
				iu.Context = ast.Constant

				// add the constant usage to the constant declaration
				symbol.Declaration.(*ast.ConstantDeclarationNode).Usage =
					append(symbol.Declaration.(*ast.ConstantDeclarationNode).Usage, iu)
			} else {
				a.appendError(expectedConstantIdentifier, iu.Name, iu.TokenStreamIndex)
			}

		case ast.Variable:
			// make the identifier a variable because its symbol is a variable and it is used in a variable context
			if iu.Context&ast.Variable != 0 {
				iu.Context = ast.Variable

				// add the variable usage to the variable declaration
				symbol.Declaration.(*ast.VariableDeclarationNode).Usage =
					append(symbol.Declaration.(*ast.VariableDeclarationNode).Usage, iu)
			} else {
				a.appendError(expectedVariableIdentifier, iu.Name, iu.TokenStreamIndex)
			}

		case ast.Procedure:
			// make the identifier a procedure because its symbol is a procedure and it is used in a procedure context
			if iu.Context&ast.Procedure != 0 {
				iu.Context = ast.Procedure

				// add the procedure usage to the procedure declaration
				symbol.Declaration.(*ast.ProcedureDeclarationNode).Usage =
					append(symbol.Declaration.(*ast.ProcedureDeclarationNode).Usage, iu)
			} else {
				a.appendError(expectedProcedureIdentifier, iu.Name, iu.TokenStreamIndex)
			}

		default:
			panic(cor.NewGeneralError(cor.Analyzer, failureMap, cor.Fatal, unknownSymbolKind, nil, nil))
		}
	}
}

// Walk the unary operation abstract syntax tree.
func (a *nameAnalysis) VisitUnaryOperation(uo *ast.UnaryOperationNode) {
	// set the usage mode bit to read for all constants and variables in the expression
	ast.Walk(uo.Operand, ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Walk the binary operation abstract syntax tree.
func (a *nameAnalysis) VisitBinaryOperation(bo *ast.BinaryOperationNode) {
	// set the usage mode bit to read for all constants and variables in the left and right operand
	ast.Walk(bo.Left, ast.PreOrder, nil, setConstantVariableUsageAsRead)
	ast.Walk(bo.Right, ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Walk the conditional operation abstract syntax tree.
func (a *nameAnalysis) VisitConditionalOperation(co *ast.ConditionalOperationNode) {
	// set the usage mode bit to read for all constants and variables in the left and right operand
	ast.Walk(co.Left, ast.PreOrder, nil, setConstantVariableUsageAsRead)
	ast.Walk(co.Right, ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Walk the assignment statement abstract syntax tree.
func (a *nameAnalysis) VisitAssignmentStatement(as *ast.AssignmentStatementNode) {
	// set the usage mode bit to write for the variable that is assigned to
	as.Variable.(*ast.IdentifierUseNode).Use |= ast.Write
}

// Walk the read statement abstract syntax tree.
func (a *nameAnalysis) VisitReadStatement(rs *ast.ReadStatementNode) {
	// set the usage mode bit to write for the variable that is read into
	rs.Variable.(*ast.IdentifierUseNode).Use |= ast.Write
}

// Walk the write statement abstract syntax tree.
func (a *nameAnalysis) VisitWriteStatement(ws *ast.WriteStatementNode) {
	// set the usage mode bit to read for all constants and variables in the expression
	ast.Walk(ws.Expression, ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Walk the call statement abstract syntax tree.
func (a *nameAnalysis) VisitCallStatement(cs *ast.CallStatementNode) {
	// set the usage mode bit to execute for the procedure that is called
	cs.Procedure.(*ast.IdentifierUseNode).Use |= ast.Execute
}

// Walk the if statement abstract syntax tree.
func (a *nameAnalysis) VisitIfStatement(is *ast.IfStatementNode) {
	// set the usage mode bit to read for all constants and variables in the condition
	ast.Walk(is.Condition, ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Walk the while statement abstract syntax tree.
func (a *nameAnalysis) VisitWhileStatement(ws *ast.WhileStatementNode) {
	// set the usage mode bit to read for all constants and variables in the condition
	ast.Walk(ws.Condition, ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Walk the compound statement abstract syntax tree.
func (a *nameAnalysis) VisitCompoundStatement(cs *ast.CompoundStatementNode) {
	// nothing to do because of an external pre-order walk
}

// Append analyzer error to the token handler.
func (a *nameAnalysis) appendError(code cor.Failure, value any, index int) {
	a.tokenHandler.AppendError(a.tokenHandler.NewErrorOnIndex(cor.Error, code, value, index))
}

// For all occurrences of a constant or variable usage, set the usage mode bit to read.
func setConstantVariableUsageAsRead(node ast.Node, _ any) {
	if iu, ok := node.(*ast.IdentifierUseNode); ok {
		if symbol := iu.Scope.Lookup(iu.Name); symbol != nil {
			if symbol.Kind == ast.Constant || symbol.Kind == ast.Variable {
				iu.Use |= ast.Read
			}
		}
	}
}

// Visit identifier declarations and check if they are used. If not, append a warning to the token handler.
func validateIdentifierUsage(node ast.Node, tokenHandler any) {
	th := tokenHandler.(cor.TokenHandler)

	switch d := node.(type) {
	case *ast.ConstantDeclarationNode:
		if len(d.Usage) == 0 {
			th.AppendError(th.NewErrorOnIndex(cor.Warning, unusedConstantIdentifier, d.Name, d.TokenStreamIndex))
		}

	case *ast.VariableDeclarationNode:
		if len(d.Usage) == 0 {
			th.AppendError(th.NewErrorOnIndex(cor.Warning, unusedVariableIdentifier, d.Name, d.TokenStreamIndex))
		}

	case *ast.ProcedureDeclarationNode:
		if len(d.Usage) == 0 {
			th.AppendError(th.NewErrorOnIndex(cor.Warning, unusedProcedureIdentifier, d.Name, d.TokenStreamIndex))
		}
	}
}

// Add all variables that are used in a block to the closure of the block if they are declared in an outer block.
func addVariableToBlockClosure(node ast.Node, _ any) {
	if iu, ok := node.(*ast.IdentifierUseNode); ok {
		if symbol := iu.Scope.Lookup(iu.Name); symbol != nil {
			if symbol.Kind == ast.Variable {
				// determine the block where the variable is declared
				declarationBlock := ast.SearchBlock(ast.CurrentBlock, symbol.Declaration)

				// determine the block where the variable is used
				useBlock := ast.SearchBlock(ast.CurrentBlock, iu)

				// add the variable to the closure of the block where it is used if it is declared in an outer block
				if useBlock.Depth-declarationBlock.Depth > 0 {
					// check if the variable is already in the closure
					if !slices.ContainsFunc(useBlock.Closure, func(d ast.Declaration) bool {
						return d == symbol.Declaration
					}) {
						useBlock.Closure = append(useBlock.Closure, symbol.Declaration)
					}
				}
			}
		}
	}
}
