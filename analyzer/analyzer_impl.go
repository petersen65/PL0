// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package analyzer

import (
	"slices"

	ast "github.com/petersen65/pl0/v3/ast"
	eh "github.com/petersen65/pl0/v3/errors"
	sym "github.com/petersen65/pl0/v3/symbol"
	tok "github.com/petersen65/pl0/v3/token"
	ts "github.com/petersen65/pl0/v3/typesystem"
)

// Implementation of the semantic analyzer.
type semanticAnalyzer struct {
	abstractSyntax ast.Block        // abstract syntax tree to run semantic analysis on
	errorHandler   eh.ErrorHandler  // error handler that is used to handle errors that occurred during semantic analysis
	tokenHandler   tok.TokenHandler // token handler that manages the tokens of the token stream
}

// Return the interface of the semantic analyzer implementation.
func newAnalyzer(abstractSyntax ast.Block, errorHandler eh.ErrorHandler, tokenHandler tok.TokenHandler) Analyzer {
	return &semanticAnalyzer{
		abstractSyntax: abstractSyntax,
		errorHandler:   errorHandler,
		tokenHandler:   tokenHandler,
	}
}

// Analyze the abstract syntax tree for declaration and use errors and fill in symbols into into the scope of blocks.
// Name analyzer itself is performing a top down, left to right, and leftmost derivation walk on the abstract syntax tree.
func (a *semanticAnalyzer) Analyze() {
	// get the root block of the abstract syntax tree to insert built-in data types into its scope
	rootBlock := a.abstractSyntax.RootBlock()

	// insert built-in data types into the scope of the root block
	int64TypeName := ts.Integer64.String()
	int64Type := ts.NewSimpleTypeDescriptor(int64TypeName, ts.Integer64)
	int64Symbol := sym.NewSymbol(int64TypeName, sym.DataTypeEntry, int64Type, nil)
	rootBlock.Insert(int64TypeName, int64Symbol)

	if a.abstractSyntax == nil || a.errorHandler == nil || a.tokenHandler == nil {
		panic(eh.NewGeneralError(eh.Analyzer, failureMap, eh.Fatal, invalidNameAnalysisState, nil, nil))
	}

	// validate the declaration of all identifiers
	if err := ast.Walk(a.abstractSyntax, ast.PreOrder, a, nil); err != nil {
		panic(eh.NewGeneralError(eh.Analyzer, failureMap, eh.Fatal, declarationValidationFailed, nil, err))
	}

	// validate the usage of all identifiers
	if err := ast.Walk(a.abstractSyntax, ast.PreOrder, a.tokenHandler, validateIdentifierUsage); err != nil {
		panic(eh.NewGeneralError(eh.Analyzer, failureMap, eh.Fatal, usageValidationFailed, nil, err))
	}

	// determine the closure of all blocks
	if err := ast.Walk(a.abstractSyntax, ast.PreOrder, nil, addVariableToBlockClosure); err != nil {
		panic(eh.NewGeneralError(eh.Analyzer, failureMap, eh.Fatal, closureDeterminationFailed, nil, err))
	}
}

// Walk the block abstract syntax tree.
func (a *semanticAnalyzer) VisitBlock(b ast.Block) {
	// nothing to do because of an external pre-order walk
}

// Insert the symbol for a constant declaration into the current block's scope.
func (a *semanticAnalyzer) VisitConstantDeclaration(cd ast.ConstantDeclaration) {
	cb := cd.CurrentBlock()             // current bloc
	s := cb.Lookup(cd.Name())           // constant symbol
	dts := cb.Lookup(cd.DataTypeName()) // constant data type symbol

	// in the case of no errors, insert the constant symbol into the current block's scope
	if s != nil {
		a.appendError(identifierAlreadyDeclared, cd.Name(), cd.Index())
	} else if dts == nil || dts.Kind != sym.DataTypeEntry {
		a.appendError(constantDataTypeNotFound, cd.DataTypeName(), cd.Index())
	} else {
		symbol := sym.NewSymbol(cd.Name(), sym.ConstantEntry, dts.DataType, cd.Value())
		cb.Insert(cd.Name(), symbol)
		cd.SetSymbol(symbol)
	}
}

// Insert the symbol for a variable declaration into the current block's scope.
func (a *semanticAnalyzer) VisitVariableDeclaration(vd ast.VariableDeclaration) {
	cb := vd.CurrentBlock()             // current block
	s := cb.Lookup(vd.Name())           // variable symbol
	dts := cb.Lookup(vd.DataTypeName()) // variable data type symbol

	// in the case of no errors, insert the variable symbol into the current block's scope
	if s != nil {
		a.appendError(identifierAlreadyDeclared, vd.Name(), vd.Index())
	} else if dts == nil || dts.Kind != sym.DataTypeEntry {
		a.appendError(variableDataTypeNotFound, vd.DataTypeName(), vd.Index())
	} else {
		symbol := sym.NewSymbol(vd.Name(), sym.VariableEntry, dts.DataType, nil)
		cb.Insert(vd.Name(), symbol)
		vd.SetSymbol(symbol)
	}
}

// Insert the symbol for a procedure declaration into the current block's scope.
func (a *semanticAnalyzer) VisitProcedureDeclaration(pd ast.ProcedureDeclaration) {
	cb := pd.CurrentBlock()                               // current block
	s := cb.Lookup(pd.Name)                               // procedure symbol
	dt := ts.NewFunctionTypeDescriptor(pd.Name, nil, nil) // procedure data type

	// in the case of no errors, insert the procedure symbol into the current block's scope
	if s != nil {
		a.appendError(identifierAlreadyDeclared, pd.Name, pd.TokenStreamIndex)
	} else {
		cb.Insert(pd.Name, sym.NewSymbol(pd.Name, sym.ProcedureEntry, dt, pd))
	}

	pd.Block.Accept(a)
}

// Walk the literal abstract syntax tree.
func (a *semanticAnalyzer) VisitLiteral(ln *ast.LiteralNode) {
	// nothing to do
}

// Check if the used identifier is declared and if it is used in the correct context.
func (a *semanticAnalyzer) VisitIdentifierUse(iu *ast.IdentifierUseNode) {
	if symbol := iu.CurrentBlock().Lookup(iu.Name); symbol == nil {
		a.appendError(identifierNotFound, iu.Name, iu.TokenStreamIndex)
	} else {
		switch symbol.Kind {
		case sym.ConstantEntry:
			// make the identifier a constant because its symbol is a constant and it is used as a constant kind
			if iu.IdentifierKind&ast.Constant != 0 {
				iu.IdentifierKind = ast.Constant

				// add the constant usage to the constant declaration
				symbol.Type.(*ast.ConstantDeclarationNode).IdentifierUsage =
					append(symbol.Type.(*ast.ConstantDeclarationNode).IdentifierUsage, iu)
			} else {
				a.appendError(expectedConstantIdentifier, iu.Name, iu.TokenStreamIndex)
			}

		case sym.VariableEntry:
			// make the identifier a variable because its symbol is a variable and it is used in a variable context
			if iu.Context&sym.VariableEntry != 0 {
				iu.Context = sym.VariableEntry

				// add the variable usage to the variable declaration
				symbol.Type.(*ast.VariableDeclarationNode).IdentifierUsage =
					append(symbol.Type.(*ast.VariableDeclarationNode).IdentifierUsage, iu)
			} else {
				a.appendError(expectedVariableIdentifier, iu.Name, iu.TokenStreamIndex)
			}

		case sym.ProcedureEntry:
			// make the identifier a procedure because its symbol is a procedure and it is used in a procedure context
			if iu.Context&sym.ProcedureEntry != 0 {
				iu.Context = sym.ProcedureEntry

				// add the procedure usage to the procedure declaration
				symbol.Type.(*ast.ProcedureDeclarationNode).IdentifierUsage =
					append(symbol.Type.(*ast.ProcedureDeclarationNode).IdentifierUsage, iu)
			} else {
				a.appendError(expectedProcedureIdentifier, iu.Name, iu.TokenStreamIndex)
			}

		default:
			panic(eh.NewGeneralError(eh.Analyzer, failureMap, eh.Fatal, unknownSymbolKind, nil, nil))
		}
	}
}

// Walk the unary operation abstract syntax tree.
func (a *semanticAnalyzer) VisitUnaryOperation(uo *ast.UnaryOperationNode) {
	// set the usage mode bit to read for all constants and variables in the expression
	ast.Walk(uo.Operand, ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Walk the binary operation abstract syntax tree.
func (a *semanticAnalyzer) VisitBinaryOperation(bo *ast.BinaryOperationNode) {
	// set the usage mode bit to read for all constants and variables in the left and right operand
	ast.Walk(bo.Left, ast.PreOrder, nil, setConstantVariableUsageAsRead)
	ast.Walk(bo.Right, ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Walk the comparison operation abstract syntax tree.
func (a *semanticAnalyzer) VisitComparisonOperation(co *ast.ComparisonOperationNode) {
	// set the usage mode bit to read for all constants and variables in the left and right operand
	ast.Walk(co.Left, ast.PreOrder, nil, setConstantVariableUsageAsRead)
	ast.Walk(co.Right, ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Walk the assignment statement abstract syntax tree.
func (a *semanticAnalyzer) VisitAssignmentStatement(as *ast.AssignmentStatementNode) {
	// set the usage mode bit to write for the variable that is assigned to
	as.Variable.(*ast.IdentifierUseNode).UsageMode |= ast.Write
}

// Walk the read statement abstract syntax tree.
func (a *semanticAnalyzer) VisitReadStatement(rs *ast.ReadStatementNode) {
	// set the usage mode bit to write for the variable that is read into
	rs.Variable.(*ast.IdentifierUseNode).UsageMode |= ast.Write
}

// Walk the write statement abstract syntax tree.
func (a *semanticAnalyzer) VisitWriteStatement(ws *ast.WriteStatementNode) {
	// set the usage mode bit to read for all constants and variables in the expression
	ast.Walk(ws.Expression, ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Walk the call statement abstract syntax tree.
func (a *semanticAnalyzer) VisitCallStatement(cs *ast.CallStatementNode) {
	// set the usage mode bit to execute for the procedure that is called
	cs.Procedure.(*ast.IdentifierUseNode).UsageMode |= ast.Execute
}

// Walk the if statement abstract syntax tree.
func (a *semanticAnalyzer) VisitIfStatement(is *ast.IfStatementNode) {
	// set the usage mode bit to read for all constants and variables in the condition
	ast.Walk(is.Condition, ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Walk the while statement abstract syntax tree.
func (a *semanticAnalyzer) VisitWhileStatement(ws *ast.WhileStatementNode) {
	// set the usage mode bit to read for all constants and variables in the condition
	ast.Walk(ws.Condition, ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Walk the compound statement abstract syntax tree.
func (a *semanticAnalyzer) VisitCompoundStatement(cs *ast.CompoundStatementNode) {
	// nothing to do because of an external pre-order walk
}

// Append analyzer error to the token handler.
func (a *semanticAnalyzer) appendError(code eh.Failure, value any, index int) {
	a.tokenHandler.AppendError(a.tokenHandler.NewErrorOnIndex(eh.Error, code, value, index))
}

// For all occurrences of a constant or variable usage, set the usage mode bit to read.
func setConstantVariableUsageAsRead(node ast.Node, _ any) {
	if iu, ok := node.(*ast.IdentifierUseNode); ok {
		if symbol := iu.CurrentBlock().Lookup(iu.Name); symbol != nil {
			if symbol.Kind == sym.ConstantEntry || symbol.Kind == sym.VariableEntry {
				iu.UsageMode |= ast.Read
			}
		}
	}
}

// Visit identifier declarations and check if they are used. If not, append a warning to the token handler.
func validateIdentifierUsage(node ast.Node, tokenHandler any) {
	th := tokenHandler.(tok.TokenHandler)

	switch d := node.(type) {
	case *ast.ConstantDeclarationNode:
		if len(d.IdentifierUsage) == 0 {
			th.AppendError(th.NewErrorOnIndex(eh.Warning, unusedConstantIdentifier, d.Name, d.TokenStreamIndex))
		}

	case *ast.VariableDeclarationNode:
		if len(d.IdentifierUsage) == 0 {
			th.AppendError(th.NewErrorOnIndex(eh.Warning, unusedVariableIdentifier, d.Name, d.TokenStreamIndex))
		}

	case *ast.ProcedureDeclarationNode:
		if len(d.IdentifierUsage) == 0 {
			th.AppendError(th.NewErrorOnIndex(eh.Warning, unusedProcedureIdentifier, d.Name, d.TokenStreamIndex))
		}
	}
}

// Add all variables that are used in a block to the closure of the block if they are declared in an outer block.
func addVariableToBlockClosure(node ast.Node, _ any) {
	if iu, ok := node.(*ast.IdentifierUseNode); ok {
		if symbol := iu.CurrentBlock().Lookup(iu.Name); symbol != nil {
			if symbol.Kind == sym.VariableEntry {
				// determine the block where the variable is declared
				declarationBlock := ast.SearchBlock(symbol.Type, ast.CurrentBlock)

				// determine the block where the variable is used
				useBlock := ast.SearchBlock(iu, ast.CurrentBlock)

				// add the variable to the closure of the block where it is used if it is declared in an outer block
				if useBlock.Depth-declarationBlock.Depth > 0 {
					// check if the variable is already in the closure
					if !slices.ContainsFunc(useBlock.Closure, func(d ast.Declaration) bool {
						return d == symbol.Type
					}) {
						useBlock.Closure = append(useBlock.Closure, symbol.Type)
					}
				}
			}
		}
	}
}
