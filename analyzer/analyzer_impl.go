// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package analyzer

import (
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

// Map parameter passing modes from the abstract syntax tree to the type system.
var passingModeMap = map[ast.PassingMode]ts.ParameterPassingMode{
	ast.CallByValue:          ts.CallByValue,
	ast.CallByReference:      ts.CallByReference,
	ast.CallByConstReference: ts.CallByConstReference,
	ast.OutputParameter:      ts.OutputParameter,
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
	int64Type := ts.NewSimpleTypeDescriptor(ts.Integer64)
	rootBlock.Insert(int64Type.Name(), sym.NewSymbol(int64Type.Name(), sym.DataTypeEntry, int64Type, nil))

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

// Insert the symbol for a function declaration into the current block's scope.
func (a *semanticAnalyzer) VisitFunctionDeclaration(fd ast.FunctionDeclaration) {
	cb := fd.CurrentBlock()   // current block
	s := cb.Lookup(fd.Name()) // function symbol

	var symbolEntry sym.EntryKind = sym.ProcedureEntry // function or procedure symbol entry kind
	var parameters = make([]*ts.FunctionParameter, 0)  // function parameters
	var returnType ts.TypeDescriptor                   // function return type or nil

	// handle the return data type of a function
	if fd.IsFunction() {
		symbolEntry = sym.FunctionEntry

		if rts := cb.Lookup(fd.ReturnTypeName()); rts == nil || rts.Kind != sym.DataTypeEntry {
			a.appendError(functionReturnTypeNotFound, fd.ReturnTypeName(), fd.Index())
		} else {
			returnType = rts.DataType
		}
	}

	// translate function parameters from the function declaration into data type representations of the type system
	for _, parameter := range fd.Parameters() {
		if pts := cb.Lookup(parameter.DataTypeName); pts == nil || pts.Kind != sym.DataTypeEntry {
			if fd.IsProcedure() {
				a.appendError(procedureParameterTypeNotFound, parameter.DataTypeName, fd.Index())
			} else {
				a.appendError(functionParameterTypeNotFound, parameter.DataTypeName, fd.Index())
			}
		} else {
			parameters = append(parameters, ts.NewFunctionParameter(parameter.Name, pts.DataType, passingModeMap[parameter.PassingMode]))
		}
	}

	// append an error if the function was already declared
	if s != nil {
		a.appendError(identifierAlreadyDeclared, fd.Name(), fd.Index())
	} else {
		// create a data type for a function or procedure with a predefined list of parameters and an optional return type
		functiontype := ts.NewFunctionTypeDescriptor(parameters, returnType)

		// if the function data type is not found, insert it into the current block's scope
		if fts := cb.Lookup(functiontype.Name()); fts != nil {
			functiontype = fts.DataType
		} else {
			cb.Insert(functiontype.Name(), sym.NewSymbol(functiontype.Name(), sym.DataTypeEntry, functiontype, nil))
		}

		// insert the function symbol into the current block's scope
		symbol := sym.NewSymbol(fd.Name(), symbolEntry, functiontype, nil)
		cb.Insert(fd.Name(), symbol)
		fd.SetSymbol(symbol)
	}

	fd.Block().Accept(a)
}

// Walk the literal abstract syntax tree.
func (a *semanticAnalyzer) VisitLiteral(ln *ast.LiteralNode) {
	// nothing to do
}

// Check if the used identifier is declared and if it is used as the expected kind of identifier.
func (a *semanticAnalyzer) VisitIdentifierUse(iu ast.IdentifierUse) {
	if symbol := iu.CurrentBlock().Lookup(iu.Name()); symbol == nil {
		a.appendError(identifierNotFound, iu.Name(), iu.Index())
	} else {
		switch symbol.Kind {
		case sym.ConstantEntry:
			// make the identifier a constant because its symbol is a constant and it is used as a constant kind
			if iu.Context()&ast.Constant != 0 {
				iu.SetContext(ast.Constant)

				// add the constant usage to the constant declaration
				if declaration := ast.SearchDeclaration(iu, symbol); declaration != nil {
					declaration.AddUsage(iu)
				}
			} else {
				a.appendError(expectedConstantIdentifier, iu.Name(), iu.Index())
			}

		case sym.VariableEntry:
			// make the identifier a variable because its symbol is a variable and it is used as a variable kind
			if iu.Context()&ast.Variable != 0 {
				iu.SetContext(ast.Variable)

				// add the variable usage to the variable declaration
				if declaration := ast.SearchDeclaration(iu, symbol); declaration != nil {
					declaration.AddUsage(iu)
				}
			} else {
				a.appendError(expectedVariableIdentifier, iu.Name(), iu.Index())
			}

		case sym.FunctionEntry:
			// make the identifier a function because its symbol is a function and it is used as a function kind
			if iu.Context()&ast.Function != 0 {
				iu.SetContext(ast.Function)

				// add the function usage to the function declaration
				if declaration := ast.SearchDeclaration(iu, symbol); declaration != nil {
					declaration.AddUsage(iu)
				}
			} else {
				a.appendError(expectedFunctionIdentifier, iu.Name(), iu.Index())
			}

		case sym.ProcedureEntry:
			// make the identifier a procedure because its symbol is a procedure and it is used as a procedure kind
			if iu.Context()&ast.Procedure != 0 {
				iu.SetContext(ast.Procedure)

				// add the procedure usage to the procedure declaration
				if declaration := ast.SearchDeclaration(iu, symbol); declaration != nil {
					declaration.AddUsage(iu)
				}
			} else {
				a.appendError(expectedProcedureIdentifier, iu.Name(), iu.Index())
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
	usageMode := as.Variable.(ast.IdentifierUse).UsageMode()
	usageMode |= ast.Write
	as.Variable.(ast.IdentifierUse).SetUsageMode(usageMode)
}

// Walk the read statement abstract syntax tree.
func (a *semanticAnalyzer) VisitReadStatement(rs *ast.ReadStatementNode) {
	// set the usage mode bit to write for the variable that is read into
	usageMode := rs.Variable.(ast.IdentifierUse).UsageMode()
	usageMode |= ast.Write
	rs.Variable.(ast.IdentifierUse).SetUsageMode(usageMode)
}

// Walk the write statement abstract syntax tree.
func (a *semanticAnalyzer) VisitWriteStatement(ws *ast.WriteStatementNode) {
	// set the usage mode bit to read for all constants and variables in the expression
	ast.Walk(ws.Expression, ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Walk the call statement abstract syntax tree.
func (a *semanticAnalyzer) VisitCallStatement(cs *ast.CallStatementNode) {
	// set the usage mode bit to execute for the procedure that is called
	usageMode := cs.Procedure.(ast.IdentifierUse).UsageMode()
	usageMode |= ast.Execute
	cs.Procedure.(ast.IdentifierUse).SetUsageMode(usageMode)
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
	if iu, ok := node.(ast.IdentifierUse); ok {
		if symbol := iu.CurrentBlock().Lookup(iu.Name()); symbol != nil {
			if symbol.Kind == sym.ConstantEntry || symbol.Kind == sym.VariableEntry {
				iu.SetUsageMode(iu.UsageMode() | ast.Read)
			}
		}
	}
}

// Visit identifier declarations and check if they are used. If not, append a warning to the token handler.
func validateIdentifierUsage(node ast.Node, tokenHandler any) {
	th := tokenHandler.(tok.TokenHandler)

	switch node.Kind() {
	case ast.KindConstantDeclaration:
		d := node.(ast.ConstantDeclaration)

		if len(d.Usage()) == 0 {
			th.AppendError(th.NewErrorOnIndex(eh.Warning, unusedConstantIdentifier, d.Name(), d.Index()))
		}

	case ast.KindVariableDeclaration:
		d := node.(ast.VariableDeclaration)

		if len(d.Usage()) == 0 {
			th.AppendError(th.NewErrorOnIndex(eh.Warning, unusedVariableIdentifier, d.Name(), d.Index()))
		}

	case ast.KindFunctionDeclaration:
		d := node.(ast.FunctionDeclaration)

		if len(d.Usage()) == 0 {
			if d.IsFunction() {
				th.AppendError(th.NewErrorOnIndex(eh.Warning, unusedFunctionIdentifier, d.Name(), d.Index()))
			} else {
				th.AppendError(th.NewErrorOnIndex(eh.Warning, unusedProcedureIdentifier, d.Name(), d.Index()))
			}
		}
	}
}

// Add all variables that are used in a block to the closure of the block if they are declared in an outer block.
func addVariableToBlockClosure(node ast.Node, _ any) {
	if iu, ok := node.(ast.IdentifierUse); ok {
		if symbol := iu.CurrentBlock().Lookup(iu.Name()); symbol != nil {
			if symbol.Kind == sym.VariableEntry {
				// determine the declaration of the symbol
				declaration := ast.SearchDeclaration(iu, symbol)

				// determine the block where the variable is declared
				declarationBlock := declaration.CurrentBlock()

				// determine the block where the variable is used
				useBlock := ast.SearchBlock(iu, ast.CurrentBlock)

				// add the variable to the closure of the block where it is used if it is declared in an outer block
				if useBlock.Depth()-declarationBlock.Depth() > 0 {
					useBlock.AddCapturedDeclaration(declaration)
				}
			}
		}
	}
}
