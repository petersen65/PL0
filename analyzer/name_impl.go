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

// Name analysis validates the correctness of identifier declarations and creates a symbol table with type name information provided by the abstract syntax tree.
// Herby, the analyzer checks for duplicate declarations and verifies that identifiers are declared before use.
type nameAnalyzer struct {
	abstractSyntax ast.Block        // abstract syntax tree to run semantic analysis on
	tokenHandler   tok.TokenHandler // token handler that manages the tokens of the token stream
}

// Return the interface of the name analyzer implementation.
func NewNameAnalyzer(abstractSyntax ast.Block, tokenHandler tok.TokenHandler) *nameAnalyzer {
	return &nameAnalyzer{
		abstractSyntax: abstractSyntax,
		tokenHandler:   tokenHandler,
	}
}

// Analyze the abstract syntax tree for declaration and use errors and fill in symbols into into the scope of blocks.
func (a *nameAnalyzer) Accept() {
	// ensure that all used identifiers are declared before use and store all identifier symbols into each block's scope
	a.abstractSyntax.Accept(a)

	// report a warning for all declared but unused identifiers
	if err := ast.Walk(a.abstractSyntax, ast.PreOrder, a.tokenHandler, reportWarningsForUnusedIdentifiers); err != nil {
		panic(eh.NewGeneralError(eh.Analyzer, failureMap, eh.Fatal, usageValidationFailed, nil, err))
	}
}

// Walk the block abstract syntax tree by visiting all declarations and the block statement.
func (a *nameAnalyzer) VisitBlock(b ast.Block) {
	for _, decl := range b.Declarations() {
		decl.Accept(a)
	}

	b.Statement().Accept(a)
}

// Enter the constant declaration as a symbol into the block's scope and check for redeclaration.
func (a *nameAnalyzer) VisitConstantDeclaration(cd ast.ConstantDeclaration) {
	cb := cd.CurrentBlock()             // current block
	s := cb.Lookup(cd.IdentifierName()) // constant symbol
	dts := cb.Lookup(cd.DataTypeName()) // constant data type symbol

	// in the case of no errors, insert the constant symbol into the current block's scope
	if s != nil {
		a.appendError(identifierAlreadyDeclared, cd.IdentifierName(), cd.Index())
	} else if dts == nil || dts.Kind != sym.DataTypeEntry {
		a.appendError(constantDataTypeNotFound, cd.DataTypeName(), cd.Index())
	} else {
		symbol := sym.NewSymbol(cd.IdentifierName(), sym.ConstantEntry, dts.DataType, cd.Value())
		cb.Insert(cd.IdentifierName(), symbol)
		cd.SetSymbol(symbol)
	}
}

// Enter the variable declaration as a symbol into the block's scope and check for redeclaration.
func (a *nameAnalyzer) VisitVariableDeclaration(vd ast.VariableDeclaration) {
	cb := vd.CurrentBlock()             // current block
	s := cb.Lookup(vd.IdentifierName()) // variable symbol
	dts := cb.Lookup(vd.DataTypeName()) // variable data type symbol

	// in the case of no errors, insert the variable symbol into the current block's scope
	if s != nil {
		a.appendError(identifierAlreadyDeclared, vd.IdentifierName(), vd.Index())
	} else if dts == nil || dts.Kind != sym.DataTypeEntry {
		a.appendError(variableDataTypeNotFound, vd.DataTypeName(), vd.Index())
	} else {
		symbol := sym.NewSymbol(vd.IdentifierName(), sym.VariableEntry, dts.DataType, nil)
		cb.Insert(vd.IdentifierName(), symbol)
		vd.SetSymbol(symbol)
	}
}

// Enter the function declaration as a symbol into the block's scope and check for redeclaration.
func (a *nameAnalyzer) VisitFunctionDeclaration(fd ast.FunctionDeclaration) {
	cb := fd.CurrentBlock()             // current block
	s := cb.Lookup(fd.IdentifierName()) // function symbol

	var symbolEntry sym.EntryKind = sym.ProcedureEntry // function or procedure symbol entry kind
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

	// enrich function parameters from the function declaration with data types descriptors
	for _, parameter := range fd.Parameters() {
		if pts := cb.Lookup(parameter.TypeName); pts == nil || pts.Kind != sym.DataTypeEntry {
			if fd.IsProcedure() {
				a.appendError(procedureParameterTypeNotFound, parameter.TypeName, fd.Index())
			} else {
				a.appendError(functionParameterTypeNotFound, parameter.TypeName, fd.Index())
			}
		} else {
			parameter.Type = pts.DataType
		}
	}

	// append an error if the function was already declared
	if s != nil {
		a.appendError(identifierAlreadyDeclared, fd.IdentifierName(), fd.Index())
	} else {
		// create a data type for a function or procedure with a predefined list of parameters and an optional return type
		functionType := ts.NewFunctionTypeDescriptor(fd.Parameters(), returnType, false)

		// if the function data type is not found, insert it into the current block's scope
		if fts := cb.Lookup(functionType.String()); fts != nil {
			functionType = fts.DataType
		} else {
			cb.Insert(functionType.String(), sym.NewSymbol(functionType.String(), sym.DataTypeEntry, functionType, nil))
		}

		// insert the function symbol into the current block's scope
		symbol := sym.NewSymbol(fd.IdentifierName(), symbolEntry, functionType, nil)
		cb.Insert(fd.IdentifierName(), symbol)

		// set the symbol and data type name of the function declaration
		fd.SetSymbol(symbol)
	}

	// visit the block of the function or procedure declaration
	fd.Block().Accept(a)
}

// Visit the literal-use node.
func (a *nameAnalyzer) VisitLiteralUse(lu ast.LiteralUse) {
	// nothing to do because a literal does not have any identifier names
}

// Check if the used identifier is declared and if it is used correctly according to its symbol kind. Record the usage of the identifier in its declaration.
func (a *nameAnalyzer) VisitIdentifierUse(iu ast.IdentifierUse) {
	// if the identifier used does not have a declaration or symbol, report an error
	if iu.Declaration() == nil || iu.Declaration().Symbol() == nil {
		a.appendError(identifierNotFound, iu.IdentifierName(), iu.Index())
		return
	}

	// get the declaration and symbol of the used identifier
	declaration := iu.Declaration()
	symbol := declaration.Symbol()

	switch symbol.Kind {
	case sym.ConstantEntry:
		// make the identifier a constant because its symbol is a constant and it is used as a constant kind
		if iu.IdentifierKind()&ast.Constant != 0 {
			// add the constant usage to the constant declaration
			iu.SetIdentifierKind(ast.Constant)
			declaration.AddUsage(iu)
		} else {
			a.appendError(expectedConstantIdentifier, iu.IdentifierName(), iu.Index())
		}

	case sym.VariableEntry:
		// make the identifier a variable because its symbol is a variable and it is used as a variable kind
		if iu.IdentifierKind()&ast.Variable != 0 {
			// add the variable usage to the variable declaration
			iu.SetIdentifierKind(ast.Variable)
			declaration.AddUsage(iu)
		} else {
			a.appendError(expectedVariableIdentifier, iu.IdentifierName(), iu.Index())
		}

	case sym.FunctionEntry:
		// make the identifier a function because its symbol is a function and it is used as a function kind
		if iu.IdentifierKind()&ast.Function != 0 {
			// add the function usage to the function declaration
			iu.SetIdentifierKind(ast.Function)
			declaration.AddUsage(iu)
		} else {
			a.appendError(expectedFunctionIdentifier, iu.IdentifierName(), iu.Index())
		}

	case sym.ProcedureEntry:
		// make the identifier a procedure because its symbol is a procedure and it is used as a procedure kind
		if iu.IdentifierKind()&ast.Procedure != 0 {
			// add the procedure usage to the procedure declaration
			iu.SetIdentifierKind(ast.Procedure)
			declaration.AddUsage(iu)
		} else {
			a.appendError(expectedProcedureIdentifier, iu.IdentifierName(), iu.Index())
		}

	default:
		panic(eh.NewGeneralError(eh.Analyzer, failureMap, eh.Fatal, unknownSymbolKind, nil, nil))
	}
}

// Visit the unary operation node and set the usage mode bit to read for all constants and variables in the operand expression.
func (a *nameAnalyzer) VisitUnaryOperation(uo ast.UnaryOperation) {
	uo.Operand().Accept(a)
	ast.Walk(uo.Operand(), ast.PreOrder, nil, setConstantVariableUsageModeAsRead)
}

// Visit the arithmetic operation node and set the usage mode bit to read for all constants and variables in the left and right expressions.
func (a *nameAnalyzer) VisitArithmeticOperation(bo ast.ArithmeticOperation) {
	bo.Left().Accept(a)
	bo.Right().Accept(a)
	ast.Walk(bo.Left(), ast.PreOrder, nil, setConstantVariableUsageModeAsRead)
	ast.Walk(bo.Right(), ast.PreOrder, nil, setConstantVariableUsageModeAsRead)
}

// Visit the comparison operation node and set the usage mode bit to read for all constants and variables in the left and right expressions.
func (a *nameAnalyzer) VisitComparisonOperation(co ast.ComparisonOperation) {
	co.Left().Accept(a)
	co.Right().Accept(a)
	ast.Walk(co.Left(), ast.PreOrder, nil, setConstantVariableUsageModeAsRead)
	ast.Walk(co.Right(), ast.PreOrder, nil, setConstantVariableUsageModeAsRead)
}

// Visit the assignment statement node and set the usage mode bit to write for the variable that is assigned to.
func (a *nameAnalyzer) VisitAssignmentStatement(as ast.AssignmentStatement) {
	as.Variable().Accept(a)
	as.Expression().Accept(a)
	as.Variable().SetUsageMode(as.Variable().UsageMode() | ast.Write)
}

// Visit the read statement node and set the usage mode bit to write for the variable that is read into.
func (a *nameAnalyzer) VisitReadStatement(rs ast.ReadStatement) {
	rs.Variable().Accept(a)
	rs.Variable().SetUsageMode(rs.Variable().UsageMode() | ast.Write)
}

// Visit the write statement node and set the usage mode bit to read for all constants and variables in the write expression.
func (a *nameAnalyzer) VisitWriteStatement(ws ast.WriteStatement) {
	ws.Expression().Accept(a)
	ast.Walk(ws.Expression(), ast.PreOrder, nil, setConstantVariableUsageModeAsRead)
}

// Visit the call statement node and set the usage mode bit to execute for the called function or procedure.
func (a *nameAnalyzer) VisitCallStatement(cs ast.CallStatement) {
	cs.Function().Accept(a)
	cs.Function().SetUsageMode(cs.Function().UsageMode() | ast.Execute)
}

// Visit the if statement node and set the usage mode bit to read for all constants and variables in the condition.
func (a *nameAnalyzer) VisitIfStatement(is ast.IfStatement) {
	is.Condition().Accept(a)
	is.Statement().Accept(a)
	ast.Walk(is.Condition(), ast.PreOrder, nil, setConstantVariableUsageModeAsRead)
}

// Visit the while statement node and set the usage mode bit to read for all constants and variables in the condition.
func (a *nameAnalyzer) VisitWhileStatement(ws ast.WhileStatement) {
	ws.Condition().Accept(a)
	ws.Statement().Accept(a)
	ast.Walk(ws.Condition(), ast.PreOrder, nil, setConstantVariableUsageModeAsRead)
}

// Visit the compound statement node by visiting all its statements.
func (a *nameAnalyzer) VisitCompoundStatement(cs ast.CompoundStatement) {
	for _, statement := range cs.Statements() {
		statement.Accept(a)
	}
}

// Append an error from the name analyzer to the token handler's error list.
func (a *nameAnalyzer) appendError(code eh.Failure, value any, index int) {
	a.tokenHandler.AppendError(a.tokenHandler.NewErrorOnIndex(eh.Error, code, value, index))
}

// This is a visitor function. For all occurrences of a constant or variable usage, set the usage mode bit to read.
func setConstantVariableUsageModeAsRead(node ast.Node, _ any) {
	// only set the usage mode bit to read for constants or variables
	if iu, ok := node.(ast.IdentifierUse); ok {
		if iu.IdentifierKind()&ast.Constant != 0 || iu.IdentifierKind()&ast.Variable != 0 {
			iu.SetUsageMode(iu.UsageMode() | ast.Read)
		}
	}
}

// This is a visitor function. Visit all identifier declarations and check if they are used. If not, report a warning.
func reportWarningsForUnusedIdentifiers(node ast.Node, tokenHandler any) {
	th := tokenHandler.(tok.TokenHandler)

	// safely switch on the kind of the node and then cast it to the appropriate declaration kind
	switch node.Kind() {
	case ast.KindConstantDeclaration:
		cd := node.(ast.ConstantDeclaration)

		// if the constant declaration has no usages, report a warning
		if len(cd.Usage()) == 0 {
			th.AppendError(th.NewErrorOnIndex(eh.Warning, unusedConstantIdentifier, cd.IdentifierName(), cd.Index()))
		}

	case ast.KindVariableDeclaration:
		vd := node.(ast.VariableDeclaration)

		// if the variable declaration has no usages, report a warning
		if len(vd.Usage()) == 0 {
			th.AppendError(th.NewErrorOnIndex(eh.Warning, unusedVariableIdentifier, vd.IdentifierName(), vd.Index()))
		}

	case ast.KindFunctionDeclaration:
		fd := node.(ast.FunctionDeclaration)

		// if the function or procedure declaration has no usages, report a warning
		if len(fd.Usage()) == 0 {
			if fd.IsFunction() {
				th.AppendError(th.NewErrorOnIndex(eh.Warning, unusedFunctionIdentifier, fd.IdentifierName(), fd.Index()))
			} else {
				th.AppendError(th.NewErrorOnIndex(eh.Warning, unusedProcedureIdentifier, fd.IdentifierName(), fd.Index()))
			}
		}
	}
}
