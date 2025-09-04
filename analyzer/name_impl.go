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

// Map parameter passing modes from the abstract syntax tree to the type system.
var passingModeMap = map[ast.PassingMode]ts.ParameterPassingMode{
	ast.CallByValue:          ts.CallByValue,
	ast.CallByReference:      ts.CallByReference,
	ast.CallByConstReference: ts.CallByConstReference,
	ast.OutputParameter:      ts.OutputParameter,
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

// Enter the variable declaration as a symbol into the block's scope and check for redeclaration.
func (a *nameAnalyzer) VisitVariableDeclaration(vd ast.VariableDeclaration) {
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

// Enter the function declaration as a symbol into the block's scope and check for redeclaration.
func (a *nameAnalyzer) VisitFunctionDeclaration(fd ast.FunctionDeclaration) {
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

	// visit the block of the function or procedure declaration
	fd.Block().Accept(a)
}

// Visit the literal-use node.
func (a *nameAnalyzer) VisitLiteralUse(lu ast.LiteralUse) {
	// nothing to do because a literal does not have any identifiers
}

// Check if the used identifier is declared and if it is used in the correct context.
func (a *nameAnalyzer) VisitIdentifierUse(iu ast.IdentifierUse) {
	// get the declaration and symbol of the used identifier
	declaration := iu.Declaration()
	symbol := iu.Symbol()

	// if the identifier used does not have a declaration or symbol, report an error
	if declaration == nil || symbol == nil {
		a.appendError(identifierNotFound, iu.Name(), iu.Index())
		return
	}

	switch symbol.Kind {
	case sym.ConstantEntry:
		// make the identifier a constant because its symbol is a constant and it is used as a constant kind
		if iu.Context()&ast.Constant != 0 {
			// add the constant usage to the constant declaration
			iu.SetContext(ast.Constant)
			declaration.AddUsage(iu)
		} else {
			a.appendError(expectedConstantIdentifier, iu.Name(), iu.Index())
		}

	case sym.VariableEntry:
		// make the identifier a variable because its symbol is a variable and it is used as a variable kind
		if iu.Context()&ast.Variable != 0 {
			// add the variable usage to the variable declaration
			iu.SetContext(ast.Variable)
			declaration.AddUsage(iu)
		} else {
			a.appendError(expectedVariableIdentifier, iu.Name(), iu.Index())
		}

	case sym.FunctionEntry:
		// make the identifier a function because its symbol is a function and it is used as a function kind
		if iu.Context()&ast.Function != 0 {
			// add the function usage to the function declaration
			iu.SetContext(ast.Function)
			declaration.AddUsage(iu)
		} else {
			a.appendError(expectedFunctionIdentifier, iu.Name(), iu.Index())
		}

	case sym.ProcedureEntry:
		// make the identifier a procedure because its symbol is a procedure and it is used as a procedure kind
		if iu.Context()&ast.Procedure != 0 {
			// add the procedure usage to the procedure declaration
			iu.SetContext(ast.Procedure)
			declaration.AddUsage(iu)
		} else {
			a.appendError(expectedProcedureIdentifier, iu.Name(), iu.Index())
		}

	default:
		panic(eh.NewGeneralError(eh.Analyzer, failureMap, eh.Fatal, unknownSymbolKind, nil, nil))
	}
}

// Visit the unary operation node and set the usage mode bit to read for all constants and variables in the operand expression.
func (a *nameAnalyzer) VisitUnaryOperation(uo ast.UnaryOperation) {
	ast.Walk(uo.Operand(), ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Visit the binary operation node and set the usage mode bit to read for all constants and variables in the left and right expressions.
func (a *nameAnalyzer) VisitBinaryOperation(bo ast.BinaryOperation) {
	ast.Walk(bo.Left(), ast.PreOrder, nil, setConstantVariableUsageAsRead)
	ast.Walk(bo.Right(), ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Visit the comparison operation node and set the usage mode bit to read for all constants and variables in the left and right expressions.
func (a *nameAnalyzer) VisitComparisonOperation(co ast.ComparisonOperation) {
	ast.Walk(co.Left(), ast.PreOrder, nil, setConstantVariableUsageAsRead)
	ast.Walk(co.Right(), ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Visit the assignment statement node and set the usage mode bit to write for the variable that is assigned to.
func (a *nameAnalyzer) VisitAssignmentStatement(as ast.AssignmentStatement) {
	as.Variable().SetUsageMode(as.Variable().UsageMode() | ast.Write)
}

// Visit the read statement node and set the usage mode bit to write for the variable that is read into.
func (a *nameAnalyzer) VisitReadStatement(rs ast.ReadStatement) {
	rs.Variable().SetUsageMode(rs.Variable().UsageMode() | ast.Write)
}

// Visit the write statement node and set the usage mode bit to read for all constants and variables in the write expression.
func (a *nameAnalyzer) VisitWriteStatement(ws ast.WriteStatement) {
	ast.Walk(ws.Expression(), ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Visit the call statement node and set the usage mode bit to execute for the called function or procedure.
func (a *nameAnalyzer) VisitCallStatement(cs ast.CallStatement) {
	cs.Function().SetUsageMode(cs.Function().UsageMode() | ast.Execute)
}

// Visit the if statement node and set the usage mode bit to read for all constants and variables in the condition.
func (a *nameAnalyzer) VisitIfStatement(is ast.IfStatement) {
	ast.Walk(is.Condition(), ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Visit the while statement node and set the usage mode bit to read for all constants and variables in the condition.
func (a *nameAnalyzer) VisitWhileStatement(ws ast.WhileStatement) {
	ast.Walk(ws.Condition(), ast.PreOrder, nil, setConstantVariableUsageAsRead)
}

// Walk the compound statement abstract syntax tree.
func (a *nameAnalyzer) VisitCompoundStatement(cs ast.CompoundStatement) {}

// Append an error from the name analyzer to the token handler's error list.
func (a *nameAnalyzer) appendError(code eh.Failure, value any, index int) {
	a.tokenHandler.AppendError(a.tokenHandler.NewErrorOnIndex(eh.Error, code, value, index))
}

// This is a visitor function. For all occurrences of a constant or variable usage, set the usage mode bit to read.
func setConstantVariableUsageAsRead(node ast.Node, _ any) {
	if iu, ok := node.(ast.IdentifierUse); ok {
		if symbol := iu.Symbol(); symbol != nil {
			if symbol.Kind == sym.ConstantEntry || symbol.Kind == sym.VariableEntry {
				iu.SetUsageMode(iu.UsageMode() | ast.Read)
			}
		}
	}
}

// This is a visitor function. Visit all identifier declarations and check if they are used. If not, report a warning.
func reportWarningsForUnusedIdentifiers(node ast.Node, tokenHandler any) {
	th := tokenHandler.(tok.TokenHandler)

	// safely switch on the kind of the node and then cast it to the appropriate declaration kind
	switch node.Kind() {
	case ast.KindConstantDeclaration:
		d := node.(ast.ConstantDeclaration)

		// if the constant declaration has no usages, report a warning
		if len(d.Usage()) == 0 {
			th.AppendError(th.NewErrorOnIndex(eh.Warning, unusedConstantIdentifier, d.Name(), d.Index()))
		}

	case ast.KindVariableDeclaration:
		d := node.(ast.VariableDeclaration)

		// if the variable declaration has no usages, report a warning
		if len(d.Usage()) == 0 {
			th.AppendError(th.NewErrorOnIndex(eh.Warning, unusedVariableIdentifier, d.Name(), d.Index()))
		}

	case ast.KindFunctionDeclaration:
		d := node.(ast.FunctionDeclaration)

		// if the function or procedure declaration has no usages, report a warning
		if len(d.Usage()) == 0 {
			if d.IsFunction() {
				th.AppendError(th.NewErrorOnIndex(eh.Warning, unusedFunctionIdentifier, d.Name(), d.Index()))
			} else {
				th.AppendError(th.NewErrorOnIndex(eh.Warning, unusedProcedureIdentifier, d.Name(), d.Index()))
			}
		}
	}
}
