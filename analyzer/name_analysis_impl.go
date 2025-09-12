// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package analyzer

import (
	"fmt"

	ast "github.com/petersen65/pl0/v3/ast"
	eh "github.com/petersen65/pl0/v3/errors"
	sym "github.com/petersen65/pl0/v3/symbol"
	tok "github.com/petersen65/pl0/v3/token"
	ts "github.com/petersen65/pl0/v3/typesystem"
)

// Name analysis validates the correctness of identifier declarations and creates a symbol table with type name information provided by the abstract syntax tree.
// Herby, the analyzer checks for duplicate declarations and verifies that identifiers are declared before use.
type nameAnalysis struct {
	abstractSyntax ast.Block        // abstract syntax tree to run semantic analysis on
	tokenHandler   tok.TokenHandler // token handler that manages the tokens of the token stream
}

// Return the interface of the name analysis implementation.
func newNameAnalysis(abstractSyntax ast.Block, tokenHandler tok.TokenHandler) *nameAnalysis {
	return &nameAnalysis{
		abstractSyntax: abstractSyntax,
		tokenHandler:   tokenHandler,
	}
}

// Analyze the abstract syntax tree for declaration and use errors and fill in symbols into into the scope of blocks.
func (na *nameAnalysis) Accept() {
	// ensure that all used identifiers are declared before use and store all identifier symbols into each block's scope
	na.abstractSyntax.Accept(na)

	// report a warning for all declared but unused identifiers
	if err := ast.Walk(na.abstractSyntax, ast.PreOrder, na.tokenHandler, reportWarningsForUnusedIdentifiers); err != nil {
		panic(eh.NewGeneralError(eh.Analyzer, failureMap, eh.Fatal, usageValidationFailed, err))
	}
}

// Walk the block abstract syntax tree by visiting all declarations and the block statement.
func (na *nameAnalysis) VisitBlock(b ast.Block) {
	for _, declaration := range b.Declarations() {
		declaration.Accept(na)
	}

	b.Statement().Accept(na)
}

// Enter the constant declaration as a symbol into the block's scope and check for redeclaration.
func (na *nameAnalysis) VisitConstantDeclaration(cd ast.ConstantDeclaration) {
	e := cd.Expression() // expression on the right side of the constant identifier
	e.Accept(na)         // visit the expression of the constant declaration
	dte := e.DataType()  // trigger the data type inference of the expression

	// set the data type name of the constant declaration from the inferred data type of the expression
	if dte != nil {
		cd.SetDataTypeName(dte.String())
	}

	cb := cd.CurrentBlock()             // current block
	s := cb.Lookup(cd.IdentifierName()) // constant symbol
	dts := cb.Lookup(cd.DataTypeName()) // constant data type symbol

	// in the case of no errors, insert the constant symbol into the current block's scope
	if dte == nil {
		na.appendError(constantDataTypeCannotBeInferred, cd.Index(), cd.IdentifierName())
	} else if !e.IsConstant() {
		na.appendError(constantExpressionMustBeConstant, cd.Index(), cd.IdentifierName())
	} else if cd.CurrentBlock().BuiltInDataType(cd.IdentifierName()) != nil {
		na.appendError(constantIdentifierHasBuiltInName, cd.Index(), cd.IdentifierName())
	} else if s != nil {
		na.appendError(identifierAlreadyDeclared, cd.Index(), cd.IdentifierName())
	} else if dts == nil || dts.DataType == nil || dts.Kind != sym.DataTypeEntry {
		na.appendError(constantDataTypeNotFound, cd.Index(), cd.DataTypeName())
	} else {
		symbol := sym.NewSymbol(cd.IdentifierName(), sym.ConstantEntry, dts.DataType, nil)
		cb.Insert(cd.IdentifierName(), symbol)
		cd.SetSymbol(symbol)

		result := &calculationResult{}
        if e.IsConstant() {
            ast.Walk(e, ast.PostOrder, result, calculateConstantExpressionValue)
            if result.valid != nil {
                // Handle error - constant expression evaluation failed
                //na.appendError(constantExpressionEvaluationFailed, cd.Index(), cd.IdentifierName(), result.valid)
            } else {
                // Store the calculated value in the symbol or constant declaration
                symbol.Value = result.value
                //cd.SetValue(result.value)
                fmt.Println("Constant", cd.IdentifierName(), "=", result.value)
            }
        }
	}
}

// Enter the variable declaration as a symbol into the block's scope and check for redeclaration.
func (na *nameAnalysis) VisitVariableDeclaration(vd ast.VariableDeclaration) {
	cb := vd.CurrentBlock()             // current block
	s := cb.Lookup(vd.IdentifierName()) // variable symbol
	dts := cb.Lookup(vd.DataTypeName()) // variable data type symbol

	// in the case of no errors, insert the variable symbol into the current block's scope
	if vd.CurrentBlock().BuiltInDataType(vd.IdentifierName()) != nil {
		na.appendError(variableIdentifierHasBuiltInName, vd.Index(), vd.IdentifierName())
	} else if s != nil {
		na.appendError(identifierAlreadyDeclared, vd.Index(), vd.IdentifierName())
	} else if dts == nil || dts.Kind != sym.DataTypeEntry {
		_, typeIndex := vd.IndexPair()
		na.appendError(variableDataTypeNotFound, typeIndex, vd.DataTypeName())
	} else {
		symbol := sym.NewSymbol(vd.IdentifierName(), sym.VariableEntry, dts.DataType, nil)
		cb.Insert(vd.IdentifierName(), symbol)
		vd.SetSymbol(symbol)
	}
}

// Enter the function declaration as a symbol into the block's scope and check for redeclaration.
func (na *nameAnalysis) VisitFunctionDeclaration(fd ast.FunctionDeclaration) {
	cb := fd.CurrentBlock()             // current block
	s := cb.Lookup(fd.IdentifierName()) // function symbol

	var symbolEntry sym.EntryKind = sym.ProcedureEntry // function or procedure symbol entry kind
	var returnType ts.TypeDescriptor                   // function return type or nil

	// handle the return data type of a function
	if fd.IsFunction() {
		symbolEntry = sym.FunctionEntry

		if rts := cb.Lookup(fd.ReturnTypeName()); rts == nil || rts.Kind != sym.DataTypeEntry {
			na.appendError(functionReturnTypeNotFound, fd.Index(), fd.ReturnTypeName())
		} else {
			returnType = rts.DataType
		}
	}

	// enrich function parameters from the function declaration with data types descriptors
	for _, parameter := range fd.Parameters() {
		if pts := cb.Lookup(parameter.TypeName); pts == nil || pts.Kind != sym.DataTypeEntry {
			if fd.IsProcedure() {
				na.appendError(procedureParameterTypeNotFound, fd.Index(), parameter.TypeName)
			} else {
				na.appendError(functionParameterTypeNotFound, fd.Index(), parameter.TypeName)
			}
		} else {
			parameter.Type = pts.DataType
		}
	}

	// append an error if the function was already declared
	if fd.CurrentBlock().BuiltInDataType(fd.IdentifierName()) != nil {
		if fd.IsFunction() {
			na.appendError(functionIdentifierHasBuiltInName, fd.Index(), fd.IdentifierName())
		} else {
			na.appendError(procedureIdentifierHasBuiltInName, fd.Index(), fd.IdentifierName())
		}
	} else if s != nil {
		na.appendError(identifierAlreadyDeclared, fd.Index(), fd.IdentifierName())
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
	fd.Block().Accept(na)
}

// Visit the literal-use node.
func (na *nameAnalysis) VisitLiteralUse(lu ast.LiteralUse) {
	// trigger the value determination and the data type inference of the literal
	lu.DataType()
}

// Check if the used identifier is declared and if it is used correctly according to its symbol kind. Record the usage of the identifier in its declaration.
func (na *nameAnalysis) VisitIdentifierUse(iu ast.IdentifierUse) {
	// if the identifier used does not have a declaration or symbol, report an error
	if iu.Declaration() == nil || iu.Declaration().Symbol() == nil {
		na.appendError(identifierNotFound, iu.Index(), iu.IdentifierName())
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
			na.appendError(expectedConstantIdentifier, iu.Index(), iu.IdentifierName())
		}

	case sym.VariableEntry:
		// make the identifier a variable because its symbol is a variable and it is used as a variable kind
		if iu.IdentifierKind()&ast.Variable != 0 {
			// add the variable usage to the variable declaration
			iu.SetIdentifierKind(ast.Variable)
			declaration.AddUsage(iu)
		} else {
			na.appendError(expectedVariableIdentifier, iu.Index(), iu.IdentifierName())
		}

	case sym.FunctionEntry:
		// make the identifier a function because its symbol is a function and it is used as a function kind
		if iu.IdentifierKind()&ast.Function != 0 {
			// add the function usage to the function declaration
			iu.SetIdentifierKind(ast.Function)
			declaration.AddUsage(iu)
		} else {
			na.appendError(expectedFunctionIdentifier, iu.Index(), iu.IdentifierName())
		}

	case sym.ProcedureEntry:
		// make the identifier a procedure because its symbol is a procedure and it is used as a procedure kind
		if iu.IdentifierKind()&ast.Procedure != 0 {
			// add the procedure usage to the procedure declaration
			iu.SetIdentifierKind(ast.Procedure)
			declaration.AddUsage(iu)
		} else {
			na.appendError(expectedProcedureIdentifier, iu.Index(), iu.IdentifierName())
		}

	default:
		panic(eh.NewGeneralError(eh.Analyzer, failureMap, eh.Fatal, unknownSymbolKind, nil))
	}
}

// Visit the unary operation node and set the usage mode bit to read for all constants and variables in the operand expression.
func (na *nameAnalysis) VisitUnaryOperation(uo ast.UnaryOperation) {
	uo.Operand().Accept(na)
	ast.Walk(uo.Operand(), ast.PreOrder, nil, setConstantVariableUsageModeAsRead)

	// get data type of the operand
	operandType := uo.Operand().DataType()

	// if the data type is nil, errors have already been reported so just return
	if operandType == nil {
		return
	}

	switch uo.Operation() {
	case ast.Negate:
		// check if the data type of the unary operation operand meets all requirements of the negate operation
		if !operandType.HasAllCapabilities(uo.Requirements()) {
			na.appendError(dataTypeCannotBeUsedInUnaryOperation, uo.Index(), uo, uo.Requirements(), operandType)
		}

	case ast.Odd:
		// check if the data type of the unary operation operand meets all requirements of the odd operation
		if !operandType.HasAllCapabilities(uo.Requirements()) {
			na.appendError(dataTypeCannotBeUsedInUnaryOperation, uo.Index(), uo, uo.Requirements(), operandType)
		}

	default:
		panic(eh.NewGeneralError(eh.Analyzer, failureMap, eh.Fatal, unknownUnaryOperation, nil))
	}
}

// Visit the arithmetic operation node and set the usage mode bit to read for all constants and variables in the left and right expressions.
func (na *nameAnalysis) VisitArithmeticOperation(ao ast.ArithmeticOperation) {
	ao.Left().Accept(na)
	ao.Right().Accept(na)
	ast.Walk(ao.Left(), ast.PreOrder, nil, setConstantVariableUsageModeAsRead)
	ast.Walk(ao.Right(), ast.PreOrder, nil, setConstantVariableUsageModeAsRead)

	// get data types of both operands
	leftType := ao.Left().DataType()
	rightType := ao.Right().DataType()

	// if either data type is nil, errors have already been reported so just return
	if leftType == nil || rightType == nil {
		return
	}

	// check if the data types of both arithmetic operation operands are equal
	if !leftType.Equal(rightType) {
		na.appendError(incompatibleDataTypesInArithmeticOperation, ao.Index(), ao, leftType, rightType)
		return
	}

	// check if the data type of the left operand meets all requirements of the arithmetic operation
	if !leftType.HasAllCapabilities(ao.Requirements()) {
		na.appendError(dataTypeCannotBeUsedInArithmeticOperation, ao.Index(), ao, ao.Requirements(), leftType)
	}
}

// Visit the comparison operation node and set the usage mode bit to read for all constants and variables in the left and right expressions.
func (na *nameAnalysis) VisitComparisonOperation(co ast.ComparisonOperation) {
	co.Left().Accept(na)
	co.Right().Accept(na)
	ast.Walk(co.Left(), ast.PreOrder, nil, setConstantVariableUsageModeAsRead)
	ast.Walk(co.Right(), ast.PreOrder, nil, setConstantVariableUsageModeAsRead)

	// get data types of both operands
	leftType := co.Left().DataType()
	rightType := co.Right().DataType()

	// if either data type is nil, errors have already been reported so just return
	if leftType == nil || rightType == nil {
		return
	}

	// check if the data types of both comparison operation operands are equal
	if !leftType.Equal(rightType) {
		na.appendError(incompatibleDataTypesInComparisonOperation, co.Index(), co, leftType, rightType)
		return
	}

	// check if the data type of the left operand meets all requirements of the comparison operation
	if !leftType.HasAllCapabilities(co.Requirements()) {
		na.appendError(dataTypeCannotBeUsedInComparisonOperation, co.Index(), co, co.Requirements(), leftType)
	}
}

// Visit the assignment statement node and set the usage mode bit to write for the variable that is assigned to.
func (na *nameAnalysis) VisitAssignmentStatement(as ast.AssignmentStatement) {
	as.Variable().Accept(na)
	as.Expression().Accept(na)
	as.Variable().SetUsageMode(as.Variable().UsageMode() | ast.Write)
}

// Visit the read statement node and set the usage mode bit to write for the variable that is read into.
func (na *nameAnalysis) VisitReadStatement(rs ast.ReadStatement) {
	rs.Variable().Accept(na)
	rs.Variable().SetUsageMode(rs.Variable().UsageMode() | ast.Write)
}

// Visit the write statement node and set the usage mode bit to read for all constants and variables in the write expression.
func (na *nameAnalysis) VisitWriteStatement(ws ast.WriteStatement) {
	ws.Expression().Accept(na)
	ast.Walk(ws.Expression(), ast.PreOrder, nil, setConstantVariableUsageModeAsRead)
}

// Visit the call statement node and set the usage mode bit to execute for the called procedure.
func (na *nameAnalysis) VisitCallStatement(cs ast.CallStatement) {
	cs.Procedure().Accept(na)
	cs.Procedure().SetUsageMode(cs.Procedure().UsageMode() | ast.Execute)
}

// Visit the if statement node and set the usage mode bit to read for all constants and variables in the condition.
func (na *nameAnalysis) VisitIfStatement(is ast.IfStatement) {
	is.Condition().Accept(na)
	is.Statement().Accept(na)
	ast.Walk(is.Condition(), ast.PreOrder, nil, setConstantVariableUsageModeAsRead)
}

// Visit the while statement node and set the usage mode bit to read for all constants and variables in the condition.
func (na *nameAnalysis) VisitWhileStatement(ws ast.WhileStatement) {
	ws.Condition().Accept(na)
	ws.Statement().Accept(na)
	ast.Walk(ws.Condition(), ast.PreOrder, nil, setConstantVariableUsageModeAsRead)
}

// Visit the compound statement node by visiting all its statements.
func (na *nameAnalysis) VisitCompoundStatement(cs ast.CompoundStatement) {
	for _, statement := range cs.Statements() {
		statement.Accept(na)
	}
}

// Append an error from the name analysis to the token handler's error list.
func (na *nameAnalysis) appendError(code eh.Failure, index int, values ...any) {
	na.tokenHandler.AppendError(na.tokenHandler.NewErrorOnIndex(eh.Error, code, index, values...))
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
			th.AppendError(th.NewErrorOnIndex(eh.Warning, unusedConstantIdentifier, cd.Index(), cd.IdentifierName()))
		}

	case ast.KindVariableDeclaration:
		vd := node.(ast.VariableDeclaration)

		// if the variable declaration has no usages, report a warning
		if len(vd.Usage()) == 0 {
			th.AppendError(th.NewErrorOnIndex(eh.Warning, unusedVariableIdentifier, vd.Index(), vd.IdentifierName()))
		}

	case ast.KindFunctionDeclaration:
		fd := node.(ast.FunctionDeclaration)

		// if the function or procedure declaration has no usages, report a warning
		if len(fd.Usage()) == 0 {
			if fd.IsFunction() {
				th.AppendError(th.NewErrorOnIndex(eh.Warning, unusedFunctionIdentifier, fd.Index(), fd.IdentifierName()))
			} else {
				th.AppendError(th.NewErrorOnIndex(eh.Warning, unusedProcedureIdentifier, fd.Index(), fd.IdentifierName()))
			}
		}
	}
}



// This is a visitor function. Calculate the value of a constant expression for all data types and operations.
// This function assumes that the starting node is a constant expression and that all unary and binary operation requirements regarding data types have been met.
// ...existing code...

type calculationResult struct {
    value any
    valid error
}

// This is a visitor function. Calculate the value of a constant expression for all data types and operations.
// This function assumes that the starting node is a constant expression and that all unary and binary operation requirements regarding data types have been met.
func calculateConstantExpressionValue(node ast.Node, result any) {
    e := node.(ast.Expression)
    cr := result.(*calculationResult)

    // if an error has already occurred, do nothing and delegate the error to the caller
    if cr.valid != nil {
        return
    }

    // safely switch on the kind of the expression node and then cast it to the appropriate expression kind
    switch e.Kind() {
    case ast.KindLiteralUse:
        lu := e.(ast.LiteralUse)
        // For literals, directly set the value
        cr.value = lu.Value()

    case ast.KindIdentifierUse:
        iu := e.(ast.IdentifierUse)
        // For identifier uses (constants), get the value
        cr.value = iu.Value()

    case ast.KindUnaryOperation:
        uo := e.(ast.UnaryOperation)
        
        // Create a new result for the operand
        operandResult := &calculationResult{}
        ast.Walk(uo.Operand(), ast.PostOrder, operandResult, calculateConstantExpressionValue)
        
        // Check for errors from operand evaluation
        if operandResult.valid != nil {
            cr.valid = operandResult.valid
            return
        }
        
        // Perform the unary operation
        cr.value, cr.valid = performUnaryOperation(uo.Operation(), operandResult.value)

    case ast.KindArithmeticOperation:
        ao := e.(ast.ArithmeticOperation)
        
        // Create results for both operands
        leftResult := &calculationResult{}
        rightResult := &calculationResult{}
        
        // Evaluate left operand
        ast.Walk(ao.Left(), ast.PostOrder, leftResult, calculateConstantExpressionValue)
        if leftResult.valid != nil {
            cr.valid = leftResult.valid
            return
        }
        
        // Evaluate right operand
        ast.Walk(ao.Right(), ast.PostOrder, rightResult, calculateConstantExpressionValue)
        if rightResult.valid != nil {
            cr.valid = rightResult.valid
            return
        }
        
        // Perform the arithmetic operation
        cr.value, cr.valid = performArithmeticOperation(ao.Operation(), leftResult.value, rightResult.value)

    case ast.KindComparisonOperation:
        co := e.(ast.ComparisonOperation)
        
        // Create results for both operands
        leftResult := &calculationResult{}
        rightResult := &calculationResult{}
        
        // Evaluate left operand
        ast.Walk(co.Left(), ast.PostOrder, leftResult, calculateConstantExpressionValue)
        if leftResult.valid != nil {
            cr.valid = leftResult.valid
            return
        }
        
        // Evaluate right operand
        ast.Walk(co.Right(), ast.PostOrder, rightResult, calculateConstantExpressionValue)
        if rightResult.valid != nil {
            cr.valid = rightResult.valid
            return
        }
        
        // Perform the comparison operation
        cr.value, cr.valid = performComparisonOperation(co.Operation(), leftResult.value, rightResult.value)

    default:
        cr.valid = eh.NewGeneralError(eh.Analyzer, failureMap, eh.Fatal, unknownExpressionKind, nil)
    }
}

// ...existing code...

func performUnaryOperation(op ast.UnaryOperator, operand any) (any, error) {
	fmt.Println(op, operand)
	return nil, nil
}

func performArithmeticOperation(op ast.ArithmeticOperator, left, right any) (any, error) {
	fmt.Println(op, left, right)

	l := left.(int64)
	r := right.(int64)

	switch op {
	case ast.Plus:
		return l + r, nil

	case ast.Minus:
		return l - r, nil

	case ast.Times:
		return l * r, nil

	case ast.Divide:
		return l / r, nil
	}

	return nil, nil
}

func performComparisonOperation(op ast.ComparisonOperator, left, right any) (any, error) {
	fmt.Println(op, left, right)
	return nil, nil
}
