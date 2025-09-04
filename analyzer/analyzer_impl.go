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

// Implementation of the semantic analyzer. It performs all semantic checks on the abstract syntax tree (e.g., name analysis or usage analysis).
type semanticAnalyzer struct {
	abstractSyntax ast.Block        // abstract syntax tree to run semantic analysis on
	tokenHandler   tok.TokenHandler // token handler that manages the tokens of the token stream
}

// Return the interface of the semantic analyzer implementation.
func newAnalyzer(abstractSyntax ast.Block, tokenHandler tok.TokenHandler) Analyzer {
	return &semanticAnalyzer{abstractSyntax: abstractSyntax, tokenHandler: tokenHandler}
}

// Analyze the abstract syntax tree for declaration and use errors and fill in symbols into into the scope of blocks.
func (a *semanticAnalyzer) Analyze() {
	// validate the state of the semantic analyzer and panic if it is invalid
	if a.abstractSyntax == nil || a.tokenHandler == nil {
		panic(eh.NewGeneralError(eh.Analyzer, failureMap, eh.Fatal, invalidNameAnalysisState, nil, nil))
	}

	// get the root block of the abstract syntax tree to insert built-in data types into its scope
	rootBlock := a.abstractSyntax.RootBlock()

	// insert built-in data types into the scope of the root block
	int64Type := ts.NewSimpleTypeDescriptor(ts.Integer64)
	rootBlock.Insert(int64Type.Name(), sym.NewSymbol(int64Type.Name(), sym.DataTypeEntry, int64Type, nil))

	// perform name analysis, enrich the abstract syntax tree, and fill in symbols into the scope of blocks
	nameAnalyzer := NewNameAnalyzer(a.abstractSyntax, a.tokenHandler)
	nameAnalyzer.Accept()

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
}

// Insert the symbol for a variable declaration into the current block's scope.
func (a *semanticAnalyzer) VisitVariableDeclaration(vd ast.VariableDeclaration) {
}

// Insert the symbol for a function declaration into the current block's scope.
func (a *semanticAnalyzer) VisitFunctionDeclaration(fd ast.FunctionDeclaration) {
}

// Walk the literal abstract syntax tree.
func (a *semanticAnalyzer) VisitLiteralUse(lu ast.LiteralUse) {
}

// Check if the used identifier is declared and if it is used as the expected kind of identifier.
func (a *semanticAnalyzer) VisitIdentifierUse(iu ast.IdentifierUse) {
}

// Walk the unary operation abstract syntax tree.
func (a *semanticAnalyzer) VisitUnaryOperation(uo ast.UnaryOperation) {
}

// Walk the binary operation abstract syntax tree.
func (a *semanticAnalyzer) VisitBinaryOperation(bo ast.BinaryOperation) {
}

// Walk the comparison operation abstract syntax tree.
func (a *semanticAnalyzer) VisitComparisonOperation(co ast.ComparisonOperation) {
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

// and creates a closure for accessing identifiers in lexical parents.
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
