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

// Implementation of the semantic analyzer. It performs semantic checks on the abstract syntax tree (e.g., name analysis or usage analysis).
type semanticAnalyzer struct {
	abstractSyntax ast.Block        // abstract syntax tree to run semantic analysis on
	tokenHandler   tok.TokenHandler // token handler that manages the tokens of the token stream
}

// Return the interface of the semantic analyzer implementation.
func newAnalyzer(abstractSyntax ast.Block, tokenHandler tok.TokenHandler) Analyzer {
	return &semanticAnalyzer{abstractSyntax: abstractSyntax, tokenHandler: tokenHandler}
}

// Setup built-in symbols like data types in the root block of the abstract syntax tree.
func (a *semanticAnalyzer) SetupBuiltInSymbols() {
	// get the root block of the abstract syntax tree to insert built-in data types into its scope
	rootBlock := a.abstractSyntax.RootBlock()

	// insert built-in data types into the scope of the root block
	int64Type := ts.NewSimpleTypeDescriptor(ts.Integer64)
	rootBlock.Insert(int64Type.Name(), sym.NewSymbol(int64Type.Name(), sym.DataTypeEntry, int64Type, nil))
}

// Perform the semantic analysis compiler phase on the abstract syntax tree.
func (a *semanticAnalyzer) PerformSemanticAnalysis() {
	// validate the state of the semantic analyzer and panic if it is invalid
	if a.abstractSyntax == nil || a.tokenHandler == nil {
		panic(eh.NewGeneralError(eh.Analyzer, failureMap, eh.Fatal, invalidNameAnalysisState, nil, nil))
	}

	// perform name analysis, enrich the abstract syntax tree, and fill in symbols from declarations into the scope of their blocks
	nameAnalyzer := NewNameAnalyzer(a.abstractSyntax, a.tokenHandler)
	nameAnalyzer.Accept()

	// determine captured declarations for all blocks in the abstract syntax tree
	// note: this must be done after name analysis so that all identifier uses refer to their declarations and symbols
	// note: even after name analysis, there might be identifier uses that do not refer to any declaration or symbol (i.e., undeclared identifiers)
	if err := ast.Walk(nameAnalyzer.abstractSyntax, ast.PreOrder, nil, addVariableToBlockClosure); err != nil {
		panic(eh.NewGeneralError(eh.Analyzer, failureMap, eh.Fatal, capturedVariableDeterminationFailed, nil, err))
	}
}

// This is a visitor function. It adds all variables that are used in this block but declared in a lexical parent to the captured declarations of the use-block.
func addVariableToBlockClosure(node ast.Node, _ any) {
	// only process identifier use nodes
	if node.Kind() != ast.KindIdentifierUse {
		return
	}

	// assert that the node is an identifier use node
	variableUse := node.(ast.IdentifierUse)

	// only process identifier uses that refer to a variable symbol so that the use is a variable use
	// note: for an identifier that was used but not declared, its symbol and declaration are nil
	if symbol := variableUse.Symbol(); symbol == nil || symbol.Kind != sym.VariableEntry {
		return
	}

	// get the declaration of the used variable and assert that it is a variable declaration
	variableDeclaration := variableUse.Declaration().(ast.VariableDeclaration)

	// determine if the variable use refers to a variable that is declared in a lexical parent block
	// and if so, add the variable declaration to the captured declarations of the block where the variable use occurs
	if variableUse.Depth() > variableDeclaration.Depth() {
		variableUse.CurrentBlock().AddCapturedDeclaration(variableDeclaration)
	}
}
