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

	// determine the closure of all blocks
	if err := ast.Walk(nameAnalyzer.abstractSyntax, ast.PreOrder, nil, addVariableToBlockClosure); err != nil {
		panic(eh.NewGeneralError(eh.Analyzer, failureMap, eh.Fatal, closureDeterminationFailed, nil, err))
	}
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
