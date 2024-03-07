// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package parser

import "github.com/petersen65/PL0/parser/ast"

// The symbol table is a table that stores all symbols of the program.
type symbolTable struct {
	symbols []ast.Symbol
}

// KindNames maps symbol kinds to their string representation.
var kindNames = map[ast.Entry]string{
	ast.Constant:  "constant",
	ast.Variable:  "variable",
	ast.Procedure: "procedure",
}

// Create a new symbol table for the PL/0 parser.
func newSymbolTable() *symbolTable {
	return &symbolTable{
		symbols: make([]ast.Symbol, 0),
	}
}

// Add a constant symbol to the symbol table.
func (s *symbolTable) addConstant(name string, depth int32, value int64) {
	s.symbols = append(s.symbols, ast.Symbol{
		Name:  name,
		Kind:  ast.Constant,
		Depth: depth,
		Value: value,
	})
}

// Add a variable symbol to the symbol table.
func (s *symbolTable) addVariable(name string, depth int32, offset *uint64) {
	s.symbols = append(s.symbols, ast.Symbol{
		Name:   name,
		Kind:   ast.Variable,
		Depth:  depth,
		Offset: *offset,
	})

	*offset++
}

// Add a procedure symbol to the symbol table.
func (s *symbolTable) addProcedure(name string, depth int32, address uint64) {
	s.symbols = append(s.symbols, ast.Symbol{
		Name:    name,
		Kind:    ast.Procedure,
		Depth:   depth,
		Address: address,
	})
}

// Find first symbol in the symbol table by searching from top to bottom.
func (s *symbolTable) find(name string) (ast.Symbol, bool) {
	for i := len(s.symbols) - 1; i >= 0; i-- {
		if s.symbols[i].Name == name {
			return s.symbols[i], true
		}
	}

	return ast.Symbol{}, false
}

// Update first found symbol in the symbol table by searching from top to bottom.
func (s *symbolTable) update(symbol ast.Symbol) bool {
	for i := len(s.symbols) - 1; i >= 0; i-- {
		if s.symbols[i].Name == symbol.Name {
			s.symbols[i] = symbol
			return true
		}
	}

	return false
}

// Remove all symbols from the symbol table that are declared at the given declaration depth.
func (s *symbolTable) remove(depth int32) []ast.Symbol {
	var removed = make([]ast.Symbol, 0)
	var filtered = make([]ast.Symbol, 0, len(s.symbols))

	for _, sym := range s.symbols {
		if sym.Depth == depth {
			removed = append(removed, sym)
		} else {
			filtered = append(filtered, sym)
		}
	}

	// Update the symbol table only if any symbols were removed.
	if len(removed) > 0 {
		s.symbols = filtered
	}

	return removed
}
