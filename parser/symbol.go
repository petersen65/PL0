// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package parser

// Kind of supported symbol table entry.
const (
	constant = entry(iota)
	variable
	procedure
)

type (
	// Kind of symbol table entries.
	entry int

	// The symbol table entry.
	symbol struct {
		name    string // name of constant, variable, or procedure
		kind    entry  // constant, variable, or procedure
		depth   int32  // declaration nesting depth of constant, variable, or procedure
		value   int64 // value of constant
		offset  uint64 // offset of variable in its runtime procedure stack frame
		address uint64 // absolute address of procedure in text section
	}

	// The symbol table is a table that stores all symbols of the program.
	symbolTable struct {
		symbols []symbol
	}
)

// KindNames maps symbol kinds to their string representation.
var kindNames = map[entry]string{
	constant:  "constant",
	variable:  "variable",
	procedure: "procedure",
}

// Create a new symbol table for the PL/0 parser.
func newSymbolTable() *symbolTable {
	return &symbolTable{
		symbols: make([]symbol, 0),
	}
}

// Add a constant symbol to the symbol table.
func (s *symbolTable) addConstant(name string, depth int32, value int64) {
	s.symbols = append(s.symbols, symbol{
		name:  name,
		kind:  constant,
		depth: depth,
		value: value,
	})
}

// Add a variable symbol to the symbol table.
func (s *symbolTable) addVariable(name string, depth int32, offset *uint64) {
	s.symbols = append(s.symbols, symbol{
		name:   name,
		kind:   variable,
		depth:  depth,
		offset: *offset,
	})

	*offset++
}

// Add a procedure symbol to the symbol table.
func (s *symbolTable) addProcedure(name string, depth int32, address uint64) {
	s.symbols = append(s.symbols, symbol{
		name:    name,
		kind:    procedure,
		depth:   depth,
		address: address,
	})
}

// Find first symbol in the symbol table by searching from top to bottom.
func (s *symbolTable) find(name string) (symbol, bool) {
	for i := len(s.symbols) - 1; i >= 0; i-- {
		if s.symbols[i].name == name {
			return s.symbols[i], true
		}
	}

	return symbol{}, false
}

// Update first found symbol in the symbol table by searching from top to bottom.
func (s *symbolTable) update(symbol symbol) bool {
	for i := len(s.symbols) - 1; i >= 0; i-- {
		if s.symbols[i].name == symbol.name {
			s.symbols[i] = symbol
			return true
		}
	}

	return false
}

// Remove all symbols from the symbol table that are declared at the given declaration depth.
func (s *symbolTable) remove(depth int32) {
	var filtered []symbol = make([]symbol, 0, len(s.symbols))

	for _, sym := range s.symbols {
		if sym.depth != depth {
			filtered = append(filtered, sym)
		}
	}

	if len(filtered) > 0 {
		s.symbols = filtered
	}
}
