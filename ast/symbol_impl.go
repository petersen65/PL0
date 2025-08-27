// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import "fmt"

type (
	// A symbol table is a data structure that stores a mapping of the symbol name to the symbol.
	symbolTable struct {
		Names   []string           `json:"names"`   // enable deterministic iteration over the symbol table
		Symbols map[string]*Symbol `json:"symbols"` // mapping of symbol names to symbols
	}

	// Implementation of the scope data structure.
	scope struct {
		Id                int           `json:"id"`                 // each scope has a unique identifier
		Outer             *scope        `json:"outer"`              // outer scope or nil if this is the outermost scope
		SymbolTable       *symbolTable  `json:"symbol_table"`       // symbol table of the scope
		IdentifierCounter map[rune]uint `json:"identifier_counter"` // counter for compiler-generated unique identifier names
	}
)

var (
	// Map symbol kinds to their string representation.
	kindNames = map[Entry]string{
		ConstantEntry:  "constant",
		VariableEntry:  "variable",
		ProcedureEntry: "procedure",
	}
)

// Create a new entry for the symbol table.
func newSymbol(name string, kind Entry, declaration Declaration) *Symbol {
	return &Symbol{
		Name:        name,
		Kind:        kind,
		Declaration: declaration,
		Extension:   make(map[ExtensionType]any),
	}
}

// Create a new symbol table.
func newSymbolTable() *symbolTable {
	return &symbolTable{
		Names:   make([]string, 0),
		Symbols: make(map[string]*Symbol),
	}
}

// Create a new scope with an outer scope and an identifier that is unique across all compilation phases.
func newScope(uniqueId int, outer Scope) Scope {
	return &scope{
		Id:                uniqueId,
		Outer:             outer.(*scope),
		SymbolTable:       newSymbolTable(),
		IdentifierCounter: make(map[rune]uint),
	}
}

// Insert a symbol into the symbol table. If the symbol already exists, it will be overwritten.
func (s *symbolTable) insert(name string, symbol *Symbol) {
	if s.lookup(name) == nil {
		s.Names = append(s.Names, name)
	}

	s.Symbols[name] = symbol
}

// Lookup a symbol in the symbol table. If the symbol is not found, nil is returned.
func (s *symbolTable) lookup(name string) *Symbol {
	return s.Symbols[name]
}

// Deterministically iterate over all symbols in the symbol table.
func (s *symbolTable) iterate() <-chan *Symbol {
	symbols := make(chan *Symbol)

	// launch a goroutine to iterate over the symbol table in the background
	go func() {
		for _, name := range s.Names {
			symbols <- s.Symbols[name]
		}

		close(symbols)
	}()

	return symbols
}

// Create a new compiler-generated unique identifier name for a scope.
func (s *scope) NewIdentifier(prefix rune) string {
	if _, ok := s.IdentifierCounter[prefix]; !ok {
		s.IdentifierCounter[prefix] = 0
	}

	s.IdentifierCounter[prefix]++
	return fmt.Sprintf("%c%v.%v", prefix, s.Id, s.IdentifierCounter[prefix])
}

// Insert the given symbol into this scope's symbol table under the provided name. If a symbol with that name already exists, it will be replaced.
func (s *scope) Insert(name string, symbol *Symbol) {
	s.SymbolTable.insert(name, symbol)
}

// Lookup a symbol in the symbol table of the scope. If the symbol is not found, the outer scope is searched.
func (s *scope) Lookup(name string) *Symbol {
	if symbol := s.LookupCurrent(name); symbol != nil {
		return symbol
	}

	if s.Outer != nil {
		return s.Outer.Lookup(name)
	}

	return nil
}

// Lookup a symbol in the symbol table of the current scope. If the symbol is not found, nil is returned.
func (s *scope) LookupCurrent(name string) *Symbol {
	if symbol := s.SymbolTable.lookup(name); symbol != nil {
		return symbol
	}

	return nil
}

// Deterministically iterate over all symbols in the symbol table of the current scope.
func (s *scope) IterateCurrent() <-chan *Symbol {
	return s.SymbolTable.iterate()
}
