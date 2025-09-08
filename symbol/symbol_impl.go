// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package symbol

import ts "github.com/petersen65/pl0/v3/typesystem"

type (
	// A symbol table is a data structure that stores a mapping of the symbol name to the symbol.
	symbolTable struct {
		Names   []string           `json:"names"`   // enable deterministic iteration over the symbol table
		Symbols map[string]*Symbol `json:"symbols"` // mapping of symbol names to symbols
	}

	// Implementation of the scope data structure.
	scope struct {
		Outer       *scope       `json:"-"`            // outer scope or nil if this is the outermost scope
		Inner       []*scope     `json:"inner"`        // inner scopes nested within this scope
		SymbolTable *symbolTable `json:"symbol_table"` // symbol table of the scope
	}
)

var (
	// Map symbol kinds to their string representation.
	kindNames = map[EntryKind]string{
		ConstantEntry:  "constant",
		VariableEntry:  "variable",
		FunctionEntry:  "function",
		ProcedureEntry: "procedure",
		DataTypeEntry:  "data_type",
	}
)

// Create a new entry for the symbol table.
func newSymbol(name string, kind EntryKind, dataType ts.TypeDescriptor, value any) *Symbol {
	return &Symbol{
		Name:      name,
		Kind:      kind,
		DataType:  dataType,
		Value:     value,
		Extension: make(map[ExtensionType]any),
	}
}

// Create a new symbol table.
func newSymbolTable() *symbolTable {
	return &symbolTable{
		Names:   make([]string, 0),
		Symbols: make(map[string]*Symbol),
	}
}

// Create a new scope with an outer scope. The outer scope can be nil. For each outermost scope, all built-in symbols are added.
func newScope(outer Scope) Scope {
	var outerScope *scope

	if outer != nil {
		outerScope = outer.(*scope)
	}

	inner := &scope{
		Outer:       outerScope,
		Inner:       make([]*scope, 0),
		SymbolTable: newSymbolTable(),
	}

	if outerScope != nil {
		outerScope.Inner = append(outerScope.Inner, inner)
	}

	inner.setupBuiltInSymbols()
	return inner
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

// Insert the given symbol into this scope's symbol table under the provided name.
// If a symbol with that name already exists, it will be replaced.
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

// Setup all built-in symbols for the scope.
func (s *scope) setupBuiltInSymbols() {
	bitType := ts.NewSimpleTypeDescriptor(ts.Bit)
	s.Insert(bitType.String(), NewSymbol(bitType.String(), DataTypeEntry, bitType, nil))

	int64Type := ts.NewSimpleTypeDescriptor(ts.Integer64)
	s.Insert(int64Type.String(), NewSymbol(int64Type.String(), DataTypeEntry, int64Type, nil))

	int32Type := ts.NewSimpleTypeDescriptor(ts.Integer32)
	s.Insert(int32Type.String(), NewSymbol(int32Type.String(), DataTypeEntry, int32Type, nil))

	int16Type := ts.NewSimpleTypeDescriptor(ts.Integer16)
	s.Insert(int16Type.String(), NewSymbol(int16Type.String(), DataTypeEntry, int16Type, nil))

	int8Type := ts.NewSimpleTypeDescriptor(ts.Integer8)
	s.Insert(int8Type.String(), NewSymbol(int8Type.String(), DataTypeEntry, int8Type, nil))

	float64Type := ts.NewSimpleTypeDescriptor(ts.Float64)
	s.Insert(float64Type.String(), NewSymbol(float64Type.String(), DataTypeEntry, float64Type, nil))

	float32Type := ts.NewSimpleTypeDescriptor(ts.Float32)
	s.Insert(float32Type.String(), NewSymbol(float32Type.String(), DataTypeEntry, float32Type, nil))

	uint64Type := ts.NewSimpleTypeDescriptor(ts.Unsigned64)
	s.Insert(uint64Type.String(), NewSymbol(uint64Type.String(), DataTypeEntry, uint64Type, nil))

	uint32Type := ts.NewSimpleTypeDescriptor(ts.Unsigned32)
	s.Insert(uint32Type.String(), NewSymbol(uint32Type.String(), DataTypeEntry, uint32Type, nil))

	uint16Type := ts.NewSimpleTypeDescriptor(ts.Unsigned16)
	s.Insert(uint16Type.String(), NewSymbol(uint16Type.String(), DataTypeEntry, uint16Type, nil))

	uint8Type := ts.NewSimpleTypeDescriptor(ts.Unsigned8)
	s.Insert(uint8Type.String(), NewSymbol(uint8Type.String(), DataTypeEntry, uint8Type, nil))

	boolType := ts.NewSimpleTypeDescriptor(ts.Boolean)
	s.Insert(boolType.String(), NewSymbol(boolType.String(), DataTypeEntry, boolType, nil))

	charType := ts.NewSimpleTypeDescriptor(ts.Character)
	s.Insert(charType.String(), NewSymbol(charType.String(), DataTypeEntry, charType, nil))

	stringType := ts.NewSimpleTypeDescriptor(ts.String)
	s.Insert(stringType.String(), NewSymbol(stringType.String(), DataTypeEntry, stringType, nil))
}
