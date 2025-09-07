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
	s.Insert(bitType.Name(), NewSymbol(bitType.Name(), DataTypeEntry, bitType, nil))

	int64Type := ts.NewSimpleTypeDescriptor(ts.Integer64)
	s.Insert(int64Type.Name(), NewSymbol(int64Type.Name(), DataTypeEntry, int64Type, nil))

	int32Type := ts.NewSimpleTypeDescriptor(ts.Integer32)
	s.Insert(int32Type.Name(), NewSymbol(int32Type.Name(), DataTypeEntry, int32Type, nil))

	int16Type := ts.NewSimpleTypeDescriptor(ts.Integer16)
	s.Insert(int16Type.Name(), NewSymbol(int16Type.Name(), DataTypeEntry, int16Type, nil))

	int8Type := ts.NewSimpleTypeDescriptor(ts.Integer8)
	s.Insert(int8Type.Name(), NewSymbol(int8Type.Name(), DataTypeEntry, int8Type, nil))

	float64Type := ts.NewSimpleTypeDescriptor(ts.Float64)
	s.Insert(float64Type.Name(), NewSymbol(float64Type.Name(), DataTypeEntry, float64Type, nil))

	float32Type := ts.NewSimpleTypeDescriptor(ts.Float32)
	s.Insert(float32Type.Name(), NewSymbol(float32Type.Name(), DataTypeEntry, float32Type, nil))

	uint64Type := ts.NewSimpleTypeDescriptor(ts.Unsigned64)
	s.Insert(uint64Type.Name(), NewSymbol(uint64Type.Name(), DataTypeEntry, uint64Type, nil))

	uint32Type := ts.NewSimpleTypeDescriptor(ts.Unsigned32)
	s.Insert(uint32Type.Name(), NewSymbol(uint32Type.Name(), DataTypeEntry, uint32Type, nil))

	uint16Type := ts.NewSimpleTypeDescriptor(ts.Unsigned16)
	s.Insert(uint16Type.Name(), NewSymbol(uint16Type.Name(), DataTypeEntry, uint16Type, nil))

	uint8Type := ts.NewSimpleTypeDescriptor(ts.Unsigned8)
	s.Insert(uint8Type.Name(), NewSymbol(uint8Type.Name(), DataTypeEntry, uint8Type, nil))

	boolType := ts.NewSimpleTypeDescriptor(ts.Boolean)
	s.Insert(boolType.Name(), NewSymbol(boolType.Name(), DataTypeEntry, boolType, nil))

	charType := ts.NewSimpleTypeDescriptor(ts.Character)
	s.Insert(charType.Name(), NewSymbol(charType.Name(), DataTypeEntry, charType, nil))

	stringType := ts.NewSimpleTypeDescriptor(ts.String)
	s.Insert(stringType.Name(), NewSymbol(stringType.Name(), DataTypeEntry, stringType, nil))
}
