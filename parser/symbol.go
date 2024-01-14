// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package parser

import "fmt"

const (
	constant = entry(iota)
	variable
	procedure
)

const (
	none = symbolType(iota)
	integer64
)

type (
	entry      int
	symbolType int

	symbol struct {
		name    string     // name of constant, variable, or procedure
		kind    entry      // constant, variable, or procedure
		depth   int32      // declaration nesting depth of constant, variable, or procedure
		value   any        // value of constant
		stype   symbolType // type of constant or variable
		offset  uint64     // offset of variable in its runtime procedure stack frame
		label   string     // label of procedure for assembly generation
		address uint64     // address of procedure in text section
	}

	symbolTable struct {
		symbols []symbol
	}
)

var kindNames = map[entry]string{
	constant:  "constant",
	variable:  "variable",
	procedure: "procedure",
}

func newSymbolTable() *symbolTable {
	return &symbolTable{
		symbols: make([]symbol, 0),
	}
}

func (s *symbolTable) addConstant(name string, depth int32, value any) {
	s.symbols = append(s.symbols, symbol{
		name:  name,
		kind:  constant,
		depth: depth,
		value: value,
		stype: integer64,
	})
}

func (s *symbolTable) addVariable(name string, depth int32, offset *uint64) {
	s.symbols = append(s.symbols, symbol{
		name:   name,
		kind:   variable,
		depth:  depth,
		offset: *offset,
		stype:  integer64,
	})

	*offset++
}

func (s *symbolTable) addProcedure(name string, depth int32, address uint64) {
	s.symbols = append(s.symbols, symbol{
		name:    name,
		kind:    procedure,
		depth:   depth,
		label:   fmt.Sprintf("_%v_%v", depth, name),
		address: address,
	})
}

func (s *symbolTable) find(name string) (symbol, bool) {
	for i := len(s.symbols) - 1; i >= 0; i-- {
		if s.symbols[i].name == name {
			return s.symbols[i], true
		}
	}

	return symbol{}, false
}

func (s *symbolTable) update(symbol symbol) bool {
	for i := len(s.symbols) - 1; i >= 0; i-- {
		if s.symbols[i].name == symbol.name {
			s.symbols[i] = symbol
			return true
		}
	}

	return false
}

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
