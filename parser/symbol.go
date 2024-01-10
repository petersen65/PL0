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

type (
	entry int

	symbol struct {
		name    string // name of constant, variable, or procedure
		kind    entry  // constant, variable, or procedure
		depth   int32    // declaration nesting depth of variable or procedure
		value   int64  // value of constant
		offset  uint64 // offset of variable in its runtime procedure stack frame
		label   string // label of procedure for assembly generation
		address uint64 // address of procedure in code segment
	}

	symbolTable struct {
		symbols []symbol
	}
)

func (s *symbolTable) addConstant(name string, depth int32, value int64) {
	s.symbols = append(s.symbols, symbol{
		name:  name,
		kind:  constant,
		depth: depth,
		value: value,
	})
}

func (s *symbolTable) addVariable(name string, depth int32, offset *uint64) {
	s.symbols = append(s.symbols, symbol{
		name:   name,
		kind:   variable,
		depth:  depth,
		offset: *offset,
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
