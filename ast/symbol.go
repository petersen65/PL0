// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// Empty scopes are required to use this number as their scope id
const EmptyScopeId = -1

// Kind of supported symbol entry as bit-mask.
const (
	ConstantEntry Entry = 1 << iota
	VariableEntry
	ProcedureEntry
)

type (
	// Kind of symbol entries (bit-mask).
	Entry uint64

	// Support for symbol extensions and scope extensions for compiler phases.
	ExtensionType int

	// A symbol is a data structure that stores all the necessary information related to a declared identifier that the compiler must know.
	Symbol struct {
		Name        string                `json:"name"`      // name of the symbol
		Kind        Entry                 `json:"kind"`      // kind of the symbol
		Declaration Declaration           `json:"-"`         // declaration node of the symbol
		Extension   map[ExtensionType]any `json:"extension"` // symbol extensions for compiler phases
	}

	// A scope is a data structure that stores information about its declared identifiers.
	// Scopes are nested from the outermost scope to the innermost scope. Each scope has exactly one outer scope.
	// Each declared identifier is associated with one specific scope. It is visible in its scope and all inner scopes. 
	// An identifier can be redeclared in an inner scope, which will shadow the outer declaration.
	Scope interface {
		NewIdentifier(prefix rune) string
		Insert(name string, symbol *Symbol)
		Lookup(name string) *Symbol
		LookupCurrent(name string) *Symbol
		IterateCurrent() <-chan *Symbol
	}
)

// Create a new entry for the symbol table.
func NewSymbol(name string, kind Entry, declaration Declaration) *Symbol {
	return newSymbol(name, kind, declaration)
}

// Create a new scope with an outer scope and an identifier that is unique across all compilation phases.
func NewScope(uniqueId int, outer Scope) Scope {
	return newScope(uniqueId, outer)
}

// An empty scope should only be used in the context of parser errors and is free from any side-effect.
func NewEmptyScope() Scope {
	return newScope(EmptyScopeId, nil)
}

// String representation of a symbol entry kind.
func (e Entry) String() string {
	return kindNames[e]
}
