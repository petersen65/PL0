// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package symbol provides a generic symbol table implementation for managing declared identifiers and scopes.
package symbol

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
	Symbol[T any] struct {
		Name        string                `json:"name"`      // name of the symbol
		Kind        Entry                 `json:"kind"`      // kind of the symbol
		Declaration T                     `json:"-"`         // declaration information of the symbol
		Extension   map[ExtensionType]any `json:"extension"` // symbol extensions for compiler phases
	}

	// A scope is a data structure that stores information about its declared identifiers.
	// Scopes are nested from the outermost scope to the innermost scope. Each scope has exactly one outer scope.
	// Each declared identifier is associated with one specific scope. It is visible in its scope and all inner scopes.
	// An identifier can be redeclared in an inner scope, which will shadow the outer declaration.
	Scope[T any] interface {
		NewIdentifier(prefix rune) string
		Insert(name string, symbol *Symbol[T])
		Lookup(name string) *Symbol[T]
		LookupCurrent(name string) *Symbol[T]
		IterateCurrent() <-chan *Symbol[T]
	}
)

// Create a new entry for the symbol table.
func NewSymbol[T any](name string, kind Entry, declaration T) *Symbol[T] {
	return newSymbol(name, kind, declaration)
}

// Create a new scope with an outer scope and an identifier that is unique across all compilation phases.
func NewScope[T any](uniqueId int, outer Scope[T]) Scope[T] {
	return newScope(uniqueId, outer)
}

// An empty scope should only be used in the context of parser errors and is free from any side-effect.
func NewEmptyScope[T any]() Scope[T] {
	return newScope[T](EmptyScopeId, nil)
}

// String representation of a symbol entry kind.
func (e Entry) String() string {
	return kindNames[e]
}
