// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package symbol provides a generic symbol table implementation for managing declared identifiers and scopes.
package symbol

import ts "github.com/petersen65/pl0/v3/typesystem"

// Empty scopes are required to use this number as their scope id
const EmptyScopeId = -1

// Kind of supported symbol entry.
const (
	ConstantEntry EntryKind = iota
	VariableEntry
	FunctionEntry
	ProcedureEntry
	DataTypeEntry
)

type (
	// Kind of symbol entry.
	EntryKind int

	// Support for symbol extensions and scope extensions for compiler phases.
	ExtensionType int

	// A symbol is a data structure that stores all the necessary information related to a declared identifier that the compiler must know.
	Symbol struct {
		Name      string                `json:"name"`      // name of the symbol
		Kind      EntryKind             `json:"kind"`      // kind of the symbol
		DataType  ts.TypeDescriptor     `json:"data_type"` // data type information of the symbol
		Value     any                   `json:"value"`     // value information of a constant entry
		Extension map[ExtensionType]any `json:"-"`         // symbol extensions for compiler phases
	}

	// A scope is a data structure that stores information about its declared identifiers.
	// Scopes are nested from the outermost scope to the innermost scope. Each scope has exactly one outer scope.
	// Each declared identifier is associated with one specific scope. It is visible in its scope and all inner scopes.
	// An identifier can be redeclared in an inner scope, which will shadow the outer declaration.
	Scope interface {
		Insert(name string, symbol *Symbol)
		Lookup(name string) *Symbol
		LookupCurrent(name string) *Symbol
		IterateCurrent() <-chan *Symbol
	}
)

// Create a new symbol to represent a declared identifier.
func NewSymbol(name string, kind EntryKind, dataType ts.TypeDescriptor, value any) *Symbol {
	return newSymbol(name, kind, dataType, value)
}

// Create a new scope with an outer scope. The outer scope can be nil. For each outermost scope, all built-in symbols are added.
func NewScope(outer Scope) Scope {
	return newScope(outer)
}

// String representation of an entry kind.
func (e EntryKind) String() string {
	return kindNames[e]
}
