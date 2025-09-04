// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import sym "github.com/petersen65/pl0/v3/symbol"

// Kind of a used identifier as bit-mask.
const (
	Constant IdentifierKind = 1 << iota
	Variable
	Function
	Procedure
)

// Usage mode of an identifier as bit-mask.
const (
	Read UsageMode = 1 << iota
	Write
	Execute
)

type (
	// Describe how an identifier is used.
	UsageMode uint64

	// Describe the kind of the used identifier.
	IdentifierKind uint64

	// An identifier-use node in the abstract syntax tree.
	IdentifierUse interface {
		Expression
		Depth() int
		Name() string
		Context() IdentifierKind
		SetContext(kind IdentifierKind)
		UsageMode() UsageMode
		SetUsageMode(mode UsageMode)
		Declaration() Declaration
		Symbol() *sym.Symbol
	}
)

// Create a new identifier-use node in the abstract syntax tree.
func NewIdentifierUse(name string, kind IdentifierKind, index int) IdentifierUse {
	return newIdentifierUse(name, kind, index)
}
