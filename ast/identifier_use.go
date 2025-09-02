// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

// Kind of a used identifier as bit-mask.
const (
	Constant IdentifierKind = 1 << iota
	Variable
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

	// Represents a single use of an identifier.
	IdentifierUseNode struct {
		commonNode                    // embedded common node
		expressionNode                // embedded expression node
		Name           string         `json:"name"`            // name of the identifier
		IdentifierKind IdentifierKind `json:"identifier_kind"` // kind of the identifier used
		UsageMode      UsageMode      `json:"usage_mode"`      // usage mode of the identifier
	}
)

// NewIdentifierUse creates a new identifier-use node in the abstract syntax tree.
func NewIdentifierUse(name string, context IdentifierKind, index int) Expression {
	return newIdentifierUse(name, context, index)
}
