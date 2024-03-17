// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package analyzer implements semantic analysis compiler passes by traversing the abstract syntax tree.
package analyzer

import ast "github.com/petersen65/PL0/ast"

// NameAnalysis is the public interface of the name analyzer implementation.
type NameAnalysis interface {
	Analyze()
}

// Return the public interface of the private name analyzer implementation.
func NewNameAnalysis(abstractSyntax ast.Block) NameAnalysis {
	return newNameAnalysis(abstractSyntax)
}
