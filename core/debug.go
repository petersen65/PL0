// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package core

type (
	DebugStringTable struct {
		CompilationUnit string
		Functions       []*FunctionDescription
	}

	FunctionDescription struct {
		Name       string
		NameSource string
		Variables  []*VariableDescription
	}

	VariableDescription struct {
		Name             string
		NameSource       string
		Function         string
		FunctionSource   string
		TokenStreamIndex int
	}

	DebugInformation interface {
		AppendFunction(name, nameSource string) bool
		AppendVariable(function, functionSource, name, nameSource string, tokenStreamIndex int) bool
		GetDebugStringTable() DebugStringTable
	}
)

func NewDebugInformation(compilationUnit string, tokenHandler TokenHandler) DebugInformation {
	return newDebugInformation(compilationUnit, tokenHandler)
}
