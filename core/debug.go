// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package core

type (
	DebugStringTable struct {
		CompilationUnit string
		Producer        string
		Optimized       bool
		Functions       []*FunctionDescription
	}

	FunctionDescription struct {
		Name             string
		NameSource       string
		TokenStreamIndex int
		Variables        []*VariableDescription
	}

	VariableDescription struct {
		Name             string
		NameSource       string
		Function         string
		FunctionSource   string
		TokenStreamIndex int
	}

	DebugInformation interface {
		AppendFunction(name, nameSource string, tokenStreamIndex int) bool
		AppendVariable(function, functionSource, name, nameSource string, tokenStreamIndex int) bool
		GetDebugStringTable() DebugStringTable
		GetSourceCodeContext(tokenStreamIndex int) (line, column int, currentLine string, ok bool)
	}
)

func NewDebugInformation(compilationUnit, producer string, optimized bool, tokenHandler TokenHandler) DebugInformation {
	return newDebugInformation(compilationUnit, producer, optimized, tokenHandler)
}
