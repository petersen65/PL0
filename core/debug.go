// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package core

type (
	// DebugStringTable holds generic debug information for a compilation unit.
	DebugStringTable struct {
		CompilationUnit string                 `json:"compilation_unit"` // name of the compilation unit (e.g., source code file name)
		Producer        string                 `json:"producer"`         // name of the producer (e.g., compiler name and its version)
		Optimized       bool                   `json:"optimized"`        // whether the code is optimized
		Functions       []*FunctionDescription `json:"functions"`        // list of functions in the compilation unit
	}

	// FunctionDescription holds information about a function in the compilation unit.
	FunctionDescription struct {
		Name             string                 `json:"name"`               // name of the function in the compilation unit
		NameSource       string                 `json:"name_source"`        // name of the function in the source code
		TokenStreamIndex int                    `json:"token_stream_index"` // index of the token stream for the function (e.g., line, column)
		Variables        []*VariableDescription `json:"variables"`          // list of variables in the function
	}

	// VariableDescription holds information about a variable in the compilation unit.
	VariableDescription struct {
		Name             string `json:"name"`               // name of the variable in the compilation unit
		NameSource       string `json:"name_source"`        // name of the variable in the source code
		Function         string `json:"function"`           // name of the function containing the variable
		FunctionSource   string `json:"function_source"`    // name of the function in the source code containing the variable
		DataType         string `json:"data_type"`          // data type of the variable
		TokenStreamIndex int    `json:"token_stream_index"` // index of the token stream for the variable (e.g., line, column)
	}

	// DebugInformation provides methods to collect and retrieve debug information.
	DebugInformation interface {
		AppendFunction(name, nameSource string, tokenStreamIndex int) bool
		AppendVariable(function, functionSource, name, nameSource, dataType string, tokenStreamIndex int) bool
		GetDebugStringTable() DebugStringTable
		GetSourceCodeContext(tokenStreamIndex int) (line, column int, currentLine string, ok bool)
	}
)

// Create a new debug information instance for a compilation unit.
func NewDebugInformation(compilationUnit, producer string, optimized bool, tokenHandler TokenHandler) DebugInformation {
	return newDebugInformation(compilationUnit, producer, optimized, tokenHandler)
}
