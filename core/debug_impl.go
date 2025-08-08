// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package core

// Private implementation of the DebugInformation interface.
type debugInformation struct {
	tokenHandler TokenHandler // token handler that manages the tokens of the token stream
	table        *DebugStringTable
}

// Create a new debug information instance for a compilation unit.
func newDebugInformation(compilationUnit, producer string, optimized bool, tokenHandler TokenHandler) DebugInformation {
	return &debugInformation{
		tokenHandler: tokenHandler,
		table:        newDebugStringTable(compilationUnit, producer, optimized),
	}
}

// Create a new debug string table for a compilation unit.
func newDebugStringTable(compilationUnit, producer string, optimized bool) *DebugStringTable {
	return &DebugStringTable{
		CompilationUnit: compilationUnit,
		Producer:        producer,
		Optimized:       optimized,
		Functions:       make([]*FunctionDescription, 0),
	}
}

// Create a new function description for a function in the compilation unit.
func newFunctionDescription(name, nameSource string, tokenStreamIndex int) *FunctionDescription {
	return &FunctionDescription{
		Name:             name,
		NameSource:       nameSource,
		TokenStreamIndex: tokenStreamIndex,
		Variables:        make([]*VariableDescription, 0),
	}
}

// Create a new variable description for a variable in the compilation unit.
func newVariableDescription(function, functionSource, name, nameSource, dataType string, tokenStreamIndex int) *VariableDescription {
	return &VariableDescription{
		Function:         function,
		FunctionSource:   functionSource,
		Name:             name,
		NameSource:       nameSource,
		DataType:         dataType,
		TokenStreamIndex: tokenStreamIndex,
	}
}

// Append a function description to the debug information.
func (d *debugInformation) AppendFunction(name, nameSource string, tokenStreamIndex int) bool {
	for _, f := range d.table.Functions {
		if f.Name == name {
			return false
		}
	}

	d.table.Functions = append(d.table.Functions, newFunctionDescription(name, nameSource, tokenStreamIndex))
	return true
}

// Append a variable description to the debug information.
func (d *debugInformation) AppendVariable(function, functionSource, name, nameSource, dataType string, tokenStreamIndex int) bool {
	for _, f := range d.table.Functions {
		if f.Name == function {
			for _, v := range f.Variables {
				if v.Name == name {
					return false
				}
			}

			f.Variables = append(f.Variables, newVariableDescription(function, functionSource, name, nameSource, dataType, tokenStreamIndex))
			return true
		}
	}

	return false
}

// Return a full copy of the debug string table.
func (d *debugInformation) GetDebugStringTable() DebugStringTable {
	return *d.table
}

// Provide the source code context for a given token stream index including line and column information.
func (d *debugInformation) GetSourceCodeContext(tokenStreamIndex int) (line, column int, currentLine string, ok bool) {
	if d.tokenHandler == nil {
		return 0, 0, "", false
	}

	if tokenDescription, ok := d.tokenHandler.GetTokenDescription(tokenStreamIndex); !ok {
		return 0, 0, "", false
	} else {
		return tokenDescription.Line, tokenDescription.Column, string(tokenDescription.CurrentLine), true
	}
}
