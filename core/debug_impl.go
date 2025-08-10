// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package core

import "slices"

// Private implementation of the DebugInformation interface.
type debugInformation struct {
	tokenHandler TokenHandler // token handler that manages the tokens of the token stream
	table        *DebugStringTable
}

// Create a new debug information instance for a compilation unit.
func newDebugInformation(compilationUnit, compilationDirectory, producer string, optimized bool, tokenHandler TokenHandler) DebugInformation {
	return &debugInformation{
		tokenHandler: tokenHandler,
		table:        newDebugStringTable(compilationUnit, compilationDirectory, producer, optimized),
	}
}

// Create a new debug string table for a compilation unit.
func newDebugStringTable(compilationUnit, compilationDirectory, producer string, optimized bool) *DebugStringTable {
	return &DebugStringTable{
		CompilationUnit:      compilationUnit,
		CompilationDirectory: compilationDirectory,
		Producer:             producer,
		Optimized:            optimized,
		Functions:            make([]*FunctionDescription, 0),
		Variables:            make([]*VariableDescription, 0),
		DataTypes:            make([]string, 0),
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
	if index := slices.IndexFunc(d.table.Functions, func(f *FunctionDescription) bool { return f.Name == function }); index == -1 {
		return false
	} else {
		fd := d.table.Functions[index]

		if slices.ContainsFunc(fd.Variables, func(v *VariableDescription) bool { return v.Name == name }) {
			return false
		}

		vd := newVariableDescription(function, functionSource, name, nameSource, dataType, tokenStreamIndex)
		fd.Variables = append(fd.Variables, vd)
		d.table.Variables = append(d.table.Variables, vd)

		if !slices.ContainsFunc(d.table.DataTypes, func(dt string) bool { return dt == dataType }) {
			d.table.DataTypes = append(d.table.DataTypes, dataType)
		}

		return true
	}
}

// Return a full deep copy of the debug string table.
func (d *debugInformation) GetDebugStringTable() DebugStringTable {
	return *d.table
}

// Provide the source code context for a given token stream index including line and column information.
func (d *debugInformation) GetSourceCodeContext(tokenStreamIndex int) (int, int, string, bool) {
	if d.tokenHandler == nil {
		return 0, 0, "", false
	}

	if tokenDescription, ok := d.tokenHandler.GetTokenDescription(tokenStreamIndex); !ok {
		return 0, 0, "", false
	} else {
		return tokenDescription.Line, tokenDescription.Column, string(tokenDescription.CurrentLine), true
	}
}
