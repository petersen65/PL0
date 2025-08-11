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
		DataTypes:            make([]*DataTypeDescription, 0),
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
func newVariableDescription(function, functionSource, name, nameSource string, dataTypeDescription *DataTypeDescription, tokenStreamIndex int) *VariableDescription {
	return &VariableDescription{
		Function:         function,
		FunctionSource:   functionSource,
		Name:             name,
		NameSource:       nameSource,
		DataType:         dataTypeDescription,
		TokenStreamIndex: tokenStreamIndex,
	}
}

// Append a function description to the debug information.
func (d *debugInformation) AppendFunction(name, nameSource string, tokenStreamIndex int) bool {
	if slices.ContainsFunc(d.table.Functions, func(fd *FunctionDescription) bool { return fd.Name == name }) {
		return false
	}

	d.table.Functions = append(d.table.Functions, newFunctionDescription(name, nameSource, tokenStreamIndex))
	return true
}

// Append a variable description to the debug information.
func (d *debugInformation) AppendVariable(function, functionSource, name, nameSource, dataType, dataTypeSource string, tokenStreamIndex int) bool {
	if index := slices.IndexFunc(d.table.Functions, func(fd *FunctionDescription) bool { return fd.Name == function }); index == -1 {
		return false
	} else {
		fd := d.table.Functions[index]

		if slices.ContainsFunc(fd.Variables, func(vd *VariableDescription) bool { return vd.Name == name }) {
			return false
		}

		var dtd *DataTypeDescription

		if index := slices.IndexFunc(d.table.DataTypes, func(dtd *DataTypeDescription) bool { return dtd.Name == dataType }); index == -1 {
			dtd = &DataTypeDescription{Name: dataType, NameSource: dataTypeSource}
			d.table.DataTypes = append(d.table.DataTypes, dtd)
		} else {
			dtd = d.table.DataTypes[index]
		}
		
		vd := newVariableDescription(function, functionSource, name, nameSource, dtd, tokenStreamIndex)
		fd.Variables = append(fd.Variables, vd)
		d.table.Variables = append(d.table.Variables, vd)

		return true
	}
}

// Update the offset of a variable in the debug information.
func (d *debugInformation) UpdateVariable(name string, offset int32) bool {
	if index := slices.IndexFunc(d.table.Variables, func(vd *VariableDescription) bool { return vd.Name == name }); index != -1 {
		vd := d.table.Variables[index]
		vd.Offset = offset
		return true
	}

	return false
}

// Update the size of a data type in the debug information.
func (d *debugInformation) UpdateDataType(name string, size int32) bool {
	if index := slices.IndexFunc(d.table.DataTypes, func(dtd *DataTypeDescription) bool { return dtd.Name == name }); index != -1 {
		dtd := d.table.DataTypes[index]
		dtd.Size = size
		return true
	}

	return false
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
