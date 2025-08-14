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

// Create a new data type of a specific kind.
func newDataType(name, nameSource string, kind DataTypeKind) DataTypeDescription {
	switch kind {
	case DataTypeSimple:
		return &SimpleDataType{
			TypeName:       name,
			TypeNameSource: nameSource,
		}

	case DataTypeComposite:
		return &CompositeDataType{
			SimpleDataType: SimpleDataType{
				TypeName:       name,
				TypeNameSource: nameSource,
			},
			CompositeMembers: make([]*DataTypeMember, 0),
		}

	default:
		return nil
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
		DataTypes:            make([]DataTypeDescription, 0),
	}
}

// Create a new function description for a function in the compilation unit.
func newFunctionDescription(name, nameSource string, tokenStreamIndex int) *FunctionDescription {
	return &FunctionDescription{
		FunctionName:       name,
		FunctionNameSource: nameSource,
		TokenStreamIndex:   tokenStreamIndex,
		Variables:          make([]*VariableDescription, 0),
	}
}

// Create a new variable description for a variable in the compilation unit.
func newVariableDescription(function, functionSource, name, nameSource string, dataType DataTypeDescription, tokenStreamIndex int) *VariableDescription {
	return &VariableDescription{
		VariableName:       name,
		VariableNameSource: nameSource,
		FunctionName:       function,
		FunctionNameSource: functionSource,
		Type:               dataType,
		TokenStreamIndex:   tokenStreamIndex,
	}
}

// Append a function description to the debug information.
func (d *debugInformation) AppendFunction(name, nameSource string, tokenStreamIndex int) bool {
	if slices.ContainsFunc(d.table.Functions, func(fd *FunctionDescription) bool { return fd.FunctionName == name }) {
		return false
	}

	d.table.Functions = append(d.table.Functions, newFunctionDescription(name, nameSource, tokenStreamIndex))
	return true
}

// Append a variable description to the debug information.
func (d *debugInformation) AppendVariable(function, functionSource, name, nameSource string, dataType DataTypeDescription, tokenStreamIndex int) bool {
	// find the function
	index := slices.IndexFunc(d.table.Functions, func(fd *FunctionDescription) bool { return fd.FunctionName == function })

	// function not found
	if index == -1 {
		return false
	}

	// extract function description
	fd := d.table.Functions[index]

	// check if variable already exists in function
	if slices.ContainsFunc(fd.Variables, func(vd *VariableDescription) bool { return vd.VariableName == name }) {
		return false
	}

	// create the variable description
	vd := newVariableDescription(function, functionSource, name, nameSource, dataType, tokenStreamIndex)

	// add to both function's variable list and global variable list
	fd.Variables = append(fd.Variables, vd)
	d.table.Variables = append(d.table.Variables, vd)

	return true
}

// Append a data type to the debug information.
func (d *debugInformation) AppendDataType(dataType DataTypeDescription) bool {
	// find the data type
	index := slices.IndexFunc(d.table.DataTypes, func(dtd DataTypeDescription) bool { return dtd.Name() == dataType.Name() })

	// data type found
	if index != -1 {
		return false
	}

	// append the new data type
	d.table.DataTypes = append(d.table.DataTypes, dataType)
	return true
}

// Update the offset of a variable in the debug information.
func (d *debugInformation) UpdateVariable(name string, offset int32) bool {
	if index := slices.IndexFunc(d.table.Variables, func(vd *VariableDescription) bool { return vd.VariableName == name }); index != -1 {
		vd := d.table.Variables[index]
		vd.Offset = offset
		return true
	}

	return false
}

// Update the byte size and base type encoding of a data type in the debug information.
func (d *debugInformation) UpdateDataType(name string, size int32, encoding int) bool {
	if index := slices.IndexFunc(d.table.DataTypes, func(dtd DataTypeDescription) bool { return dtd.Name() == name }); index != -1 {
		switch dt := d.table.DataTypes[index].(type) {
		case *SimpleDataType:
			dt.ByteSize = size
			dt.BaseTypeEncoding = encoding

		case *CompositeDataType:
			dt.ByteSize = size
			dt.BaseTypeEncoding = encoding
		}

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

// Append a member to a composite data type.
func (c *CompositeDataType) AppendMember(name, nameSource string, dataType DataTypeDescription) bool {
	// check if member already exists
	if slices.ContainsFunc(c.CompositeMembers, func(m *DataTypeMember) bool { return m.MemberName == name }) {
		return false
	}

	// create and append the member
	member := &DataTypeMember{MemberName: name, MemberNameSource: nameSource, Type: dataType}
	member.Order = len(c.CompositeMembers)
	c.CompositeMembers = append(c.CompositeMembers, member)

	return true
}
