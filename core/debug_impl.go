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
func newDebugInformation(compilationUnit, compilationDirectory, producer, stringName string, optimized bool, tokenHandler TokenHandler) DebugInformation {
	return &debugInformation{
		tokenHandler: tokenHandler,
		table:        newDebugStringTable(compilationUnit, compilationDirectory, producer, stringName, optimized),
	}
}

// Create a new simple data type whose size and encoding will be set later.
func newSimpleDataType(name, nameSource string) *SimpleDataType {
	return &SimpleDataType{
		TypeName:       name,
		TypeNameSource: nameSource,
	}
}

// Create a new composite data type whose size and members will be set later.
func newCompositeDataType(name, nameSource string) *CompositeDataType {
	return &CompositeDataType{
		TypeName:         name,
		TypeNameSource:   nameSource,
		CompositeMembers: make([]*CompositeMember, 0),
	}
}

// Create a new pointer data type whose size will be set later.
func newPointerDataType(name, nameSource string, elementType DataTypeDescription) *PointerDataType {
	return &PointerDataType{
		TypeName:       name,
		TypeNameSource: nameSource,
		ElementType:    elementType,
	}
}

// Create a new debug string table for a compilation unit.
func newDebugStringTable(compilationUnit, compilationDirectory, producer, stringName string, optimized bool) *DebugStringTable {
	return &DebugStringTable{
		CompilationUnit:      compilationUnit,
		CompilationDirectory: compilationDirectory,
		Producer:             producer,
		String:               stringName,
		Optimized:            optimized,
		Functions:            make([]*FunctionDescription, 0),
		Variables:            make([]*VariableDescription, 0),
		DataTypes:            make([]DataTypeDescription, 0),
	}
}

// Create a new function description for a function in the compilation unit.
func newFunctionDescription(name, nameSource string, globalSymbol bool, tokenStreamIndex int) *FunctionDescription {
	return &FunctionDescription{
		FunctionName:       name,
		FunctionNameSource: nameSource,
		GlobalSymbol:       globalSymbol,
		TokenStreamIndex:   tokenStreamIndex,
		Variables:          make([]*VariableDescription, 0),
	}
}

// Create a new constant description for a constant in the compilation unit.
func newConstantDescription(function, functionSource, name, nameSource string, dataType DataTypeDescription, value any, tokenStreamIndex int) *ConstantDescription {
	return &ConstantDescription{
		ConstantName:       name,
		ConstantNameSource: nameSource,
		FunctionName:       function,
		FunctionNameSource: functionSource,
		Type:               dataType,
		Value:              value,
		TokenStreamIndex:   tokenStreamIndex,
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
func (d *debugInformation) AppendFunction(name, nameSource string, globalSymbol bool, tokenStreamIndex int) bool {
	if slices.ContainsFunc(d.table.Functions, func(fd *FunctionDescription) bool { return fd.FunctionName == name }) {
		return false
	}

	d.table.Functions = append(d.table.Functions, newFunctionDescription(name, nameSource, globalSymbol, tokenStreamIndex))
	return true
}

// Append a constant description to the debug information.
func (d *debugInformation) AppendConstant(function, functionSource, name, nameSource string, dataType DataTypeDescription, value any, tokenStreamIndex int) bool {
	// find the function
	index := slices.IndexFunc(d.table.Functions, func(fd *FunctionDescription) bool { return fd.FunctionName == function })

	// function not found
	if index == -1 {
		return false
	}

	// extract function description
	fd := d.table.Functions[index]

	// check if the constant already exists in the function
	if slices.ContainsFunc(fd.Constants, func(cd *ConstantDescription) bool { return cd.ConstantName == name }) {
		return false
	}

	// check if the data type already exists and append it if not
	if index := slices.IndexFunc(d.table.DataTypes, func(dtd DataTypeDescription) bool { return dtd.Name() == dataType.Name() }); index == -1 {
		d.AppendDataType(dataType)
	} else {
		dataType = d.table.DataTypes[index]
	}

	// create the constant description
	cd := newConstantDescription(function, functionSource, name, nameSource, dataType, value, tokenStreamIndex)

	// add to both function's constant list and global constant list
	fd.Constants = append(fd.Constants, cd)
	d.table.Constants = append(d.table.Constants, cd)

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

	// check if the variable already exists in the function
	if slices.ContainsFunc(fd.Variables, func(vd *VariableDescription) bool { return vd.VariableName == name }) {
		return false
	}

	// check if the data type already exists and append it if not
	if index := slices.IndexFunc(d.table.DataTypes, func(dtd DataTypeDescription) bool { return dtd.Name() == dataType.Name() }); index == -1 {
		d.AppendDataType(dataType)
	} else {
		dataType = d.table.DataTypes[index]
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
	if slices.IndexFunc(d.table.DataTypes, func(dtd DataTypeDescription) bool { return dtd.Name() == dataType.Name() }) != -1 {
		return false
	}

	d.table.DataTypes = append(d.table.DataTypes, dataType)
	return true
}

// Append a member of a composite data type to the debug information.
func (d *debugInformation) AppendMember(compositeName, name, nameSource string, dataType DataTypeDescription) bool {
	// find the composite data type
	index := slices.IndexFunc(d.table.DataTypes, func(dtd DataTypeDescription) bool {
		return dtd.Kind() == DataTypeComposite && dtd.Name() == compositeName
	})

	// composite not found
	if index == -1 {
		return false
	}

	// extract composite data type
	cdt := d.table.DataTypes[index].(*CompositeDataType)

	// check if member already exists
	if slices.ContainsFunc(cdt.CompositeMembers, func(m *CompositeMember) bool { return m.MemberName == name }) {
		return false
	}

	// check if the member data type already exists and append it if not
	if index := slices.IndexFunc(d.table.DataTypes, func(dtd DataTypeDescription) bool { return dtd.Name() == dataType.Name() }); index == -1 {
		d.AppendDataType(dataType)
	} else {
		dataType = d.table.DataTypes[index]
	}

	// create and append the member
	member := &CompositeMember{MemberName: name, MemberNameSource: nameSource, Type: dataType}
	member.Order = len(cdt.CompositeMembers)
	cdt.CompositeMembers = append(cdt.CompositeMembers, member)

	return true
}

// Update the byte size of a simple data type in the debug information.
func (d *debugInformation) UpdateSimpleDataTypeSize(name string, size int32) bool {
	if index := slices.IndexFunc(d.table.DataTypes, func(dtd DataTypeDescription) bool {
		return dtd.Kind() == DataTypeSimple && dtd.Name() == name
	}); index != -1 {
		d.table.DataTypes[index].(*SimpleDataType).ByteSize = size
		return true
	}

	return false
}

// Update the encoding of a simple data type in the debug information.
func (d *debugInformation) UpdateSimpleDataTypeEncoding(name string, encoding int) bool {
	if index := slices.IndexFunc(d.table.DataTypes, func(dtd DataTypeDescription) bool {
		return dtd.Kind() == DataTypeSimple && dtd.Name() == name
	}); index != -1 {
		d.table.DataTypes[index].(*SimpleDataType).BaseTypeEncoding = encoding
		return true
	}

	return false
}

// Update the size of all pointer data types in the debug information.
func (d *debugInformation) UpdatePointerDataTypeSizes(size int32) bool {
	found := false

	for i := range d.table.DataTypes {
		if d.table.DataTypes[i].Kind() == DataTypePointer {
			d.table.DataTypes[i].(*PointerDataType).ByteSize = size
			found = true
		}
	}

	return found
}

// Recursively update the sizes of all composite data types in the debug information.
// Note: all simple and pointer data types must have their sizes set before calling this function.
func (d *debugInformation) UpdateCompositeDataTypeSizes() bool {
	found := false

	// track which types are being processed to detect circular references
	processing := make(map[string]bool)

	// cache calculated sizes of types to avoid recalculation
	calculated := make(map[string]bool)

	// declaration of internal recursive function to calculate size (required because of recursive calls)
	var calculateSize func(dataType DataTypeDescription) int32

	// definition of internal recursive function to calculate size (knows itself because of declaration)
	calculateSize = func(dataType DataTypeDescription) int32 {
		// if already calculated, just return the size
		if calculated[dataType.Name()] {
			return dataType.Size()
		}

		// handle different data type kinds
		switch dt := dataType.(type) {
		case *SimpleDataType, *PointerDataType:
			// simple and pointer types must already have their sizes set
			calculated[dt.Name()] = true
			return dt.Size()

		case *CompositeDataType:
			// check for circular reference
			if processing[dt.Name()] {
				panic(NewGeneralError(Core, failureMap, Fatal, circularDependencyInCompositeDataType, dt.Name(), nil))
			}

			// processing of this composite data type is running
			processing[dt.Name()] = true

			// always reset the size before calculating
			dt.ByteSize = 0

			// recursively calculate size of each member
			for i := range dt.CompositeMembers {
				dt.ByteSize += calculateSize(dt.CompositeMembers[i].Type)
			}

			// processing of this composite data type is complete
			delete(processing, dt.Name())

			// mark this composite data type as calculated
			calculated[dt.Name()] = true
			return dt.ByteSize

		default:
			panic(NewGeneralError(Core, failureMap, Fatal, unexpectedDataTypeKind, dt.Kind(), nil))
		}
	}

	// process all composite data types by calling the internal calculation function
	for i := range d.table.DataTypes {
		if d.table.DataTypes[i].Kind() == DataTypeComposite {
			calculateSize(d.table.DataTypes[i])
			found = true
		}
	}

	return found
}

// Update the offset of all members in all composite data types that exists in the debug information.
// Note: all composite data types must have their sizes set before calling this function.
func (d *debugInformation) UpdateCompositeDataTypeOffsets() bool {
	found := false

	// iterate over all data types to find composite data types
	for i := range d.table.DataTypes {
		if d.table.DataTypes[i].Kind() == DataTypeComposite {
			ctd := d.table.DataTypes[i].(*CompositeDataType)

			// sort the composite members by their given order
			slices.SortFunc(ctd.CompositeMembers, func(m1, m2 *CompositeMember) int {
				return m1.Order - m2.Order
			})

			// update the offset by adding the byte size of the previous member
			for i := 1; i < len(ctd.CompositeMembers); i++ {
				ctd.CompositeMembers[i].Offset = ctd.CompositeMembers[i-1].Offset + ctd.CompositeMembers[i-1].Type.Size()
			}

			found = true
		}
	}

	return found
}

// Update the offset of a variable in the debug information.
func (d *debugInformation) UpdateVariableOffset(name string, offset int32) bool {
	if index := slices.IndexFunc(d.table.Variables, func(vd *VariableDescription) bool { return vd.VariableName == name }); index != -1 {
		vd := d.table.Variables[index]
		vd.Offset = offset
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

// Find a function by name in the debug string table.
func (dst *DebugStringTable) FindFunction(name string) *FunctionDescription {
	if index := slices.IndexFunc(dst.Functions, func(fd *FunctionDescription) bool { return fd.FunctionName == name }); index != -1 {
		return dst.Functions[index]
	}

	return nil
}

// Find a variable by function name and variable name in the debug string table.
func (dst *DebugStringTable) FindVariable(function, name string) *VariableDescription {
	if index := slices.IndexFunc(dst.Variables, func(vd *VariableDescription) bool {
		return vd.FunctionName == function && vd.VariableName == name
	}); index != -1 {
		return dst.Variables[index]
	}

	return nil
}

// Find a constant by function name and constant name in the debug string table.
func (dst *DebugStringTable) FindConstant(function, name string) *ConstantDescription {
	if index := slices.IndexFunc(dst.Constants, func(cd *ConstantDescription) bool {
		return cd.FunctionName == function && cd.ConstantName == name
	}); index != -1 {
		return dst.Constants[index]
	}

	return nil
}

// Find a data type by name in the debug string table.
func (dst *DebugStringTable) FindDataType(name string) DataTypeDescription {
	if index := slices.IndexFunc(dst.DataTypes, func(dtd DataTypeDescription) bool { return dtd.Name() == name }); index != -1 {
		return dst.DataTypes[index]
	}

	return nil
}
