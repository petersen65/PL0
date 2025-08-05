// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package core

type debugInformation struct {
	tokenHandler TokenHandler // token handler that manages the tokens of the token stream
	table        *DebugStringTable
}

func newDebugInformation(compilationUnit string, tokenHandler TokenHandler) DebugInformation {
	return &debugInformation{
		tokenHandler: tokenHandler,
		table:        newDebugStringTable(compilationUnit),
	}
}

func newDebugStringTable(compilationUnit string) *DebugStringTable {
	return &DebugStringTable{
		CompilationUnit: compilationUnit,
		Functions:       make([]*FunctionDescription, 0),
	}
}

func newFunctionDescription(name, nameSource string, tokenStreamIndex int) *FunctionDescription {
	return &FunctionDescription{
		Name:             name,
		NameSource:       nameSource,
		TokenStreamIndex: tokenStreamIndex,
		Variables:        make([]*VariableDescription, 0),
	}
}

func newVariableDescription(function, functionSource, name, nameSource string, tokenStreamIndex int) *VariableDescription {
	return &VariableDescription{
		Function:         function,
		FunctionSource:   functionSource,
		Name:             name,
		NameSource:       nameSource,
		TokenStreamIndex: tokenStreamIndex,
	}
}

func (d *debugInformation) AppendFunction(name, nameSource string, tokenStreamIndex int) bool {
	for _, f := range d.table.Functions {
		if f.Name == name {
			return false
		}
	}

	d.table.Functions = append(d.table.Functions, newFunctionDescription(name, nameSource, tokenStreamIndex))
	return true
}

func (d *debugInformation) AppendVariable(function, functionSource, name, nameSource string, tokenStreamIndex int) bool {
	for _, f := range d.table.Functions {
		if f.Name == function {
			for _, v := range f.Variables {
				if v.Name == name {
					return false
				}
			}

			f.Variables = append(f.Variables, newVariableDescription(function, functionSource, name, nameSource, tokenStreamIndex))
			return true
		}
	}

	return false
}

func (d *debugInformation) GetDebugStringTable() DebugStringTable {
	return *d.table
}

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
