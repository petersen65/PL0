// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package core

// Distinguishes between different kinds of data types.
const (
	DataTypeSimple DataTypeKind = iota
	DataTypeComposite
)

type (
	// DataTypeKind represents the kind of a data type.
	DataTypeKind int

	// DebugStringTable holds generic debug information.
	DebugStringTable struct {
		CompilationUnit      string                 `json:"compilation_unit"`
		CompilationDirectory string                 `json:"compilation_directory"`
		Producer             string                 `json:"producer"`
		Optimized            bool                   `json:"optimized"`
		Functions            []*FunctionDescription `json:"functions"`
		Variables            []*VariableDescription `json:"variables"`
		DataTypes            []DataTypeDescription  `json:"data_types"`
	}

	// FunctionDescription holds information about a function in the compilation unit.
	FunctionDescription struct {
		FunctionName       string                 `json:"name"`               // name of the function in the compilation unit
		FunctionNameSource string                 `json:"name_source"`        // name of the function in the source code
		TokenStreamIndex   int                    `json:"token_stream_index"` // index of the token stream for the function (e.g., line, column)
		Variables          []*VariableDescription `json:"variables"`          // list of variables in the function
	}

	// VariableDescription holds information about a variable.
	VariableDescription struct {
		VariableName       string              `json:"name"`               // name of the variable
		VariableNameSource string              `json:"name_source"`        // name in the source code
		FunctionName       string              `json:"function"`           // containing function
		FunctionNameSource string              `json:"function_source"`    // function name in source
		Type               DataTypeDescription `json:"data_type"`          // data type of the variable
		Offset             int32               `json:"offset"`             // offset in memory space (will be set separately)
		TokenStreamIndex   int                 `json:"token_stream_index"` // token stream index
	}

	// SimpleDataType represents primitive/built-in data types.
	SimpleDataType struct {
		TypeName         string `json:"name"`        // name of the data type
		TypeNameSource   string `json:"name_source"` // name in the source code
		ByteSize         int32  `json:"size"`        // size in bytes (will be set separately)
		BaseTypeEncoding int    `json:"base_type"`   // base type encoding in target debugger (will be set separately)
	}

	// CompositeDataType represents structured data types with members.
	CompositeDataType struct {
		SimpleDataType                     // embed SimpleDataType for common fields
		CompositeMembers []*DataTypeMember `json:"members"` // list of members
	}

	// DataTypeMember holds information about a member of a composite type.
	DataTypeMember struct {
		MemberName       string              `json:"name"`        // name of the member
		MemberNameSource string              `json:"name_source"` // name in the source code
		Type             DataTypeDescription `json:"type"`        // data type of the member
		Order            int                 `json:"order"`       // order in the structure
		Offset           int32               `json:"offset"`      // offset within the composite type (will be set separately)
	}

	// DataTypeDescription is the common interface for all data types.
	DataTypeDescription interface {
		Kind() DataTypeKind
		Name() string
		NameSource() string
		Size() int32
	}

	// DebugInformation provides methods to collect and retrieve debug information.
	DebugInformation interface {
		AppendFunction(name, nameSource string, tokenStreamIndex int) bool
		AppendVariable(function, functionSource, name, nameSource string, dataType DataTypeDescription, tokenStreamIndex int) bool
		AppendDataType(dataType DataTypeDescription) bool
		UpdateVariable(name string, offset int32) bool
		GetDebugStringTable() DebugStringTable
		GetSourceCodeContext(tokenStreamIndex int) (int, int, string, bool)
	}
)

// Create a new debug information instance for a compilation unit.
func NewDebugInformation(compilationUnit, compilationDirectory, producer string, optimized bool, tokenHandler TokenHandler) DebugInformation {
	return newDebugInformation(compilationUnit, compilationDirectory, producer, optimized, tokenHandler)
}

// Create a new data type of a specific kind.
func NewDataType(name, nameSource string, kind DataTypeKind) DataTypeDescription {
	return newDataType(name, nameSource, kind)
}

// SimpleDataType methods
func (s *SimpleDataType) Kind() DataTypeKind { return DataTypeSimple }
func (s *SimpleDataType) Name() string       { return s.TypeName }
func (s *SimpleDataType) NameSource() string { return s.TypeNameSource }
func (s *SimpleDataType) Size() int32        { return s.ByteSize }
func (s *SimpleDataType) Encoding() int      { return s.BaseTypeEncoding }

// CompositeDataType methods
func (c *CompositeDataType) Kind() DataTypeKind         { return DataTypeComposite }
func (c *CompositeDataType) Name() string               { return c.TypeName }
func (c *CompositeDataType) NameSource() string         { return c.TypeNameSource }
func (c *CompositeDataType) Size() int32                { return c.ByteSize }
func (c *CompositeDataType) Members() []*DataTypeMember { return c.CompositeMembers }
