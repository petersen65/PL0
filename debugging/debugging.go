// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package debugging provides support for debug information collection and DWARF metadata generation.
package debugging

import tok "github.com/petersen65/pl0/v3/token"

// Distinguishes between different kinds of data types.
const (
	DataTypeSimple DataTypeKind = iota
	DataTypeComposite
	DataTypePointer
)

type (
	// DataTypeKind represents the kind of a data type.
	DataTypeKind int

	// DebugStringTable holds generic debug information.
	DebugStringTable struct {
		CompilationUnit      string                 `json:"compilation_unit"`      // name of the compilation unit (e.g., source code file name)
		CompilationDirectory string                 `json:"compilation_directory"` // absolute directory path of the compilation unit
		Producer             string                 `json:"producer"`              // name of the producer (e.g., compiler name and its version)
		String               string                 `json:"string"`                // name of the string data type so that it can be specifically handled
		Optimized            bool                   `json:"optimized"`             // whether the code is optimized
		Functions            []*FunctionDescription `json:"functions"`             // list of all functions in the compilation unit
		Constants            []*ConstantDescription `json:"constants"`             // list of all constants in the compilation unit
		Variables            []*VariableDescription `json:"variables"`             // list of all variables in the compilation unit
		DataTypes            []DataTypeDescription  `json:"data_types"`            // list of all data types in the compilation unit
	}

	// FunctionDescription holds information about a function in the compilation unit.
	FunctionDescription struct {
		FunctionName       string                 `json:"name"`               // name of the function in the compilation unit
		FunctionNameSource string                 `json:"name_source"`        // name of the function in the source code
		GlobalSymbol       bool                   `json:"global_symbol"`      // whether the function is exposed as global symbol
		EntryPoint         bool                   `json:"entry_point"`        // whether the function is the entry point
		TokenStreamIndex   int                    `json:"token_stream_index"` // index of the token stream for the function (e.g., line, column)
		Constants          []*ConstantDescription `json:"constants"`          // list of constants in the function
		Variables          []*VariableDescription `json:"variables"`          // list of variables in the function
	}

	// ConstantDescription holds information about a constant.
	ConstantDescription struct {
		ConstantName       string              `json:"name"`               // name of the constant
		ConstantNameSource string              `json:"name_source"`        // name in the source code
		FunctionName       string              `json:"function"`           // containing function
		FunctionNameSource string              `json:"function_source"`    // function name in the source code
		Type               DataTypeDescription `json:"type"`               // data type of the constant
		Value              any                 `json:"value"`              // value of the constant
		TokenStreamIndex   int                 `json:"token_stream_index"` // index of the token stream for the constant (e.g., line, column)
	}

	// VariableDescription holds information about a variable.
	VariableDescription struct {
		VariableName       string              `json:"name"`               // name of the variable
		VariableNameSource string              `json:"name_source"`        // name in the source code
		FunctionName       string              `json:"function"`           // containing function
		FunctionNameSource string              `json:"function_source"`    // function name in the source code
		Type               DataTypeDescription `json:"type"`               // data type of the variable
		Offset             int32               `json:"offset"`             // offset in memory space (will be set separately)
		TokenStreamIndex   int                 `json:"token_stream_index"` // index of the token stream for the variable (e.g., line, column)
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
		TypeName         string             `json:"name"`        // name of the data type
		TypeNameSource   string             `json:"name_source"` // name in the source code
		ByteSize         int32              `json:"size"`        // size in bytes (will be set separately)
		CompositeMembers []*CompositeMember `json:"members"`     // list of members
	}

	// PointerDataType is a derived type representing pointer types.
	PointerDataType struct {
		TypeName       string              `json:"name"`        // name of the pointer type
		TypeNameSource string              `json:"name_source"` // name in the source code
		ByteSize       int32               `json:"size"`        // size in bytes (will be set separately)
		ElementType    DataTypeDescription `json:"element"`     // type of the element pointed to
	}

	// CompositeMember holds information about a member of a composite type.
	CompositeMember struct {
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
		AppendFunction(name, nameSource string, globalSymbol, entryPoint bool, tokenStreamIndex int) bool
		AppendConstant(function, functionSource, name, nameSource string, dataType DataTypeDescription, value any, tokenStreamIndex int) bool
		AppendVariable(function, functionSource, name, nameSource string, dataType DataTypeDescription, tokenStreamIndex int) bool
		AppendDataType(dataType DataTypeDescription) bool
		AppendMember(compositeName, name, nameSource string, dataType DataTypeDescription) bool
		UpdateSimpleDataTypeSize(name string, size int32) bool
		UpdateSimpleDataTypeEncoding(name string, encoding int) bool
		UpdatePointerDataTypeSizes(size int32) bool
		UpdateCompositeDataTypeSizes() bool
		UpdateCompositeDataTypeOffsets() bool
		UpdateVariableOffset(name string, offset int32) bool
		GetDebugStringTable() DebugStringTable
		GetSourceCodeContext(tokenStreamIndex int) (int, int, string, bool)
	}
)

// Create a new debug information instance for a compilation unit.
func NewDebugInformation(compilationUnit, compilationDirectory, producer, stringName string, optimized bool, tokenHandler tok.TokenHandler) DebugInformation {
	return newDebugInformation(compilationUnit, compilationDirectory, producer, stringName, optimized, tokenHandler)
}

// Create a new simple data type whose size and encoding will be set later.
func NewSimpleDataType(name, nameSource string) *SimpleDataType {
	return newSimpleDataType(name, nameSource)
}

// Create a new composite data type whose size and members will be set later.
func NewCompositeDataType(name, nameSource string) *CompositeDataType {
	return newCompositeDataType(name, nameSource)
}

// Create a new pointer data type whose size will be set later.
func NewPointerDataType(name, nameSource string, elementType DataTypeDescription) *PointerDataType {
	return newPointerDataType(name, nameSource, elementType)
}

// SimpleDataType functions
func (s *SimpleDataType) Kind() DataTypeKind { return DataTypeSimple }
func (s *SimpleDataType) Name() string       { return s.TypeName }
func (s *SimpleDataType) NameSource() string { return s.TypeNameSource }
func (s *SimpleDataType) Size() int32        { return s.ByteSize }
func (s *SimpleDataType) Encoding() int      { return s.BaseTypeEncoding }

// CompositeDataType functions
func (c *CompositeDataType) Kind() DataTypeKind          { return DataTypeComposite }
func (c *CompositeDataType) Name() string                { return c.TypeName }
func (c *CompositeDataType) NameSource() string          { return c.TypeNameSource }
func (c *CompositeDataType) Size() int32                 { return c.ByteSize }
func (c *CompositeDataType) Members() []*CompositeMember { return c.CompositeMembers }

// PointerDataType functions
func (p *PointerDataType) Kind() DataTypeKind           { return DataTypePointer }
func (p *PointerDataType) Name() string                 { return p.TypeName }
func (p *PointerDataType) NameSource() string           { return p.TypeNameSource }
func (p *PointerDataType) Size() int32                  { return p.ByteSize }
func (p *PointerDataType) Element() DataTypeDescription { return p.ElementType }
