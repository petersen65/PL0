// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package typesystem

// Structure field describes a field in a structure type descriptor.
type StructureField struct {
	Name       string         `json:"name"`      // name identifier used to access this field within the structure
	TypeName   string         `json:"type_name"` // type string defining the data type name of this field
	Type       TypeDescriptor `json:"type"`      // type descriptor defining the data type of this field
	ByteOffset int            `json:"offset"`    // byte offset is automatically calculated by the type descriptor
}

// Create a new structure type descriptor with fields.
func NewStructureTypeDescriptor(structureTypeName string, fields []*StructureField, isPacked, builtIn bool) TypeDescriptor {
	enforceSpecifiedApplicationBinaryInterface()

	if fields == nil {
		fields = make([]*StructureField, 0)
	}

	return &structureTypeDescriptor{
		commonTypeDescriptor: commonTypeDescriptor{Abi: currentABI, Kind_: DataTypeStructure, BuiltIn: builtIn},
		TypeName:             structureTypeName,
		Fields:               fields,
		IsPacked:             isPacked,
		ByteSize:             byteSizeNotCalculated,
		ByteAlignment:        byteAlignmentNotCalculated,
	}
}

// Create a new structure field with a name and field type information.
// A structure field can either be used with a type name or a type descriptor or both. The type name can be empty or the type descriptor can be nil.
func NewStructureField(fieldName string, fieldTypeName string, fieldType TypeDescriptor) *StructureField {
	return &StructureField{
		Name:     fieldName,
		TypeName: fieldTypeName,
		Type:     fieldType,
	}
}
