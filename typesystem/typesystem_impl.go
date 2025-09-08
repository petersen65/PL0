// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package typesystem

import (
	"fmt"
	"strings"

	eh "github.com/petersen65/pl0/v3/errors"
	plt "github.com/petersen65/pl0/v3/platform"
)

// The sentinel values indicate that the byte size or byte alignment has not been calculated yet for a type descriptor.
const byteSizeNotCalculated = -1
const byteAlignmentNotCalculated = -1

// Prefixes for pointer and reference modifier string representations.
const (
	pointerPrefix   = "ptr."
	referencePrefix = "ref."
)

// Packing rules define how structure based data types are packed.
const (
	naturalPacking   packingRule = iota // align each field to its natural alignment, max struct alignment 16
	microsoftPacking                    // like natural packing, but max struct alignment 8, nested structs capped at 8
)

type (
	// A packing rule defines how structure based data types are packed.
	packingRule int

	// Common fields shared by all type descriptors.
	commonTypeDescriptor struct {
		Abi plt.ApplicationBinaryInterface `json:"application_binary_interface"` // ABI governing size and alignment calculations
	}

	// Primitive data types that cannot be further refined.
	simpleTypeDescriptor struct {
		commonTypeDescriptor                   // embedded common type descriptor
		PrimitiveType        PrimitiveDataType `json:"primitive_type"` // underlying primitive data type
	}

	// Pointer type that represents a memory address pointing to a value with a specific data type.
	pointerTypeDescriptor struct {
		commonTypeDescriptor                // embedded common type descriptor
		ValueType            TypeDescriptor `json:"value_type"`   // type descriptor of the value that this pointer references
		IsReference          bool           `json:"is_reference"` // true if this is a reference rather than a raw pointer
	}

	// Structure based data types that group multiple fields.
	structureTypeDescriptor struct {
		commonTypeDescriptor                   // embedded common type descriptor
		TypeName             string            `json:"type_name"` // name of the structure data type
		Fields               []*StructureField `json:"fields"`    // ordered list of fields that comprise this structure
		IsPacked             bool              `json:"is_packed"` // memory layout optimized to minimize padding between fields
		ByteSize             int               `json:"size"`      // cache the total size of the structure in bytes
		ByteAlignment        int               `json:"alignment"` // cache the alignment of the structure in bytes
	}

	// Data type of a function or procedure that defines its signature.
	functionTypeDescriptor struct {
		commonTypeDescriptor                      // embedded common type descriptor
		Parameters           []*FunctionParameter `json:"parameters"`  // ordered list of parameters that the function accepts
		ReturnType           TypeDescriptor       `json:"return_type"` // type descriptor of the value returned by the function (nil for procedures)
	}

	// The application binary interface specification contains size, alignment, and packing rules.
	applicationBinaryInterfaceSpecification struct {
		Name              string                    `json:"name"`                // name of the ABI specification
		PointerSize       int                       `json:"pointer_size"`        // size of pointers in bytes
		PointerAlignment  int                       `json:"pointer_alignment"`   // alignment requirement for pointers
		TypeSizes         map[PrimitiveDataType]int `json:"type_sizes"`          // size mapping for each primitive type
		TypeAlignments    map[PrimitiveDataType]int `json:"type_alignments"`     // alignment mapping for each primitive type
		StructPackingRule packingRule               `json:"struct_packing_rule"` // rule for packing struct fields
		MaxAlignment      int                       `json:"max_alignment"`       // maximum alignment allowed
	}
)

var (
	// Current application binary interface specification.
	currentABI plt.ApplicationBinaryInterface = plt.ABI_NotSpecified

	// Application binary interface specifications for modern 64-bit platforms.
	applicationBinaryInterfaceSpecifications = map[plt.ApplicationBinaryInterface]*applicationBinaryInterfaceSpecification{
		plt.ABI_SystemV_AMD64: {
			Name:             plt.ABI_SystemV_AMD64.String(),
			PointerSize:      8,
			PointerAlignment: 8,
			TypeSizes: map[PrimitiveDataType]int{
				Integer8:   1,
				Integer16:  2,
				Integer32:  4,
				Integer64:  8,
				Float32:    4,
				Float64:    8,
				Unsigned8:  1,
				Unsigned16: 2,
				Unsigned32: 4,
				Unsigned64: 8,
				Boolean:    1,
				Character:  4,
				String:     16,
			},
			TypeAlignments: map[PrimitiveDataType]int{
				Integer8:   1,
				Integer16:  2,
				Integer32:  4,
				Integer64:  8,
				Float32:    4,
				Float64:    8,
				Unsigned8:  1,
				Unsigned16: 2,
				Unsigned32: 4,
				Unsigned64: 8,
				Boolean:    1,
				Character:  4,
				String:     8,
			},
			StructPackingRule: naturalPacking,
			MaxAlignment:      16,
		},
		plt.ABI_Microsoft_x64: {
			Name:             plt.ABI_Microsoft_x64.String(),
			PointerSize:      8,
			PointerAlignment: 8,
			TypeSizes: map[PrimitiveDataType]int{
				Integer8:   1,
				Integer16:  2,
				Integer32:  4,
				Integer64:  8,
				Float32:    4,
				Float64:    8,
				Unsigned8:  1,
				Unsigned16: 2,
				Unsigned32: 4,
				Unsigned64: 8,
				Boolean:    1,
				Character:  4,
				String:     16,
			},
			TypeAlignments: map[PrimitiveDataType]int{
				Integer8:   1,
				Integer16:  2,
				Integer32:  4,
				Integer64:  8,
				Float32:    4,
				Float64:    8,
				Unsigned8:  1,
				Unsigned16: 2,
				Unsigned32: 4,
				Unsigned64: 8,
				Boolean:    1,
				Character:  4,
				String:     8,
			},
			StructPackingRule: microsoftPacking,
			MaxAlignment:      8,
		},
		plt.ABI_AAPCS64: {
			Name:             plt.ABI_AAPCS64.String(),
			PointerSize:      8,
			PointerAlignment: 8,
			TypeSizes: map[PrimitiveDataType]int{
				Integer8:   1,
				Integer16:  2,
				Integer32:  4,
				Integer64:  8,
				Float32:    4,
				Float64:    8,
				Unsigned8:  1,
				Unsigned16: 2,
				Unsigned32: 4,
				Unsigned64: 8,
				Boolean:    1,
				Character:  4,
				String:     16,
			},
			TypeAlignments: map[PrimitiveDataType]int{
				Integer8:   1,
				Integer16:  2,
				Integer32:  4,
				Integer64:  8,
				Float32:    4,
				Float64:    8,
				Unsigned8:  1,
				Unsigned16: 2,
				Unsigned32: 4,
				Unsigned64: 8,
				Boolean:    1,
				Character:  4,
				String:     8,
			},
			StructPackingRule: naturalPacking,
			MaxAlignment:      16,
		},
		plt.ABI_Windows_ARM64: {
			Name:             plt.ABI_Windows_ARM64.String(),
			PointerSize:      8,
			PointerAlignment: 8,
			TypeSizes: map[PrimitiveDataType]int{
				Integer8:   1,
				Integer16:  2,
				Integer32:  4,
				Integer64:  8,
				Float32:    4,
				Float64:    8,
				Unsigned8:  1,
				Unsigned16: 2,
				Unsigned32: 4,
				Unsigned64: 8,
				Boolean:    1,
				Character:  4,
				String:     16,
			},
			TypeAlignments: map[PrimitiveDataType]int{
				Integer8:   1,
				Integer16:  2,
				Integer32:  4,
				Integer64:  8,
				Float32:    4,
				Float64:    8,
				Unsigned8:  1,
				Unsigned16: 2,
				Unsigned32: 4,
				Unsigned64: 8,
				Boolean:    1,
				Character:  4,
				String:     8,
			},
			StructPackingRule: microsoftPacking,
			MaxAlignment:      8,
		},
	}

	// Map a primitive data type to its string representation.
	primitiveDataTypeNames = map[PrimitiveDataType]string{
		Integer64:  "int64",
		Integer32:  "int32",
		Integer16:  "int16",
		Integer8:   "int8",
		Float64:    "float64",
		Float32:    "float32",
		Unsigned64: "uint64",
		Unsigned32: "uint32",
		Unsigned16: "uint16",
		Unsigned8:  "uint8",
		Boolean:    "bool",
		Character:  "char",
		String:     "string",
	}

	// Map a data type kind to its string representation.
	dataTypeKindNames = map[DataTypeKind]string{
		DataTypeSimple:    "simple",
		DataTypePointer:   "pointer",
		DataTypeStructure: "structure",
	}

	// Map a parameter passing mode to its string representation.
	parameterModeNames = map[ParameterPassingMode]string{
		CallByValue:          "",
		CallByReference:      "var",
		CallByConstReference: "const",
		OutputParameter:      "out",
	}
)

// String representation of the simple type.
func (d *simpleTypeDescriptor) String() string {
	return d.PrimitiveType.String()
}

// Data type classification for simple type descriptors.
func (d *simpleTypeDescriptor) Kind() DataTypeKind {
	return DataTypeSimple
}

// Depending on the ABI, calculate the memory size in bytes for the simple type descriptor.
func (d *simpleTypeDescriptor) Size() int {
	return applicationBinaryInterfaceSpecifications[d.Abi].TypeSizes[d.PrimitiveType]
}

// Depending on the ABI, determine the memory alignment requirement for the simple type descriptor.
func (d *simpleTypeDescriptor) Alignment() int {
	return applicationBinaryInterfaceSpecifications[d.Abi].TypeAlignments[d.PrimitiveType]
}

// String representation of the pointer type.
func (d *pointerTypeDescriptor) String() string {
	if d.IsReference {
		return referencePrefix + d.ValueType.String()
	}

	return pointerPrefix + d.ValueType.String()
}

// Data type classification for pointer type descriptors.
func (d *pointerTypeDescriptor) Kind() DataTypeKind {
	if d.IsReference {
		return DataTypeReference
	}

	return DataTypePointer
}

// Depending on the ABI, calculate the memory size in bytes for the pointer type descriptor.
func (d *pointerTypeDescriptor) Size() int {
	return applicationBinaryInterfaceSpecifications[d.Abi].PointerSize
}

// Depending on the ABI, determine the memory alignment requirement for the pointer type descriptor.
func (d *pointerTypeDescriptor) Alignment() int {
	return applicationBinaryInterfaceSpecifications[d.Abi].PointerAlignment
}

// String representation of the structure type.
func (d *structureTypeDescriptor) String() string {
	var fields []string

	for _, field := range d.Fields {
		fields = append(fields, fmt.Sprintf("%v %v", field.Name, field.Type.String()))
	}

	return fmt.Sprintf("struct {%v}", strings.Join(fields, ", "))
}

// Data type classification for structure type descriptors.
func (d *structureTypeDescriptor) Kind() DataTypeKind {
	return DataTypeStructure
}

// Depending on the ABI, calculate the memory size in bytes for the structure type descriptor.
func (d *structureTypeDescriptor) Size() int {
	// use the cached byte size if available
	if d.ByteSize != byteSizeNotCalculated {
		return d.ByteSize
	}

	// use cycle detection to prevent infinite recursion
	seen := make(map[string]bool)

	// recursive size calculation
	return d.calculateSize(seen)
}

// Depending on the ABI, determine the memory alignment requirement for the structure type descriptor.
func (d *structureTypeDescriptor) Alignment() int {
	// use the cached byte alignment if available
	if d.ByteAlignment != byteAlignmentNotCalculated {
		return d.ByteAlignment
	}

	// use cycle detection to prevent infinite recursion
	seen := make(map[string]bool)

	// recursive alignment calculation
	return d.calculateAlignment(seen)
}

// Calculate the memory size in bytes of a structure using a recursive approach.
func (d *structureTypeDescriptor) calculateSize(seen map[string]bool) int {
	// check for a circular reference
	if seen[d.TypeName] {
		// return pointer size as fallback to allow forward declarations and self-referential structs via pointers
		return applicationBinaryInterfaceSpecifications[d.Abi].PointerSize
	}

	// mark this type as being processed and clean up when done
	seen[d.TypeName] = true
	defer delete(seen, d.TypeName)

	// track total size
	var totalSize int

	// track maximum alignment requirement
	maxAlignment := 1

	// calculate field offsets and total size of the structure
	for _, field := range d.Fields {
		// get field alignment for the current field in the structure
		fieldAlign := d.getFieldAlignment(field.Type, seen)

		// track maximum alignment requirement
		if fieldAlign > maxAlignment {
			maxAlignment = fieldAlign
		}

		// adjust the total size so that the current field starts at the correct aligned offset (unless packed)
		if !d.IsPacked && totalSize%fieldAlign != 0 {
			totalSize += fieldAlign - (totalSize % fieldAlign)
		}

		// the offset of the current field is correctly aligned
		field.ByteOffset = totalSize

		// the new total size starts one byte after the current field
		totalSize += d.getFieldSize(field.Type, seen)
	}

	// limit the tracked maximum alignment to the maximum allowed by the application binary interface
	if maxAlignment > applicationBinaryInterfaceSpecifications[d.Abi].MaxAlignment {
		maxAlignment = applicationBinaryInterfaceSpecifications[d.Abi].MaxAlignment
	}

	// the total size of the structure must be a multiple of the maximum alignment (unless packed)
	//   - each power of two alignment is valid for all smaller powers of two
	//   - this guarantees that arrays of these structs will have properly aligned elements
	if !d.IsPacked && totalSize%maxAlignment != 0 {
		totalSize += maxAlignment - (totalSize % maxAlignment)
	}

	// the total size calculation requires all calculated field sizes and field alignments
	d.ByteSize = totalSize
	return totalSize
}

// Calculate the memory alignment in bytes of a structure using a recursive approach.
func (d *structureTypeDescriptor) calculateAlignment(seen map[string]bool) int {
	// check for a circular reference
	if seen[d.TypeName] {
		// return pointer alignment as fallback to allow forward declarations and self-referential structs via pointers
		return applicationBinaryInterfaceSpecifications[d.Abi].PointerAlignment
	}

	// mark this type as being processed and clean up when done
	seen[d.TypeName] = true
	defer delete(seen, d.TypeName)

	// packed structure types have one byte alignment
	if d.IsPacked {
		return 1
	}

	// track maximum alignment requirement
	var maxAlignment int = 1

	// determine the maximum alignment requirement of all fields
	for _, field := range d.Fields {
		// get field alignment for the current field in the structure
		fieldAlign := d.getFieldAlignment(field.Type, seen)

		// track maximum alignment requirement
		if fieldAlign > maxAlignment {
			maxAlignment = fieldAlign
		}
	}

	// limit the tracked maximum alignment to the maximum allowed by the application binary interface
	if maxAlignment > applicationBinaryInterfaceSpecifications[d.Abi].MaxAlignment {
		maxAlignment = applicationBinaryInterfaceSpecifications[d.Abi].MaxAlignment
	}

	// the alignment calculation requires all calculated field alignments
	d.ByteAlignment = maxAlignment
	return maxAlignment
}

// Calculate the memory size in bytes of a field using a recursive approach.
func (d *structureTypeDescriptor) getFieldSize(fieldType TypeDescriptor, seen map[string]bool) int {
	// for record types, use the cycle-aware calculation
	if fieldRecord, ok := fieldType.(*structureTypeDescriptor); ok {
		return fieldRecord.calculateSize(seen)
	}

	// for other types, use their standard size calculation
	return fieldType.Size()
}

// Calculate the memory alignment in bytes of a field using a recursive approach.
func (d *structureTypeDescriptor) getFieldAlignment(fieldType TypeDescriptor, seen map[string]bool) int {
	// for record types, use the cycle-aware calculation
	if fieldRecord, ok := fieldType.(*structureTypeDescriptor); ok {
		return fieldRecord.calculateAlignment(seen)
	}

	// for other types, use their standard alignment calculation
	return fieldType.Alignment()
}

// String representation of the function or procedure type.
func (d *functionTypeDescriptor) String() string {
	var parameters []string

	for _, parameter := range d.Parameters {
		parameters = append(parameters, strings.TrimSpace(fmt.Sprintf("%v %v %v", parameter.Mode, parameter.Name, parameter.Type.String())))
	}

	if d.ReturnType != nil {
		return fmt.Sprintf("function(%v): %v", strings.Join(parameters, ", "), d.ReturnType.String())
	}

	return fmt.Sprintf("procedure(%v)", strings.Join(parameters, ", "))
}

// Data type classification for function or procedure type descriptors.
func (d *functionTypeDescriptor) Kind() DataTypeKind {
	if d.ReturnType != nil {
		return DataTypeFunction
	}

	return DataTypeProcedure
}

// Depending on the ABI, calculate the memory size in bytes for the function type descriptor.
func (d *functionTypeDescriptor) Size() int {
	return applicationBinaryInterfaceSpecifications[d.Abi].PointerSize
}

// Depending on the ABI, calculate the memory alignment in bytes for the function type descriptor.
func (d *functionTypeDescriptor) Alignment() int {
	return applicationBinaryInterfaceSpecifications[d.Abi].PointerAlignment
}

// Enforce that the current application binary interface has been specified and panic if not.
func enforceSpecifiedApplicationBinaryInterface() {
	if currentABI == plt.ABI_NotSpecified {
		panic(eh.NewGeneralError(eh.TypeSystem, failureMap, eh.Fatal, applicationBinaryInterfaceNotSpecified, nil, nil))
	}
}
