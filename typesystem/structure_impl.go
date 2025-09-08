// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package typesystem

import (
	"fmt"
	"strings"
)

// The sentinel values indicate that the byte size or byte alignment has not been calculated yet for a type descriptor.
const byteSizeNotCalculated = -1
const byteAlignmentNotCalculated = -1

// Structure based data types that group multiple fields.
type structureTypeDescriptor struct {
	commonTypeDescriptor                   // embedded common type descriptor
	TypeName             string            `json:"type_name"` // name of the structure data type
	Fields               []*StructureField `json:"fields"`    // ordered list of fields that comprise this structure
	IsPacked             bool              `json:"is_packed"` // memory layout optimized to minimize padding between fields
	ByteSize             int               `json:"size"`      // cache the total size of the structure in bytes
	ByteAlignment        int               `json:"alignment"` // cache the alignment of the structure in bytes
}

// String representation of the structure type.
func (d *structureTypeDescriptor) String() string {
	var fields []string

	for _, field := range d.Fields {
		fields = append(fields, fmt.Sprintf("%v %v", field.Name, field.Type.String()))
	}

	return fmt.Sprintf("struct {%v}", strings.Join(fields, ", "))
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
