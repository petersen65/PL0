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

type (
	// Track visited structure pairs during type equality checks to prevent infinite recursion.
	structurePair struct {
		a, b *structureTypeDescriptor
	}

	// Structure based data types that group multiple fields.
	structureTypeDescriptor struct {
		commonTypeDescriptor                          // embedded common type descriptor
		TypeName               string                 `json:"type_name"` // name of the structure data type
		Fields                 []*StructureField      `json:"fields"`    // ordered list of fields that comprise this structure
		IsPacked               bool                   `json:"is_packed"` // memory layout optimized to minimize padding between fields
		ByteSize               int                    `json:"size"`      // cache the total size of the structure in bytes
		ByteAlignment          int                    `json:"alignment"` // cache the alignment of the structure in bytes
		equalityCycleDetection map[structurePair]bool `json:"-"`         // track visited structure pairs during type equality checks
	}
)

// String representation of the structure type descriptor.
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
	cycleDetection := make(map[string]bool)

	// recursive size calculation
	return d.calculateSize(cycleDetection)
}

// Depending on the ABI, determine the memory alignment requirement for the structure type descriptor.
func (d *structureTypeDescriptor) Alignment() int {
	// use the cached byte alignment if available
	if d.ByteAlignment != byteAlignmentNotCalculated {
		return d.ByteAlignment
	}

	// use cycle detection to prevent infinite recursion
	cycleDetection := make(map[string]bool)

	// recursive alignment calculation
	return d.calculateAlignment(cycleDetection)
}

// Check if the structure type descriptor is equal to another type descriptor.
func (d *structureTypeDescriptor) Equal(other TypeDescriptor) bool {
	// if either type descriptor is nil, they are not equal
	if d == nil || other == nil {
		return false
	}

	// detect self-comparison early and indicate equality to avoid unnecessary work
    if d == other {
        return true
    }

	// check if the other type descriptor is also a structure type descriptor
	if o, ok := other.(*structureTypeDescriptor); ok {
		// initialize the recursion detection map at the top-level call for both structures
		if d.equalityCycleDetection == nil {
			d.equalityCycleDetection = make(map[structurePair]bool)
			o.equalityCycleDetection = d.equalityCycleDetection

			// the map is cleared after the top-level call completes
			defer func() {
				d.equalityCycleDetection = nil
				o.equalityCycleDetection = nil
			}()
		}

		// create a pair representing the two structures being compared
		pair := structurePair{d, o}

		// if the pair has already been visited, they are equal
		// note: this is safe because if they weren't equal, it would have been caught in a previous comparison
		if d.equalityCycleDetection[pair] {
			return true
		}

		// mark this pair as being compared
		d.equalityCycleDetection[pair] = true

		// the structure types are equal if they have the same number of fields with the same names and types
		if d.TypeName != o.TypeName || d.IsPacked != o.IsPacked {
			return false
		}

		// number of fields must match
		if len(d.Fields) != len(o.Fields) {
			return false
		}

		// compare each field in order
		for i := range d.Fields {
			descriptor, other := d.Fields[i], o.Fields[i]

			// field names must match
			if descriptor.Name != other.Name {
				return false
			}

			// field types must match (both nil or both non-nil and equal)
			if (descriptor.Type == nil) != (other.Type == nil) {
				return false
			}

			// field types must match if both are non-nil
			if descriptor.Type != nil && !descriptor.Type.Equal(other.Type) {
				return false
			}
		}

		return true
	}

	// data types are not equal if they are of different type descriptors
	return false
}

// Calculate the memory size in bytes of a structure using a recursive approach.
func (d *structureTypeDescriptor) calculateSize(cycleDetection map[string]bool) int {
	// check for a circular reference
	if cycleDetection[d.TypeName] {
		// return pointer size as fallback to allow forward declarations and self-referential structs via pointers
		return applicationBinaryInterfaceSpecifications[d.Abi].PointerSize
	}

	// mark this type as being processed and clean up when done
	cycleDetection[d.TypeName] = true

	// track total size
	var totalSize int

	// track maximum alignment requirement
	maxAlignment := 1

	// calculate field offsets and total size of the structure
	for _, field := range d.Fields {
		// get field alignment for the current field in the structure
		fieldAlign := d.getFieldAlignment(field.Type, cycleDetection)

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
		totalSize += d.getFieldSize(field.Type, cycleDetection)
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
func (d *structureTypeDescriptor) calculateAlignment(cycleDetection map[string]bool) int {
	// check for a circular reference
	if cycleDetection[d.TypeName] {
		// return pointer alignment as fallback to allow forward declarations and self-referential structs via pointers
		return applicationBinaryInterfaceSpecifications[d.Abi].PointerAlignment
	}

	// mark this type as being processed and clean up when done
	cycleDetection[d.TypeName] = true

	// packed structure types have one byte alignment
	if d.IsPacked {
		return 1
	}

	// track maximum alignment requirement
	var maxAlignment int = 1

	// determine the maximum alignment requirement of all fields
	for _, field := range d.Fields {
		// get field alignment for the current field in the structure
		fieldAlign := d.getFieldAlignment(field.Type, cycleDetection)

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
func (d *structureTypeDescriptor) getFieldSize(fieldType TypeDescriptor, cycleDetection map[string]bool) int {
	// for record types, use the cycle-aware calculation
	if fieldRecord, ok := fieldType.(*structureTypeDescriptor); ok {
		return fieldRecord.calculateSize(cycleDetection)
	}

	// for other types, use their standard size calculation
	return fieldType.Size()
}

// Calculate the memory alignment in bytes of a field using a recursive approach.
func (d *structureTypeDescriptor) getFieldAlignment(fieldType TypeDescriptor, cycleDetection map[string]bool) int {
	// for record types, use the cycle-aware calculation
	if fieldRecord, ok := fieldType.(*structureTypeDescriptor); ok {
		return fieldRecord.calculateAlignment(cycleDetection)
	}

	// for other types, use their standard alignment calculation
	return fieldType.Alignment()
}
