// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package typesystem

// Prefixes for pointer and reference modifier string representations.
const (
	pointerPrefix   = "ptr."
	referencePrefix = "ref."
)

// Pointer type that represents a memory address pointing to a value with a specific data type.
type pointerTypeDescriptor struct {
	commonTypeDescriptor                // embedded common type descriptor
	ValueType            TypeDescriptor `json:"value_type"` // type descriptor of the value that this pointer references
}

// String representation of the pointer type.
func (d *pointerTypeDescriptor) String() string {
	if d.Kind_ == DataTypeReference {
		return referencePrefix + d.ValueType.String()
	}

	return pointerPrefix + d.ValueType.String()
}

// Depending on the ABI, calculate the memory size in bytes for the pointer type descriptor.
func (d *pointerTypeDescriptor) Size() int {
	return applicationBinaryInterfaceSpecifications[d.Abi].PointerSize
}

// Depending on the ABI, determine the memory alignment requirement for the pointer type descriptor.
func (d *pointerTypeDescriptor) Alignment() int {
	return applicationBinaryInterfaceSpecifications[d.Abi].PointerAlignment
}

// Check if the pointer type descriptor is equal to another type descriptor.
func (d *pointerTypeDescriptor) Equal(other TypeDescriptor) bool {
	// detect self-comparison early and indicate equality to avoid unnecessary work
    if d == other {
        return true
    }

	// check if the other type descriptor is also a pointer type descriptor and has the same kind (pointer or reference)
	if o, ok := other.(*pointerTypeDescriptor); ok && d.Kind_ == o.Kind_{
		return d.ValueType.Equal(o.ValueType)
	}

	// data types are not equal if they are of different type descriptors
	return false
}
