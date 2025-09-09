// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package typesystem

import (
	"fmt"
	"strings"
)

// Data type of a function or procedure that defines its signature.
type functionTypeDescriptor struct {
	commonTypeDescriptor                      // embedded common type descriptor
	Parameters           []*FunctionParameter `json:"parameters"`  // ordered list of parameters that the function accepts
	ReturnType           TypeDescriptor       `json:"return_type"` // type descriptor of the value returned by the function (nil for procedures)
}

// Map a parameter passing mode to its string representation.
var parameterModeNames = map[ParameterPassingMode]string{
	CallByValue:          "",
	CallByReference:      "var",
	CallByConstReference: "const",
	OutputParameter:      "out",
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

// Depending on the ABI, calculate the memory size in bytes for the function type descriptor.
func (d *functionTypeDescriptor) Size() int {
	return applicationBinaryInterfaceSpecifications[d.Abi].PointerSize
}

// Depending on the ABI, calculate the memory alignment in bytes for the function type descriptor.
func (d *functionTypeDescriptor) Alignment() int {
	return applicationBinaryInterfaceSpecifications[d.Abi].PointerAlignment
}

// Check if the function type descriptor is equal to another type descriptor.
func (d *functionTypeDescriptor) Equal(other TypeDescriptor) bool {
	// detect self-comparison early and indicate equality to avoid unnecessary work
    if d == other {
        return true
    }

	// check if the other type descriptor is also a function type descriptor
	if o, ok := other.(*functionTypeDescriptor); ok {
		// the function signatures are equal if they have the same number of parameters with the same passing modes and types, and the same return type
		if len(d.Parameters) != len(o.Parameters) {
			return false
		}

		// compare each parameter in order
		for i := range d.Parameters {
			descriptor, other := d.Parameters[i], o.Parameters[i]

			// passing modes must match
			if descriptor.Mode != other.Mode {
				return false
			}

			// parameter types must match (both nil or both non-nil and equal)
			if (descriptor.Type == nil) != (other.Type == nil) {
				return false
			}

			// parameter types must match if both are non-nil
			if descriptor.Type != nil && !descriptor.Type.Equal(other.Type) {
				return false
			}
		}

		// return types must match (both nil or both non-nil and equal)
		if (d.ReturnType == nil) != (o.ReturnType == nil) {
			return false
		}

		// return types must match if both are non-nil
		if d.ReturnType != nil && !d.ReturnType.Equal(o.ReturnType) {
			return false
		}

		return true
	}

	// data types are not equal if they are of different type descriptors
	return false
}
