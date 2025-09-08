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
	o, ok := other.(*functionTypeDescriptor)
	if !ok {
		return false
	}
	if len(d.Parameters) != len(o.Parameters) {
		return false
	}
	for i := range d.Parameters {
		p1, p2 := d.Parameters[i], o.Parameters[i]
		if p1.Mode != p2.Mode {
			return false
		}
		if (p1.Type == nil) != (p2.Type == nil) {
			return false
		}
		if p1.Type != nil && !p1.Type.Equal(p2.Type) {
			return false
		}
	}
	if (d.ReturnType == nil) != (o.ReturnType == nil) {
		return false
	}
	if d.ReturnType != nil && !d.ReturnType.Equal(o.ReturnType) {
		return false
	}
	return true
}
