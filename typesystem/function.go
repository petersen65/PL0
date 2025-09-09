// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package typesystem

// Parameter passing modes for functions and procedures.
const (
	CallByValue          ParameterPassingMode = iota // call by value
	CallByReference                                  // call by reference
	CallByConstReference                             // call by const reference
	OutputParameter                                  // output parameter
)

type (
	// Mode for parameter passing in functions and procedures.
	ParameterPassingMode int

	// Parameter and its passing mode for a function or procedure call.
	FunctionParameter struct {
		Name     string               `json:"name"`      // identifier name used to reference this parameter within the function
		TypeName string               `json:"type_name"` // type string defining the data type name of this parameter
		Type     TypeDescriptor       `json:"type"`      // type descriptor defining the data type of this parameter
		Mode     ParameterPassingMode `json:"mode"`      // determines how the parameter is passed (e.g., by value, by reference)
	}
)

// Create a new function type descriptor with parameters and a return type. The return type may be nil for procedures.
func NewFunctionTypeDescriptor(parameters []*FunctionParameter, returnType TypeDescriptor, builtIn bool) TypeDescriptor {
	kind := DataTypeFunction
	enforceSpecifiedApplicationBinaryInterface()

	if parameters == nil {
		parameters = make([]*FunctionParameter, 0)
	}

	if returnType == nil {
		kind = DataTypeProcedure
	}

	return &functionTypeDescriptor{
		commonTypeDescriptor: commonTypeDescriptor{Abi: currentABI, Kind_: kind, BuiltIn: builtIn},
		Parameters:           parameters,
		ReturnType:           returnType,
	}
}

// Create a new function parameter with a name, parameter type information, and passing mode.
// A function parameter can either be used with a type name or a type descriptor or both. The type name can be empty or the type descriptor can be nil.
func NewFunctionParameter(parameterName, parameterTypeName string, parameterType TypeDescriptor, mode ParameterPassingMode) *FunctionParameter {
	return &FunctionParameter{
		Name:     parameterName,
		TypeName: parameterTypeName,
		Type:     parameterType,
		Mode:     mode,
	}
}

// String representation of a parameter passing mode.
func (m ParameterPassingMode) String() string {
	return parameterModeNames[m]
}
