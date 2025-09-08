// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package typesystem

// Create a new pointer type descriptor with a value type. The pointer can either be a regular pointer or a reference type.
func NewPointerTypeDescriptor(valueType TypeDescriptor, isReference bool) TypeDescriptor {
	kind := DataTypePointer
	enforceSpecifiedApplicationBinaryInterface()

	if isReference {
		kind = DataTypeReference
	}

	return &pointerTypeDescriptor{
		commonTypeDescriptor: commonTypeDescriptor{Abi: currentABI, Kind_: kind},
		ValueType:            valueType,
	}
}
