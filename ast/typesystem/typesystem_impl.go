// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package typesystem

import "fmt"

// Prefixes for pointer and reference modifier string representations.
const (
	pointerPrefix   = "^"
	referencePrefix = "var "
)

var (
	// dataTypeNames maps a data type to its string representation.
	dataTypeNames = map[PrimitiveDataType]string{
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
)

// String representation of a data type.
func (t PrimitiveDataType) String() string {
	var prefix string

	if t.IsPointer() {
		prefix = pointerPrefix
	} else if t.IsReference() {
		prefix = referencePrefix
	}

	return fmt.Sprintf("%v%v", prefix, dataTypeNames[t.AsPlain()])
}
