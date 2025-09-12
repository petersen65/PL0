// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package typesystem

// Create a new pointer type descriptor with a value type.
func NewPointerTypeDescriptor(valueType TypeDescriptor, builtIn bool) TypeDescriptor {
	return newPointerTypeDescriptor(valueType, builtIn)
}
