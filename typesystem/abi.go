// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package typesystem

import plt "github.com/petersen65/pl0/v3/platform"

// The type system requires a current application binary interface to be set.
func SetCurrentApplicationBinaryInterface(abi plt.ApplicationBinaryInterface) {
	currentABI = abi
	enforceSpecifiedApplicationBinaryInterface()
}

// Get the current application binary interface used by the type system.
func GetCurrentApplicationBinaryInterface() plt.ApplicationBinaryInterface {
	return currentABI
}
