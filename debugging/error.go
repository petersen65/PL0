// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package debugging

import cor "github.com/petersen65/pl0/v3/core"

// Failure codes for debugging support.
const (
	_ cor.Failure = iota + 200
	unexpectedDataTypeKind
	circularDependencyInCompositeDataType
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	unexpectedDataTypeKind:                "unexpected kind of data type: %v",
	circularDependencyInCompositeDataType: "circular dependency in composite data type: %v",
}
