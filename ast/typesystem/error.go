// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package typesystem

import eh "github.com/petersen65/pl0/v3/errors"

// Failure codes for the abstract syntax tree.
const (
	_ eh.Failure = iota + 300
	cannotInstantiateNonGenericType
)

// Map failure codes to error messages.
var failureMap = map[eh.Failure]string{
	cannotInstantiateNonGenericType: "cannot instantiate non-generic type: %v",
}
