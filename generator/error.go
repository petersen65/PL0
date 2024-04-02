// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package generator

import cor "github.com/petersen65/PL0/v2/core"

// Failure codes for the code generator.
const (
	_ = cor.Failure(iota + 5000)
	invalidContextInIdentifierUse
	unknownUnaryOperation
	unknownBinaryOperation
	unknownConditionalOperation
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	invalidContextInIdentifierUse: "invalid context in identifier use",
	unknownUnaryOperation:         "unknown unary operation",
	unknownBinaryOperation:        "unknown binary operation",
	unknownConditionalOperation:   "unknown conditional operation",
}
