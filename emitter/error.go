// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emitter

import cor "github.com/petersen65/PL0/v2/core"

// Failure codes for the emitter.
const (
	_ = cor.Failure(iota + 8000)
	unsupportedCpuTarget
	unknownIntermediateCodeOperation
	invalidSizeForOperandInCpuOperation
	unexpectedNumberOfFunctionArguments
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	unsupportedCpuTarget:                "unsupported CPU target: %v",
	unknownIntermediateCodeOperation:    "unknown intermediate code operation: %v",
	invalidSizeForOperandInCpuOperation: "invalid size for operand in CPU operation: %v",
	unexpectedNumberOfFunctionArguments: "unexpected number of arguments for function call",
}
