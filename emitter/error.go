// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emitter

import cor "github.com/petersen65/PL0/v2/core"

// Failure codes for the emitter.
const (
	_ cor.Failure = iota + 8000
	unsupportedTargetPlatform
	unknownIntermediateCodeOperation
	unsupportedDataTypeForIntermediateCodeOperation
	unsupportedJumpOperationForConditionalJump
	unsupportedDataTypeInArithmeticOperation
	unsupportedDataTypeInComparisonOperation
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	unsupportedTargetPlatform:                       "unsupported target platform: %v",
	unknownIntermediateCodeOperation:                "unknown intermediate code operation: %v",
	unsupportedDataTypeForIntermediateCodeOperation: "unsupported data type for intermediate code operation: %v",
	unsupportedJumpOperationForConditionalJump:      "unsupported jump operation for conditional jump: %v",
	unsupportedDataTypeInArithmeticOperation:        "unsupported data type in arithmetic operation: %v",
	unsupportedDataTypeInComparisonOperation:        "unsupported data type in comparison operation: %v",
}
