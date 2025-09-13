// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package evaluation

import eh "github.com/petersen65/pl0/v3/errors"

// Failure codes for the evaluation.
const (
	_ eh.Failure = iota + 4000
	float32EpsilonMustBeBetween0And1
	float64EpsilonMustBeBetween0And1
	float32EpsilonShouldNotBeGreaterThanFloat64Epsilon
	unknownUnaryOperation
	arithmeticOverflowOnConstantExpression
	arithmeticUnderflowOnConstantExpression
	divisionByZeroInConstantExpression
	invalidFloatingPointOperationNaN
	invalidFloatingPointOperationInf
	dataTypeCannotBeUsedInArithmeticOperation
	dataTypeCannotBeUsedInComparisonOperation
	dataTypeCannotBeUsedInUnaryOperation
)

// Map failure codes to error messages.
var failureMap = map[eh.Failure]string{
	float32EpsilonMustBeBetween0And1:                   "float32 epsilon must be between 0 and 1: %v",
	float64EpsilonMustBeBetween0And1:                   "float64 epsilon must be between 0 and 1: %v",
	float32EpsilonShouldNotBeGreaterThanFloat64Epsilon: "float32 epsilon should not be greater than float64 epsilon: %v > %v",
	unknownUnaryOperation:                              "unknown unary operation: %v",
	arithmeticOverflowOnConstantExpression:             "arithmetic overflow on constant expression evaluation for %v: %v %v",
	arithmeticUnderflowOnConstantExpression:            "arithmetic underflow on constant expression evaluation for %v: %v %v",
	divisionByZeroInConstantExpression:                 "division by zero in constant expression evaluation for %v: %v %v",
	invalidFloatingPointOperationNaN:                   "invalid floating point operation resulting in NaN for %v: %v %v %v",
	invalidFloatingPointOperationInf:                   "invalid floating point operation resulting in Inf for %v: %v %v %v",
	dataTypeCannotBeUsedInArithmeticOperation:          "operand data type cannot be used in arithmetic operation %v [%v]: %v",
	dataTypeCannotBeUsedInComparisonOperation:          "operand data type cannot be used in comparison operation %v [%v]: %v",
	dataTypeCannotBeUsedInUnaryOperation:               "operand data type cannot be used in unary operation %v [%v]: %v",
}
