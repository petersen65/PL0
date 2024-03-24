// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emulator

import cor "github.com/petersen65/PL0/core"

// Failure codes for the IL/0 emulator.
const (
	_ = cor.Failure(iota + 10000)
	addressOutOfRange
	stackOverflow
	unknownOperation
	arithmeticOverflowNegation
	arithmeticOverflowAddition
	arithmeticOverflowSubtraction
	arithmeticOverflowMultiplication
	arithmeticOverflowDivision
	divisionByZero
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	addressOutOfRange:                "halt - address '%v' out of range",
	stackOverflow:                    "halt - stack overflow at address '%v'",
	unknownOperation:                 "halt - unknown operation at address '%v'",
	arithmeticOverflowNegation:       "halt - arithmetic overflow (negation) at address '%v'",
	arithmeticOverflowAddition:       "halt - arithmetic overflow (addition) at address '%v'",
	arithmeticOverflowSubtraction:    "halt - arithmetic overflow (subtraction) at address '%v'",
	arithmeticOverflowMultiplication: "halt - arithmetic overflow (multiplication) at address '%v'",
	arithmeticOverflowDivision:       "halt - arithmetic overflow (division) at address '%v'",
	divisionByZero:                   "halt - division by zero at address '%v'",
}
