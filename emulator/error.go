// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emulator

import cor "github.com/petersen65/PL0/v2/core"

// Failure codes for the emulator.
const (
	_ = cor.Failure(iota + 50000)
	unknownStandardCallCode
	unsupportedOperand
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
	unknownStandardCallCode:          "halt - unknown standard library call code: %v",
	unsupportedOperand:               "halt - unsupported operand for operation '%v'",
	addressOutOfRange:                "halt - address '%v' out of range",
	stackOverflow:                    "halt - stack overflow at stack pointer '%v'",
	unknownOperation:                 "halt - unknown operation at address '%v'",
	arithmeticOverflowNegation:       "halt - arithmetic overflow (negation) at address '%v'",
	arithmeticOverflowAddition:       "halt - arithmetic overflow (addition) at address '%v'",
	arithmeticOverflowSubtraction:    "halt - arithmetic overflow (subtraction) at address '%v'",
	arithmeticOverflowMultiplication: "halt - arithmetic overflow (multiplication) at address '%v'",
	arithmeticOverflowDivision:       "halt - arithmetic overflow (division) at address '%v'",
	divisionByZero:                   "halt - division by zero at address '%v'",
}
