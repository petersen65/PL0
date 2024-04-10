// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emulator

import cor "github.com/petersen65/PL0/v2/core"

// Failure codes for the emulator.
const (
	_ = cor.Failure(iota + 50000)
	recoveredFromIllegalIntermediateCode
	textSectionImportFailed
	unknownIntermediateCodeOperation
	consecutiveBranchOperationsNotSupported
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
	recoveredFromIllegalIntermediateCode:    "recovered from illegal intermediate code: %v",
	textSectionImportFailed:                 "failed to import raw bytes into the text section",
	unknownIntermediateCodeOperation:        "unknown intermediate code operation: '%v'",
	consecutiveBranchOperationsNotSupported: "consecutive branch operations are not supported",
	unsupportedOperand:                      "halt - unsupported operand for operation '%v'",
	addressOutOfRange:                       "halt - address '%v' out of range",
	stackOverflow:                           "halt - stack overflow at stack pointer '%v'",
	unknownOperation:                        "halt - unknown operation at address '%v'",
	arithmeticOverflowNegation:              "halt - arithmetic overflow (negation) at address '%v'",
	arithmeticOverflowAddition:              "halt - arithmetic overflow (addition) at address '%v'",
	arithmeticOverflowSubtraction:           "halt - arithmetic overflow (subtraction) at address '%v'",
	arithmeticOverflowMultiplication:        "halt - arithmetic overflow (multiplication) at address '%v'",
	arithmeticOverflowDivision:              "halt - arithmetic overflow (division) at address '%v'",
	divisionByZero:                          "halt - division by zero at address '%v'",
}
