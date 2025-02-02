// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emitter

import cor "github.com/petersen65/PL0/v2/core"

// Failure codes for the emitter.
const (
	_ = cor.Failure(iota + 40000)
	recoveredFromIllegalIntermediateCode
	unsupportedCpuTarget
	unknownIntermediateCodeOperation
	unsupportedOperandDataType
	operandParsingError
	unknownInstructionOperand
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	recoveredFromIllegalIntermediateCode: "recovered from illegal intermediate code: %v",
	unsupportedCpuTarget:                 "unsupported CPU target: %v",
	unknownIntermediateCodeOperation:     "unknown intermediate code operation: %v",
	unsupportedOperandDataType:           "datatype not supported for an operand: %v",
	operandParsingError:                  "value of operand cannot be parsed: %v",
	unknownInstructionOperand:            "unknown instruction operand: %v",
}
