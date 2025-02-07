// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emitter

import cor "github.com/petersen65/PL0/v2/core"

// Failure codes for the emitter.
const (
	_ = cor.Failure(iota + 40000)
	unsupportedCpuTarget
	unknownIntermediateCodeOperation
	intermediateCodeAddressParsingError
	unknownKindOfOperandInCpuOperation
	unresolvedLabelReference
	unsupportedDataTypeInIntermediateCodeAddress
	unexpectedIntermediateCodeAddressVariantForArg1
	unexpectedIntermediateCodeAddressVariantForArg2
	unexpectedIntermediateCodeAddressVariantForResult
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	unsupportedCpuTarget:                              "unsupported CPU target: %v",
	unknownIntermediateCodeOperation:                  "unknown intermediate code operation: %v",
	intermediateCodeAddressParsingError:               "value of intermediate code address cannot be parsed: %v",
	unknownKindOfOperandInCpuOperation:                "unknown kind of operand in CPU operation: %v",
	unresolvedLabelReference:                          "unresolved label reference: %v",
	unsupportedDataTypeInIntermediateCodeAddress:      "unsupported data type in intermediate code address: %v",
	unexpectedIntermediateCodeAddressVariantForArg1:   "unexpected address variant '%v' for arg1 in intermediate code operation: %v",
	unexpectedIntermediateCodeAddressVariantForArg2:   "unexpected address variant '%v' for arg2 in intermediate code operation: %v",
	unexpectedIntermediateCodeAddressVariantForResult: "unexpected address variant '%v' for result in intermediate code operation: %v",
}
