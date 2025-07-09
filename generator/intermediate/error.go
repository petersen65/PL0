// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package intermediate

import cor "github.com/petersen65/PL0/v2/core"

// Failure codes for the intermediate code.
const (
	_ cor.Failure = iota + 6000
	invalidAddressesContract
	unsupportedDataTypeInIntermediateCodeAddress
	unexceptedVariantInIntermediateCodeAddress
	unknownInstructionOption
	unknownDataTypeRepresentation
	intermediateCodeAddressParsingError
	unknownExportFormat
	intermediateCodeExportFailed
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	invalidAddressesContract:                     "invalid addresses contract: %v",
	unsupportedDataTypeInIntermediateCodeAddress: "unsupported data type in intermediate code address: %v",
	unexceptedVariantInIntermediateCodeAddress:   "unexcepted variant in intermediate code address: %v",
	unknownInstructionOption:                     "unknown instruction option: %v",
	unknownDataTypeRepresentation:                "unknown data type representation: %v",
	intermediateCodeAddressParsingError:          "value of intermediate code address cannot be parsed: %v",
	unknownExportFormat:                          "unknown export format: %v",
	intermediateCodeExportFailed:                 "failed to export intermediate code",
}
