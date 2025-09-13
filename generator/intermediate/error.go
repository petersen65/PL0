// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package intermediate

import eh "github.com/petersen65/pl0/v3/errors"

// Failure codes for the intermediate code.
const (
	_ eh.Failure = iota + 6300
	invalidAddressesContract
	invalidIntermediateCodeAddress
	unexceptedVariantInIntermediateCodeAddress
	unknownInstructionOption
	unknownExportFormat
	intermediateCodeExportFailed
)

// Map failure codes to error messages.
var failureMap = map[eh.Failure]string{
	invalidAddressesContract:                   "invalid addresses contract: %v",
	invalidIntermediateCodeAddress:             "invalid intermediate code address: %v",
	unexceptedVariantInIntermediateCodeAddress: "unexcepted variant in intermediate code address: %v",
	unknownInstructionOption:                   "unknown instruction option: %v",
	unknownExportFormat:                        "unknown export format: %v",
	intermediateCodeExportFailed:               "failed to export intermediate code",
}
