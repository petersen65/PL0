// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package x86_64

import eh "github.com/petersen65/pl0/v3/errors"

// Failure codes for the assembly code.
const (
	_ eh.Failure = iota + 9000
	unknownKindOfOperandInCpuOperation
	illegalDisplacementInMemoryOperand
	unknownKindOfReadOnlyData
	unknownOutputKind
	invalidAttributeForDirective
	debuggingInformationIncomplete
	optimizationDebugNotSupportedInRuntime
	compilationDetailsAndProducerRequired
	predefinedDataTypeRequired
	unexpectedDataTypeKind
	unknownExportFormat
	assemblyCodeExportFailed
)

// Map failure codes to error messages.
var failureMap = map[eh.Failure]string{
	unknownKindOfOperandInCpuOperation:     "unknown kind of operand in CPU operation: %v",
	illegalDisplacementInMemoryOperand:     "illegal displacement in memory operand: %v",
	unknownKindOfReadOnlyData:              "unknown kind of read-only data: %v",
	unknownOutputKind:                      "unknown output kind for assembly code: %v",
	invalidAttributeForDirective:           "invalid attribute for directive: %v",
	debuggingInformationIncomplete:         "debugging information is incomplete: %v",
	optimizationDebugNotSupportedInRuntime: "debug optimization is not supported for runtime functions: %v",
	compilationDetailsAndProducerRequired:  "compilation details and producer are required for debug information",
	predefinedDataTypeRequired:             "predefined data type is required for debug information: %v",
	unexpectedDataTypeKind:                 "unexpected kind of data type: %v",
	unknownExportFormat:                    "unknown export format: %v",
	assemblyCodeExportFailed:               "failed to export assembly code",
}
