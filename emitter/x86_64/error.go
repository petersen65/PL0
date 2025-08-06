// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package x86_64

import cor "github.com/petersen65/PL0/v2/core"

// Failure codes for the assembly code.
const (
	_ cor.Failure = iota + 9000
	unknownKindOfOperandInCpuOperation
	illegalDisplacementInMemoryOperand
	unknownKindOfReadOnlyData
	unknownOutputKind
	invalidAttributeForDirective
	optimizationDebugNotSupportedinRuntime
	compilationUnitAndProducerRequired
	unknownExportFormat
	assemblyCodeExportFailed
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	unknownKindOfOperandInCpuOperation:     "unknown kind of operand in CPU operation: %v",
	illegalDisplacementInMemoryOperand:     "illegal displacement in memory operand: %v",
	unknownKindOfReadOnlyData:              "unknown kind of read-only data: %v",
	unknownOutputKind:                      "unknown output kind for assembly code: %v",
	invalidAttributeForDirective:           "invalid attribute for directive: %v",
	optimizationDebugNotSupportedinRuntime: "debug optimization is not supported for runtime functions: %v",
	compilationUnitAndProducerRequired:     "compilation unit and producer are required for debug information",
	unknownExportFormat:                    "unknown export format: %v",
	assemblyCodeExportFailed:               "failed to export assembly code",
}
