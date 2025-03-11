// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emitter

import cor "github.com/petersen65/PL0/v2/core"

// Failure codes for the emitter.
const (
	_ = cor.Failure(iota + 40000)
	unsupportedCpuTarget
	unknownIntermediateCodeOperation
	unknownKindOfOperandInCpuOperation
	unexpectedKindOfOperandInCpuOperation
	invalidSizeForOperandInCpuOperation
	unresolvedLabelReferenceInAssemblyCode
	unexpectedNumberOfFunctionArguments
	unknownExportFormat
	unknownImportFormat
	assemblyCodeExportFailed
	assemblyCodeImportFailed
	linkingStepMissing
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	unsupportedCpuTarget:                   "unsupported CPU target: %v",
	unknownIntermediateCodeOperation:       "unknown intermediate code operation: %v",
	unknownKindOfOperandInCpuOperation:     "unknown kind of operand in CPU operation: %v",
	unexpectedKindOfOperandInCpuOperation:  "unexpected kind of operand in CPU operation: %v",
	invalidSizeForOperandInCpuOperation:    "invalid size for operand in CPU operation: %v",
	unresolvedLabelReferenceInAssemblyCode: "unresolved label reference in assembly code: %v",
	unexpectedNumberOfFunctionArguments:    "unexpected number of arguments for function call",
	unknownExportFormat:                    "unknown export format: %v",
	unknownImportFormat:                    "unknown import format: %v",
	assemblyCodeExportFailed:               "failed to export assembly code",
	assemblyCodeImportFailed:               "failed to import assembly code",
	linkingStepMissing:                     "assembly code cannot be exported because linking step is missing",
}
