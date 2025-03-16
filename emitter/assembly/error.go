// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package assembly

import cor "github.com/petersen65/PL0/v2/core"

// Failure codes for the assembly code.
const (
	_ = cor.Failure(iota + 9000)
	unknownKindOfOperandInCpuOperation
	unexpectedKindOfOperandInCpuOperation
	unresolvedLabelReferenceInAssemblyCode
	linkingStepMissing
	unknownExportFormat
	unknownImportFormat
	assemblyCodeExportFailed
	assemblyCodeImportFailed
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	unknownKindOfOperandInCpuOperation:     "unknown kind of operand in CPU operation: %v",
	unexpectedKindOfOperandInCpuOperation:  "unexpected kind of operand in CPU operation: %v",
	unresolvedLabelReferenceInAssemblyCode: "unresolved label reference in assembly code: %v",
	linkingStepMissing:                     "assembly code cannot be exported because linking step is missing",
	unknownExportFormat:                    "unknown export format: %v",
	unknownImportFormat:                    "unknown import format: %v",
	assemblyCodeExportFailed:               "failed to export assembly code",
	assemblyCodeImportFailed:               "failed to import assembly code",
}
