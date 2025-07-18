// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package amd64

import cor "github.com/petersen65/PL0/v2/core"

// Failure codes for the assembly code.
const (
	_ cor.Failure = iota + 9000
	unknownKindOfOperandInCpuOperation
	invalidReadOnlyDataValue
	unknownKindOfReadOnlyData
	unknownExportFormat
	assemblyCodeExportFailed
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	unknownKindOfOperandInCpuOperation: "unknown kind of operand in CPU operation: %v",
	invalidReadOnlyDataValue:           "invalid read-only data value: %v",
	unknownKindOfReadOnlyData:          "unknown kind of read-only data: %v",
	unknownExportFormat:                "unknown export format: %v",
	assemblyCodeExportFailed:           "failed to export assembly code",
}
