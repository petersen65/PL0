// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package assembler

import cor "github.com/petersen65/PL0/v2/core"

// Failure codes for the assembler.
const (
	_ = cor.Failure(iota + 21000)
	instructionOutOfRange
	unexpectedOperation
	unknownExportFormat
	moduleExportFailed
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	instructionOutOfRange: "instruction is out of range: %v",
	unexpectedOperation:   "unexpected operation: %v",
	unknownExportFormat:   "unknown export format: %v",
	moduleExportFailed:    "failed to export the module",
}
