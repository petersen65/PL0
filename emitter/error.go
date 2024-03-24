// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emitter

import cor "github.com/petersen65/PL0/core"

// Failure codes for the IL/0 emitter.
const (
	_ = cor.Failure(iota + 9000)
	instructionOutOfRange
	binaryTextSectionExportFailed
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	instructionOutOfRange:         "instruction is out of range: %v",
	binaryTextSectionExportFailed: "failed to export the text section as binary data",
}
