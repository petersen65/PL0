// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package elf

import cor "github.com/petersen65/PL0/v2/core"

// Failure codes for ELF.
const (
	_ cor.Failure = iota + 10000
	invalidReadOnlyDataValue
	unknownKindOfReadOnlyData
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	invalidReadOnlyDataValue:  "invalid read-only data value: %v",
	unknownKindOfReadOnlyData: "unknown kind of read-only data: %v",
}
