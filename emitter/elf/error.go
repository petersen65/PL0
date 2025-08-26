// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package elf

import cor "github.com/petersen65/PL0/v3/core"

// Failure codes for ELF.
const (
	_ cor.Failure = iota + 10000
	invalidReadOnlyDataValue
	unknownKindOfReadOnlyData
	sleb128DecodingOverflow
	sleb128DecodingIncomplete
	uleb128DecodingOverflow
	uleb128DecodingIncomplete
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	invalidReadOnlyDataValue:  "invalid read-only data value: %v",
	unknownKindOfReadOnlyData: "unknown kind of read-only data: %v",
	sleb128DecodingOverflow:   "signed leb128 decoding overflow: %v",
	sleb128DecodingIncomplete: "signed leb128 decoding incomplete: %v",
	uleb128DecodingOverflow:   "unsigned leb128 decoding overflow: %v",
	uleb128DecodingIncomplete: "unsigned leb128 decoding incomplete: %v",
}
