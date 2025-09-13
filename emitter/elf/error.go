// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package elf

import eh "github.com/petersen65/pl0/v3/errors"

// Failure codes for ELF.
const (
	_ eh.Failure = iota + 8500
	invalidReadOnlyDataValue
	unknownKindOfReadOnlyData
	sleb128DecodingOverflow
	sleb128DecodingIncomplete
	uleb128DecodingOverflow
	uleb128DecodingIncomplete
)

// Map failure codes to error messages.
var failureMap = map[eh.Failure]string{
	invalidReadOnlyDataValue:  "invalid read-only data value: %v",
	unknownKindOfReadOnlyData: "unknown kind of read-only data: %v",
	sleb128DecodingOverflow:   "signed leb128 decoding overflow: %v",
	sleb128DecodingIncomplete: "signed leb128 decoding incomplete: %v",
	uleb128DecodingOverflow:   "unsigned leb128 decoding overflow: %v",
	uleb128DecodingIncomplete: "unsigned leb128 decoding incomplete: %v",
}
