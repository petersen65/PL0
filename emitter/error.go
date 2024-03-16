// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package emitter

import tok "github.com/petersen65/PL0/token"

// Error codes for the IL/0 emitter.
const (
	_ = tok.Failure(iota + 3000)
	instructionOutOfRange
)

// Map error codes to error messages.
var errorMap = map[tok.Failure]string{
	instructionOutOfRange: "instruction is out of range: %v",
}
