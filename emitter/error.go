// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

// Package emitter implements the IL/0 emitter that generates intermediate language code targeted for the IL/0 emulator.
package emitter

import "fmt"

// Error codes for the IL/0 emitter.
const (
	_ = failure(iota + 3000)
	instructionOutOfRange
)

type failure int

// Map error codes to error messages.
var errorMap = map[failure]string{
	instructionOutOfRange: "instruction is out of range: %v",
}

// Create a new error by mapping the error code to its corresponding error message.
func newError(code failure, value any) error {
	var message string

	if value != nil {
		message = fmt.Sprintf(errorMap[code], value)
	} else {
		message = errorMap[code]
	}

	return fmt.Errorf("emitter error %v: %v", code, message)
}
