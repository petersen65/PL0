// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package emitter

import "fmt"

const textSectionMax = 200

const (
	_ = failure(iota + 3000)
	reachedTextSectionMax
	instructionOutOfRange
)

type failure int

var errorMap = map[failure]string{
	reachedTextSectionMax: "reached text section maximum size: %v",
	instructionOutOfRange: "provided instruction is out of range: %v",
}

func (e *emitter) error(code failure, value any) error {
	var message string

	if value != nil {
		message = fmt.Sprintf(errorMap[code], value)
	} else {
		message = errorMap[code]
	}

	return fmt.Errorf("emitter error %v: %v", code, message)
}
