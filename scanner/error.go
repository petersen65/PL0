// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package scanner

import "fmt"

const (
	digitsMax     = 19 // maximum int64 length: -9223372036854775808 to 9223372036854775807
	identifierMax = 64 // maximum length of any identifier
)

// Error codes for the PL/0 scanner.
const (
	_ = failure(iota + 1000)
	eofComment
	tooLongIdentifier
	tooLongNumber
	illegalInteger
	unexpectedCharacter
)

// Failure is a type for error codes of the PL/0 scanner.
type failure int

// Map error codes to error messages.
var errorMap = map[failure]string{
	eofComment:             "end of file reached inside comment",
	tooLongIdentifier:      "identifier %s is too long",
	tooLongNumber:          "number %s is too long",
	illegalInteger:         "cannot parse number %s into integer value",
	unexpectedCharacter:    "unexpected character '%c'",
}

// Create a new error by mapping the error code to its corresponding error message.
func newError(code failure, value any, line, column int) error {
	var message string

	if value != nil {
		message = fmt.Sprintf(errorMap[code], value)
	} else {
		message = errorMap[code]
	}

	return fmt.Errorf("scanner error %v [%v,%v]: %v", code, line, column, message)
}
