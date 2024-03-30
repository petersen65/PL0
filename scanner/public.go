// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package scanner implements the PL/0 scanner that performs a lexical analysis of the source code.
package scanner

import cor "github.com/petersen65/PL0/v2/core"

type (
	// The scanner interface provides methods for scanning binary UTF-8 encoded source code into a binary token stream.
	Scanner interface {
		Scan(content []byte) (cor.TokenStream, error)
	}
)

// Return the public interface of the private scanner implementation.
func NewScanner() Scanner {
	return newScanner()
}
