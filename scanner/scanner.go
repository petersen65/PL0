// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package scanner implements a tokenizer and a lexical analyzer for the scanning compiler phase.
package scanner

import (
	eh "github.com/petersen65/pl0/v3/errors"
	tok "github.com/petersen65/pl0/v3/token"
)

// The scanner provides features for scanning UTF-8 encoded source code and producing a stream of tokens.
type Scanner interface {
	Scan(content []byte) tok.TokenStream
}

// Return the interface of the scanner implementation.
func NewScanner(errorHandler eh.ErrorHandler) Scanner {
	return newScanner(errorHandler)
}
