// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package scanner implements a tokenizer and lexical analyzer for the programming language PL/0.
package scanner

import cor "github.com/petersen65/PL0/v3/core"

// The scanner provides features for scanning UTF-8 encoded source code and producing a stream of tokens.
type Scanner interface {
	Scan(content []byte) (cor.TokenStream, error)
}

// Return the interface of the scanner implementation.
func NewScanner() Scanner {
	return newScanner()
}
