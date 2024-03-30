// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package assembler

// Private implementation of the assembler.
type assembler struct {
	textSection TextSection
}

// Return the public interface of the private assembler implementation.
func newAssembler() Assembler {
	return &assembler{
		textSection: make(TextSection, 0),
	}
}
