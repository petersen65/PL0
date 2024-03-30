// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package assembler implements an emitter that generates LLVM assembly language.
package assembler

type (
	// Text section of the emitted assembly language.
	TextSection []string
)

// The assembler interface provides an abstract API for emitting LLVM assembly language.
type Assembler interface {
}

// Return the public interface of the private assembler implementation.
func NewAssembler() Assembler {
	return newAssembler()
}
