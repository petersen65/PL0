// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package emitter implements the assembly code generation compiler phase by iterating over the intermediate code unit.
package emitter

import gen "github.com/petersen65/PL0/v2/generator"

const (
	Amd64 = CentralProcessingUnit(iota) // CPU target for the assembly code emitter
)

// Call codes for the programming language standard library.
const (
	_ = StandardCall(iota)
	Readln
	Writeln
)

type (
	// Type for CPU targets.
	CentralProcessingUnit int32

	// Type for standard library call codes.
	StandardCall int64

	// The Emitter interface provides methods for emitting assembly code for CPU targets.
	Emitter interface {
		Emit()
		GetAssemblyCodeUnit() AssemblyCodeUnit
	}
)

// Return the public interface of the private emitter implementation.
func NewEmitter(cpu CentralProcessingUnit, intermediateCode gen.IntermediateCodeUnit) Emitter {
	return newEmitter(cpu, intermediateCode)
}
