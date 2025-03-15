// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package emitter implements the assembly code generation compiler phase by iterating over the intermediate code unit.
package emitter

import gen "github.com/petersen65/PL0/v2/generator"

// CPU target for the assembly code emitter.
const Amd64 = CentralProcessingUnit(iota)

// Call codes for the programming language standard library.
const (
	Readln = StandardCall(iota)
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

// Return the interface of the emitter implementation.
func NewEmitter(cpu CentralProcessingUnit, intermediateCode gen.IntermediateCodeUnit) Emitter {
	return newEmitter(cpu, intermediateCode)
}

// String representation of a CPU target.
func (cpu CentralProcessingUnit) String() string {
	return cpuNames[cpu]
}
