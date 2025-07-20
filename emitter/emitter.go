// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package emitter implements the assembly code generation compiler phase by iterating over the intermediate code unit.
package emitter

import (
	"fmt"

	x64 "github.com/petersen65/PL0/v2/emitter/x86_64"
	ic "github.com/petersen65/PL0/v2/generator/intermediate"
)

// Operating systems for which the assembly code is generated.
const (
	MacOS   OperatingSystem = iota // macOS 2026 or later, supported on AArch64 (Apple Silicon)
	Linux                          // Linux builds from 2025 onward, supported on x86_64 and AArch64
	Windows                        // Windows 11 or later, supported on x86_64
)

// CPU instruction set architectures (ISA) for which the assembly code is generated.
const (
	X86_64  InstructionSetArchitecture = iota // x86-64 processors from AMD and Intel supporting the 64-bit instruction set
	AArch64                                   // ARM 64-bit architecture family, found in Apple Silicon and other ARM-based systems
)

// CPU instruction sets for which the assembly code is generated.
const (
	// x86_64 instruction sets
	ISA_Base   = iota // baseline for all x86_64 CPUs
	ISA_SSE2          // baseline for modern x86_64 CPUs
	ISA_SSE4_2        // adds CRC32, text search, and string comparison instructions
	ISA_AVX           // adds 256-bit SIMD floating-point instructions
	ISA_AVX2          // adds 256-bit SIMD integer instructions
	ISA_AVX512        // adds 512-bit SIMD, masking, ternary logic, and gather/scatter

	// AArch64 instruction sets
	ISA_ARMv8   // baseline AArch64 instruction set (mandatory on all ARM64 chips)
	ISA_ARMv8_2 // adds atomic operations, FP16 support
	ISA_ARMv9   // adds Scalable Vector Extensions (SVE), improved security features
	ISA_ARMv9_2 // later refinements: extended SVE, more cryptographic and ML instructions
)

type (
	// Target operating system.
	OperatingSystem int

	// CPU instruction set architecture (ISA) represents the CPU family for which assembly code is generated.
	InstructionSetArchitecture int

	// Determine the specific instruction set features available on the target CPU family.
	InstructionSet int

	// TargetPlatform defines the target platform for which assembly code is generated.
	TargetPlatform struct {
		OperatingSystem            OperatingSystem
		InstructionSetArchitecture InstructionSetArchitecture
		InstructionSet             InstructionSet
	}

	// The Emitter interface provides methods for emitting assembly code for the target platform.
	Emitter interface {
		Emit()
		GetAssemblyCodeUnit() x64.AssemblyCodeUnit
	}
)

// Return the interface of the emitter implementation.
func NewEmitter(target TargetPlatform, intermediateCode ic.IntermediateCodeUnit) Emitter {
	return newEmitter(target, intermediateCode)
}

// String representation of the target operating system.
func (os OperatingSystem) String() string {
	return operatingSystemNames[os]
}

// String representation of the instruction set architecture.
func (isa InstructionSetArchitecture) String() string {
	return instructionSetArchitectureNames[isa]
}

// String representation of the instruction set.
func (is InstructionSet) String() string {
	return instructionSetNames[is]
}

// String representation of the target platform.
func (t TargetPlatform) String() string {
	return fmt.Sprintf("%v %v %v", t.OperatingSystem, t.InstructionSetArchitecture, t.InstructionSet)
}
