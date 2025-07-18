// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package emitter implements the assembly code generation compiler phase by iterating over the intermediate code unit.
package emitter

import (
	"fmt"

	ac "github.com/petersen65/PL0/v2/emitter/assembly"
	ic "github.com/petersen65/PL0/v2/generator/intermediate"
)

// Operating systems for which the assembly code is generated.
const (
	MacOS   OperatingSystem = iota // macOS 2026 or later, supported on ARM64 (Apple Silicon)
	Linux                          // Linux builds from 2025 onward, supported on AMD64 and ARM64
	Windows                        // Windows 11 or later, supported on AMD64
)

// CPU family for which the assembly code is generated.
const (
	Amd64 CentralProcessingUnitFamily = iota // all x86-64 processors from AMD and Intel supporting the 64-bit instruction set
	Arm64                                    // ARM 64-bit architecture family, found in Apple Silicon and other ARM-based systems
)

// CPU instruction sets for which the assembly code is generated.
const (
	_ InstructionSetArchitecture = iota

	// AMD64 instruction sets
	ISA_Base   // baseline for all AMD64 CPUs
	ISA_SSE2   // baseline for modern AMD64 CPUs
	ISA_SSE4_2 // adds CRC32, text search, and string comparison instructions
	ISA_AVX    // adds 256-bit SIMD floating-point instructions
	ISA_AVX2   // adds 256-bit SIMD integer instructions
	ISA_AVX512 // adds 512-bit SIMD, masking, ternary logic, and gather/scatter

	// ARM64 instruction sets
	ISA_ARMv8   // baseline AArch64 instruction set (mandatory on all ARM64 chips)
	ISA_ARMv8_2 // adds atomic operations, FP16 support
	ISA_ARMv9   // adds Scalable Vector Extensions (SVE), improved security features
	ISA_ARMv9_2 // later refinements: extended SVE, more cryptographic and ML instructions
)

type (
	// Target operating system.
	OperatingSystem int

	// Target CPU family.
	CentralProcessingUnitFamily int

	// Determine the specific instruction set features available on the target CPU family.
	InstructionSetArchitecture int

	// TargetPlatform defines the target platform for which assembly code is generated.
	TargetPlatform struct {
		OperatingSystem OperatingSystem
		Cpu             CentralProcessingUnitFamily
		InstructionSet  InstructionSetArchitecture
	}

	// The Emitter interface provides methods for emitting assembly code for the target platform.
	Emitter interface {
		Emit()
		GetAssemblyCodeUnit() ac.AssemblyCodeUnit
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

// String representation of the CPU family.
func (cpu CentralProcessingUnitFamily) String() string {
	return cpuFamilyNames[cpu]
}

// String representation of the instruction set architecture.
func (isa InstructionSetArchitecture) String() string {
	return instructionSetArchitectureNames[isa]
}

// String representation of the target platform.
func (t TargetPlatform) String() string {
	return fmt.Sprintf("%v %v %v", t.OperatingSystem, t.Cpu, t.InstructionSet)
}
