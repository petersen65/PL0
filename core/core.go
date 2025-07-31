// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package core provides foundation features, the token type-system, and the error handling mechanism for the compiler.
// This combination enables an error handler to connect an error to a location in the token stream.
// Package core is required to depend on Go standard library packages only.
package core

import (
	"fmt"
	"io"
)

// Label for the entry point of an application.
const EntryPointLabel = "main"

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

// Packages of the compiler which can generate errors as a bit-mask enumeration.
const (
	Core Component = 1 << iota
	Scanner
	Parser
	AbstractSyntaxTree
	Analyzer
	Generator
	Intermediate
	ControlFlowGraph
	Emitter
	Intel
	ExecutableLinkableFormat
	AllComponents = Component(^uint64(0))
)

// Export formats for the compiler which can be used to export intermediate results.
const (
	Json ExportFormat = iota
	Text
	Binary
)

// Kind of output that is represented by the assembly code.
const (
	Application OutputKind = iota // an application consisting of business logic
	Runtime                       // programming language runtime library
)

// Optimization algorithms as bit-mask.
const (
	Debug   = Optimization(0)          // all algorithms turned off, suitable for development and debugging
	Release = Optimization(^uint64(0)) // all algorithms turned on, suitable for production builds
)

type (
	// Target operating system.
	OperatingSystem int

	// CPU instruction set architecture (ISA) represents the CPU family for which assembly code is generated.
	InstructionSetArchitecture int

	// Determine the specific instruction set features available on the target ISA.
	InstructionSet int

	// Component describes packages of the compiler which can generate errors (bit-mask).
	Component uint64

	// Export formats for the compiler.
	ExportFormat int

	// Output kind of the assembly code.
	OutputKind int

	// Optimization algorithms to be applied during code emission.
	Optimization uint64

	// TargetPlatform defines the target platform for which assembly code is generated.
	TargetPlatform struct {
		OperatingSystem            OperatingSystem            `json:"operating_system"`             // operating system name
		InstructionSetArchitecture InstructionSetArchitecture `json:"instruction_set_architecture"` // CPU family name
		InstructionSet             InstructionSet             `json:"instruction_set"`              // instruction set of the CPU
	}

	// Build configuration used during the compilation process.
	BuildConfiguration struct {
		SourcePath        string         `json:"source_path"`         // path to the source code file
		TargetPath        string         `json:"target_path"`         // path to the target assembly file
		TargetPlatform    TargetPlatform `json:"target_platform"`     // target platform for which the assembly code is generated
		DriverDisplayName string         `json:"driver_display_name"` // compiler driver display name
		OutputKind        OutputKind     `json:"output_kind"`         // kind of output represented by the assembly code
		Optimization      Optimization   `json:"optimization"`        // optimization algorithms to be applied during code emission
	}

	// Exporter is an interface that provides methods for exporting intermediate results.
	Exporter interface {
		Print(print io.Writer, args ...any) error
		Export(format ExportFormat, print io.Writer) error
	}

	// Importer is an interface that provides methods for importing intermediate results.
	Importer interface {
		Import(format ExportFormat, scan io.Reader) error
	}
)

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

// String representation of the output kind.
func (ok OutputKind) String() string {
	return outputKindNames[ok]
}

// String representation of the optimization algorithm.
func (o Optimization) String() string {
	return optimizationNames[o]
}
