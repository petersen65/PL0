// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package platform provides abstractions for different target platforms.
package platform

import "fmt"

// Label for the entry point of an application.
const EntryPointLabel = "main"

// Operating systems for which the assembly code is built.
const (
	MacOS   OperatingSystem = iota // macOS 2026 or later, supported on AArch64 (Apple Silicon)
	Linux                          // Linux builds from 2025 onward, supported on x86_64 and AArch64
	Windows                        // Windows 11 or later, supported on x86_64
)

// CPU instruction set architectures (ISA) for which the assembly code is built.
const (
	X86_64  InstructionSetArchitecture = iota // x86-64 processors from AMD and Intel supporting the 64-bit instruction set
	AArch64                                   // ARM 64-bit architecture family, found in Apple Silicon and other ARM-based systems
)

// CPU instruction sets for which the assembly code is built.
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

// String encodings for which the assembly code is built.
const (
	UTF8  StringEncoding = 1 << iota // UTF-8 encoding, used for text in source code and string literals
	UTF16                            // UTF-16 encoding, used for wide character strings
	UTF32                            // UTF-32 encoding, used for fixed-width character strings
)

// Application binary interfaces for which assembly code is generated.
const (
	ABI_SystemV_AMD64 ApplicationBinaryInterface = iota // System V AMD64 ABI (Linux x86_64, macOS x86_64)
	ABI_Microsoft_x64                                   // Microsoft x64 ABI (Windows x86_64)
	ABI_AAPCS64                                         // ARM64 AAPCS64 (Linux ARM64, macOS ARM64)
	ABI_Windows_ARM64                                   // Windows ARM64 ABI
)

// Kind of output that is represented by the assembly code.
const (
	Application OutputKind = iota // an application consisting of business logic
	Runtime                       // runtime of programming language
)

// Optimization algorithms as bit-mask.
const (
	Debug   Optimization = 1 << iota // all algorithms turned off, suitable for development and debugging
	Release                          // selected algorithms are turned on, suitable for production builds
)

type (
	// Target operating system.
	OperatingSystem int

	// CPU instruction set architecture (ISA) represents the CPU family for which assembly code is generated.
	InstructionSetArchitecture int

	// Determine the specific instruction set features available on the target ISA.
	InstructionSet int

	// String encoding format constants represent the encoding and byte size in each single value.
	StringEncoding int

	// The application binary interface defines
	//   - how data structures are laid out in memory,
	//   - how functions receive parameters and return values,
	//   - and how operating system calls are made.
	ApplicationBinaryInterface int

	// Output kind of the assembly code.
	OutputKind int

	// Optimization algorithms to be applied during code emission.
	Optimization uint64

	// TargetPlatform defines the target platform for which assembly code is generated.
	TargetPlatform struct {
		OperatingSystem            OperatingSystem            `json:"operating_system"`             // operating system name
		InstructionSetArchitecture InstructionSetArchitecture `json:"instruction_set_architecture"` // CPU family name
		InstructionSet             InstructionSet             `json:"instruction_set"`              // instruction set of the CPU
		StringEncoding             StringEncoding             `json:"string_encoding"`              // string encoding format
		ApplicationBinaryInterface ApplicationBinaryInterface `json:"application_binary_interface"` // application binary interface
	}

	// Build configuration used during the compilation process.
	BuildConfiguration struct {
		SourcePath         string         `json:"source_path"`          // path to the source code file
		TargetPath         string         `json:"target_path"`          // path to the target assembly file
		SourceAbsolutePath string         `json:"source_absolute_path"` // absolute path to the source code file
		TargetAbsolutePath string         `json:"target_absolute_path"` // absolute path to the target assembly file
		TargetPlatform     TargetPlatform `json:"target_platform"`      // target platform for which the assembly code is generated
		DriverDisplayName  string         `json:"driver_display_name"`  // compiler driver display name
		OutputKind         OutputKind     `json:"output_kind"`          // kind of output represented by the assembly code
		Optimization       Optimization   `json:"optimization"`         // optimization algorithms to be applied during code emission
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

// String representation of the string encoding.
func (se StringEncoding) String() string {
	return stringEncodingNames[se]
}

// String representation of the application binary interface.
func (abi ApplicationBinaryInterface) String() string {
	return applicationBinaryInterfaceNames[abi]
}

// String representation of the target platform.
func (t TargetPlatform) String() string {
	return fmt.Sprintf("%v %v %v %v %v", t.OperatingSystem, t.InstructionSetArchitecture, t.InstructionSet, t.StringEncoding, t.ApplicationBinaryInterface)
}

// String representation of the output kind.
func (ok OutputKind) String() string {
	return outputKindNames[ok]
}
