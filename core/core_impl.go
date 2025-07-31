// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package core

var (
	// Map target operating systems to their names.
	operatingSystemNames = map[OperatingSystem]string{
		Linux:   "Linux",
		MacOS:   "macOS",
		Windows: "Windows",
	}

	// Map CPU families to their names.
	instructionSetArchitectureNames = map[InstructionSetArchitecture]string{
		X86_64:  "x86_64",
		AArch64: "AArch64",
	}

	// Map instruction sets to their names.
	instructionSetNames = map[InstructionSet]string{
		ISA_Base:    "Base",
		ISA_SSE2:    "SSE2",
		ISA_SSE4_2:  "SSE4.2",
		ISA_AVX:     "AVX",
		ISA_AVX2:    "AVX2",
		ISA_AVX512:  "AVX512",
		ISA_ARMv8:   "ARMv8",
		ISA_ARMv8_2: "ARMv8.2",
		ISA_ARMv9:   "ARMv9",
		ISA_ARMv9_2: "ARMv9.2",
	}

	// Map compiler components to their corresponding names.
	componentMap = map[Component]string{
		Core:                     "core",
		Scanner:                  "scanner",
		Parser:                   "parser",
		AbstractSyntaxTree:       "ast",
		Analyzer:                 "analyzer",
		Generator:                "generator",
		Intermediate:             "intermediate",
		ControlFlowGraph:         "cfg",
		Emitter:                  "emitter",
		Intel:                    "x86_64",
		ExecutableLinkableFormat: "elf",
	}

	// Map output kinds to their names.
	outputKindNames = map[OutputKind]string{
		Application: "application",
		Runtime:     "runtime",
	}

	// Map optimization algorithms to their names.
	optimizationNames = map[Optimization]string{
		Debug:   "debug",
		Release: "release",
	}
)
