// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package core

import (
	"strings"
	"unicode/utf8"
)

// Maximum number of allowed UTF-8 decoding errors before source content decoding is aborted.
const maxDecodingErrors = 5

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

	// Map string encodings to their names.
	stringEncodingNames = map[StringEncoding]string{
		UTF8:  "UTF-8",
		UTF16: "UTF-16",
		UTF32: "UTF-32",
	}

	// Map application binary interface names to their corresponding names.
	applicationBinaryInterfaceNames = map[ApplicationBinaryInterface]string{
		ABI_SystemV_AMD64: "SystemV_AMD64",
		ABI_Microsoft_x64: "Microsoft_x64",
		ABI_AAPCS64:       "AAPCS64",
		ABI_Windows_ARM64: "Windows_ARM64",
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

// String representation of optimization algorithms.
func (o Optimization) String() string {
	representation := make([]string, 0)

	// the debug optimization turns off all optimization algorithms and always overrides any release optimizations
	if o&Debug != 0 {
		representation = append(representation, optimizationNames[Debug])
	} else {
		representation = append(representation, optimizationNames[Release])
	}

	return strings.Join(representation, OptimizationSeparator)
}

// Filter binary source content from all UTF-8 errors, replace all tabulators, and return the binary content as valid source code.
func CreateSourceCode(content []byte) []byte {
	var decodingErrors int
	sourceCode := make([]byte, 0, len(content))
	tabulator := []byte(strings.Repeat(" ", TabulatorSize))

	// iterate over the binary content and decode each UTF-8 character
	for i := 0; i < len(content); {
		// decode the next UTF-8 character from the source content
		codepoint, width := utf8.DecodeRune(content[i:])

		// check for decoding errors, replace tabulators, and only append valid Unicode code points to the source code
		switch codepoint {
		case utf8.RuneError:
			decodingErrors++
			sourceCode = append(sourceCode, ' ')

		case '\t':
			sourceCode = append(sourceCode, tabulator...)

		default:
			// append the original UTF-8 bytes for the decoded rune
			sourceCode = append(sourceCode, content[i:i+width]...)
		}

		// increment the index by the byte size of the decoded rune
		i += width

		// abort decoding with empty source code if too many errors occurred
		if decodingErrors > maxDecodingErrors {
			return make([]byte, 0)
		}
	}

	return sourceCode
}
