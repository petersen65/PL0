// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package elf

import (
	"fmt"
	"strings"

	cor "github.com/petersen65/PL0/v2/core"
)

const (
	// Provide a string representation for the text section.
	textSectionDetails = ".section .text"
)

var (
	// Map directives to their string representation.
	directiveNames = map[Directive]string{
		Global:       ".globl",
		Type:         ".type",
		Size:         ".size",
		Weak:         ".weak",
		Hidden:       ".hidden",
		Section:      ".section",
		Text:         ".text",
		Data:         ".data",
		Rodata:       ".rodata",
		Bss:          ".bss",
		Utf32:        ".rodata.str4.4",
		Int64:        ".rodata.int8.8",
		P2Align:      ".p2align",
		Align:        ".align",
		Balign:       ".balign",
		Byte:         ".byte",
		Word:         ".word",
		Long:         ".long",
		Quad:         ".quad",
		Zero:         ".zero",
		String:       ".string",
		Ascii:        ".ascii",
		File:         ".file",
		Loc:          ".loc",
		Line:         ".line",
		CfiStartProc: ".cfi_startproc",
		CfiEndProc:   ".cfi_endproc",
		CfiDefCfa:    ".cfi_def_cfa",
		CfiOffset:    ".cfi_offset",
	}

	// Map attributes to their string representation.
	attributeNames = map[Attribute]string{
		Allocatable: "\"a\"",
		Writable:    "\"w\"",
		Executable:  "\"x\"",
		ProgramBits: "@progbits",
		NoBits:      "@nobits",
	}
)

// String representation of an assembly section.
func (s *AssemblySection[T]) String() string {
	var parts []string
	var builder strings.Builder

	// Write all directives with space separation into the builder and append a newline.
	parts = make([]string, len(s.Directives))

	for i := range s.Directives {
		parts[i] = s.Directives[i].String()
	}
	
	builder.WriteString(strings.Join(parts, " ") + "\n")

	// Write all attributes with space separation into the builder and append a newline.
	parts = make([]string, len(s.Attributes))

	for i := range s.Attributes {
		parts[i] = s.Attributes[i].String()
	}

	builder.WriteString(strings.Join(parts, " ") + "\n")

	// Write all contents with a newline after each item.
	for _, content := range s.Content {
		builder.WriteString(content.String() + "\n")
	}

	return builder.String()
}

// String representation of a read-only data item.
func (rdi *ReadOnlyDataItem) String() string {
	var builder strings.Builder

	// write the labels first and mark them with a local label prefix
	for _, label := range rdi.Labels {
		builder.WriteString(fmt.Sprintf(".L%v:\n", label))
	}

	// encode different kinds of read-only data
	switch rdi.Kind {
	case ReadOnlyUtf32:
		var utf32 []rune

		// encode to UTF-32 string based on the Go data type
		switch v := rdi.Value.(type) {
		case string:
			utf32 = []rune(v)

		case []rune:
			utf32 = v

		default:
			panic(cor.NewGeneralError(cor.Intel, failureMap, cor.Fatal, invalidReadOnlyDataValue, rdi.Value, nil))
		}

		for _, r := range utf32 {
			builder.WriteString(fmt.Sprintf("  .long 0x%08X\n", r))
		}

	case ReadOnlyInt64:
		// encode to 64-bit integer based on supported Go data types
		switch v := rdi.Value.(type) {
		case int64, uint64:
			builder.WriteString(fmt.Sprintf("  .quad %v\n", v))

		default:
			panic(cor.NewGeneralError(cor.Intel, failureMap, cor.Fatal, invalidReadOnlyDataValue, rdi.Value, nil))
		}

	default:
		panic(cor.NewGeneralError(cor.Intel, failureMap, cor.Fatal, unknownKindOfReadOnlyData, rdi.Kind, nil))
	}

	return builder.String()
}
