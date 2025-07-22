// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package elf

import (
	"fmt"
	"strings"

	cor "github.com/petersen65/PL0/v2/core"
)

var (
	// Map directives to their string representation.
	directiveNames = map[Directive]string{
		Global:       ".global",
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
		P2align:      ".p2align",
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

	// write all directives with space separation into the builder
	if len(s.Directives) > 0 {
		parts = make([]string, len(s.Directives))

		for i := range s.Directives {
			parts[i] = s.Directives[i].String()
		}

		builder.WriteString(strings.Join(parts, " "))

		// write all attributes with comma separation into the builder
		if len(s.Attributes) > 0 {
			parts = make([]string, len(s.Attributes))

			for i := range s.Attributes {
				parts[i] = s.Attributes[i].String()
			}

			builder.WriteString("," + strings.Join(parts, ","))
		}

		// only write a newline if an alignment is specified or the content is not empty
		if s.Alignment >= P2align1 || len(s.Content) > 0 {
			builder.WriteString("\n")
		}
	}

	// write the alignment directive if the alignment is greater than or equal to P2align1
	// note: P2align1 is 0, which means no alignment
	if s.Alignment >= P2align1 {
		builder.WriteString(fmt.Sprintf("%v %v", P2align, s.Alignment))

		// only write a newline if the content is not empty
		if len(s.Content) > 0 {
			builder.WriteString("\n")
		}
	}

	// write all contents with a newline after each item (expect the last one)
	for i, content := range s.Content {
		builder.WriteString(content.String())

		if i < len(s.Content)-1 {
			builder.WriteString("\n")
		}
	}

	// the section string representation does not end with a newline
	return builder.String()
}

// String representation of a read-only data item.
func (rdi *ReadOnlyDataItem) String() string {
	var builder strings.Builder

	// write the literal data labels first and mark them with a local label prefix
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

		// write all code points with a newline after each item (expect the last one)
		for _, r := range utf32 {
			builder.WriteString(fmt.Sprintf("  %v 0x%08X\n", Long, uint32(r)))
		}

		// write the zero terminator to the UTF-32 string
		builder.WriteString(fmt.Sprintf("  %v 0x%08X", Long, uint32(0)))

	case ReadOnlyInt64:
		// encode to 64-bit integer based on supported Go data types
		switch v := rdi.Value.(type) {
		case int64:
			builder.WriteString(fmt.Sprintf("  %v 0x%016X", Quad, uint64(v)))

		case uint64:
			builder.WriteString(fmt.Sprintf("  %v 0x%016X", Quad, v))

		default:
			panic(cor.NewGeneralError(cor.Intel, failureMap, cor.Fatal, invalidReadOnlyDataValue, rdi.Value, nil))
		}

	default:
		panic(cor.NewGeneralError(cor.Intel, failureMap, cor.Fatal, unknownKindOfReadOnlyData, rdi.Kind, nil))
	}

	// the read-only data item string representation does not end with a newline
	return builder.String()
}
