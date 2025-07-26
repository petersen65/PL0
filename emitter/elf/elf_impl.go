// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package elf

import (
	"fmt"
	"strings"

	cor "github.com/petersen65/PL0/v2/core"
)

const descriptorLabel = "%v.desc"

var (
	// Map directives to their string representation.
	directiveNames = map[Directive]string{
		Global:       ".global",
		Extern:       ".extern",
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
		StrDesc:      ".rodata.strdesc",
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

	// Map section attributes to their string representation.
	sectionAttributeNames = map[SectionAttribute]string{
		SectionAllocatable: "\"a\"",
		SectionWritable:    "\"w\"",
		SectionExecutable:  "\"x\"",
		SectionProgramBits: "@progbits",
		SectionNoBits:      "@nobits",
	}

	// Map type attributes to their string representation.
	typeAttributeNames = map[TypeAttribute]string{
		TypeFunction: "@function",
		TypeObject:   "@object",
		TypeCommon:   "@common",
		TypeTls:      "@tls_object",
		TypeIfunc:    "@gnu_indirect_function",
	}

	// Map size attributes to their string representation templates.
	sizeAttributeNames = map[SizeAttribute]string{
		SizeFromLabel:  ".-%s",
		SizeAbsolute:   "%d",
		SizeExpression: "%s",
	}
)

// String representation of an assembler section.
func (s *AssemblerSection[T]) String() string {
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

	// check if the values are nil, which is not allowed for read-only data items
	if rdi.Values == nil {
		panic(cor.NewGeneralError(cor.Intel, failureMap, cor.Fatal, invalidReadOnlyDataValue, nil, nil))
	}

	// encode different kinds of read-only data
	switch rdi.Kind {
	case ReadOnlyUtf32:
		var utf32 []rune

		// encode to UTF-32 string based on supported Go data types
		switch values := rdi.Values.(type) {
		case string:
			utf32 = []rune(values)

		case []rune:
			utf32 = values

		default:
			panic(cor.NewGeneralError(cor.Intel, failureMap, cor.Fatal, invalidReadOnlyDataValue, rdi.Values, nil))
		}

		// write all code points with a newline after each item (expect the last one)
		for _, r := range utf32 {
			builder.WriteString(fmt.Sprintf("  %v 0x%08X\n", Long, uint32(r)))
		}

		// write the zero terminator to the UTF-32 string
		builder.WriteString(fmt.Sprintf("  %v 0x%08X", Long, uint32(0)))

	case ReadOnlyInt64:
		// encode to 64-bit integers based on supported Go data types
		switch values := rdi.Values.(type) {
		case int64:
			builder.WriteString(fmt.Sprintf("  %v 0x%016X", Quad, uint64(values)))

		case uint64:
			builder.WriteString(fmt.Sprintf("  %v 0x%016X", Quad, values))

		case []int64:
			for i, value := range values {
				builder.WriteString(fmt.Sprintf("  %v 0x%016X", Quad, uint64(value)))

				if i < len(values)-1 {
					builder.WriteString("\n")
				}
			}

		case []uint64:
			for i, value := range values {
				builder.WriteString(fmt.Sprintf("  %v 0x%016X", Quad, value))

				if i < len(values)-1 {
					builder.WriteString("\n")
				}
			}

		default:
			panic(cor.NewGeneralError(cor.Intel, failureMap, cor.Fatal, invalidReadOnlyDataValue, rdi.Values, nil))
		}

	case ReadOnlyStrDesc:
		// encode to string descriptors with a 64-bit pointer and a 64-bit length based on supported Go data types
		switch values := rdi.Values.(type) {
		case string:
			builder.WriteString(fmt.Sprintf("  %v .L%v", Quad, values))

		case uint64:
			builder.WriteString(fmt.Sprintf("  %v 0x%016X", Quad, values))

		case []any:
			for i, value := range values {
				switch desc := value.(type) {
				case string:
					builder.WriteString(fmt.Sprintf("  %v .L%v", Quad, desc))

				case uint64:
					builder.WriteString(fmt.Sprintf("  %v 0x%016X", Quad, desc))

				default:
					panic(cor.NewGeneralError(cor.Intel, failureMap, cor.Fatal, invalidReadOnlyDataValue, value, nil))
				}

				if i < len(values)-1 {
					builder.WriteString("\n")
				}
			}
		}

	default:
		panic(cor.NewGeneralError(cor.Intel, failureMap, cor.Fatal, unknownKindOfReadOnlyData, rdi.Kind, nil))
	}

	// the read-only data item string representation does not end with a newline
	return builder.String()
}

// String representation of a directive detail.
func (dd *DirectiveDetail) String() string {
	// if no arguments are provided, just return the directive and symbol
	if len(dd.Arguments) == 0 {
		return fmt.Sprintf("%s %s", dd.Directive, dd.Symbol)
	}

	// join symbol and arguments with commas for multi-argument directives
	args := append([]string{dd.Symbol}, dd.Arguments...)
	return fmt.Sprintf("%s %s", dd.Directive, strings.Join(args, ", "))
}

// Append content to the assembly section.
func (s *AssemblerSection[T]) Append(content T) {
	s.Content = append(s.Content, content)
}
