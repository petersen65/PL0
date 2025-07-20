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
	// Map directive kinds to their string representation.
	directiveKindNames = map[DirectiveKind]string{
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

	// Map read-only data kinds to their section and alignment representations.
	readOnlyDataKindDetails = map[ReadOnlyDataKind]ReadOnlyDataItemDetails{
		ReadOnlyUtf32: {Section: ".section .rodata.str4.4,\"a\",@progbits", Alignment: ".p2align 2"},
		ReadOnlyInt64: {Section: ".section .rodata.int8.8,\"a\",@progbits", Alignment: ".p2align 3"},
	}
)

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
