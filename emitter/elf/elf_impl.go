// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package elf

import (
	"fmt"
	"slices"
	"strings"

	cor "github.com/petersen65/PL0/v2/core"
)

// Provide a format for a comment.
const commentFormat = "# %v\n"

// Provide a format for a string representation of a descriptor label.
const descriptorLabel = "%v.desc"

// Prefix for label names.
const labelPrefix = "."

// Provide formats for labels that are used in ELF sections.
const labelFormat = ".L%v:\n"
const startLabelFormat = ".L%v_start:\n"
const endLabelFormat = "\n.L%v_end:"

// Prefix for DWARF string items.
const debugStringPrefix = ".str_"

var (
	// Map a boolean value to a 0 or 1 integer representation.
	boolToIntMap = map[bool]int8{
		true:  1,
		false: 0,
	}

	// Map directives to their string representation.
	directiveNames = map[DirectiveKind]string{
		IntelSyntax:       ".intel_syntax",
		AttSyntax:         ".att_syntax",
		PushSection:       ".pushsection",
		PopSection:        ".popsection",
		Previous:          ".previous",
		Global:            ".global",
		Extern:            ".extern",
		Type:              ".type",
		Size:              ".size",
		Weak:              ".weak",
		Hidden:            ".hidden",
		Ident:             ".ident",
		Section:           ".section",
		Text:              ".text",
		Data:              ".data",
		Rodata:            ".rodata",
		Bss:               ".bss",
		Utf32:             ".rodata.str4.4",
		Int64:             ".rodata.int8.8",
		StrDesc:           ".rodata.strdesc",
		DebugAbbrev:       ".debug_abbrev",
		DebugInfo:         ".debug_info",
		DebugStr:          ".debug_str",
		P2align:           ".p2align",
		Align:             ".align",
		Balign:            ".balign",
		Byte:              ".byte",
		Word:              ".word",
		Long:              ".long",
		Quad:              ".quad",
		Zero:              ".zero",
		String:            ".string",
		Ascii:             ".ascii",
		Uleb128:           ".uleb128",
		Sleb128:           ".sleb128",
		File:              ".file",
		Loc:               ".loc",
		Line:              ".line",
		CfiStartProc:      ".cfi_startproc",
		CfiEndProc:        ".cfi_endproc",
		CfiDefCfa:         ".cfi_def_cfa",
		CfiOffset:         ".cfi_offset",
		CfiDefCfaOffset:   ".cfi_def_cfa_offset",
		CfiDefCfaRegister: ".cfi_def_cfa_register",
		CfiRestore:        ".cfi_restore",
		CfiUndefined:      ".cfi_undefined",
		CfiEscape:         ".cfi_escape",
	}

	// Map prefix attributes to their string representation.
	prefixAttributeNames = map[PrefixAttribute]string{
		IntelPrefix:   "prefix",
		IntelNoPrefix: "noprefix",
	}

	// Map section attributes to their string representation.
	sectionAttributeNames = map[SectionAttribute]string{
		SectionNone:             "\"\"",
		SectionAllocatable:      "\"a\"",
		SectionWritable:         "\"w\"",
		SectionExecutable:       "\"x\"",
		SectionMergeable:        "\"M\"",
		SectionStrings:          "\"S\"",
		SectionMergeableStrings: "\"MS\"",
		SectionProgramBits:      "@progbits",
		SectionNoBits:           "@nobits",
	}

	// Map file attributes to their string representation.
	fileAttributeNames = map[FileAttribute]string{
		FileId:        "%d",
		FileName:      "\"%s\"",
		FileDelimiter: " ",
	}

	// Map location attributes to their string representation.
	locationAttributeNames = map[LocationAttribute]string{
		LocationFileId:                     "%d",
		LocationLine:                       "%d",
		LocationColumn:                     "%d",
		LocationBasicBlock:                 "basic_block",
		LocationPrologueEnd:                "prologue_end",
		LocationEpilogueBegin:              "epilogue_begin",
		LocationIsStatement:                "is_stmt %d",
		LocationInstructionSetArchitecture: "isa %d",
		LocationDiscriminator:              "discriminator %d",
		LocationDelimiter:                  " ",
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
		SizeLabel:      ".-%s",
		SizeAbsolute:   "%d",
		SizeExpression: "%s",
	}

	// Map call frame information attributes to their string representation.
	callFrameInformationAttributeNames = map[CallFrameInformationAttribute]string{
		CallFrameInformationOffset: "%d",
	}
)

// Create a .file directive with a file identifier and name.
func newFile(id int, name string) *Directive {
	return NewDirective(File, nil,
		fmt.Sprintf(strings.Join([]string{
			FileId.String(),
			FileName.String()},
			FileDelimiter.String(),
		), id, name),
	)
}

// Create a .loc directive for source locations in debug info.
func newLocation(id, line, column int, debugger Debugger, attributes ...string) *Directive {
	if !slices.Contains(attributes, LocationPrologueEnd.String()) && debugger&DebuggerPrologueEnd != 0 {
		attributes = append(attributes, LocationPrologueEnd.String())
	}

	if !slices.Contains(attributes, LocationEpilogueBegin.String()) && debugger&DebuggerEpilogueBegin != 0 {
		attributes = append(attributes, LocationEpilogueBegin.String())
	}

	return NewDirective(Loc, nil,
		fmt.Sprintf(strings.Join([]string{
			LocationFileId.String(),
			LocationLine.String(),
			LocationColumn.String(),
			strings.Join(attributes, LocationDelimiter.String())},
			LocationDelimiter.String(),
		), id, line, column),
	)
}

// String representation of an ELF section.
func (s *ElfSection[T]) String() string {
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

	// write a start label if the section requires offset calculations
	if len(s.Directives) > 1 && s.Offsets {
		builder.WriteString(fmt.Sprintf(startLabelFormat, strings.TrimLeft(s.Directives[1].String(), labelPrefix)))
	}

	// write all contents with a newline after each item (expect the last one)
	for i, content := range s.Content {
		builder.WriteString(content.String())

		if i < len(s.Content)-1 {
			builder.WriteString("\n")
		}
	}

	// write an end label if the section requires offset calculations
	if len(s.Directives) > 1 && s.Offsets {
		builder.WriteString(fmt.Sprintf(endLabelFormat, strings.TrimLeft(s.Directives[1].String(), labelPrefix)))
	}

	// the section string representation does not end with a newline
	return builder.String()
}

// String representation of a read-only data item.
func (rdi *ReadOnlyDataItem) String() string {
	var builder strings.Builder

	// write the literal data labels first and mark them with a local label prefix
	for _, label := range rdi.Labels {
		builder.WriteString(fmt.Sprintf(labelFormat, label))
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

// String representation of an abbreviation entry.
func (ae AbbreviationEntry) String() string {
    var builder strings.Builder

	// write the abbreviation code and tag as attribute-list parent
    builder.WriteString(fmt.Sprintf("%v 0x%02x  # entry code = %d\n", Uleb128, ae.Code, ae.Code))
    builder.WriteString(fmt.Sprintf("%v 0x%02x  # entry tag = %d\n", Uleb128, ae.Tag, ae.Tag))

	// has children flag
    builder.WriteString(fmt.Sprintf("%v    0x%02x  # has children flag\n", Byte, boolToIntMap[ae.HasChildren]))

	// write the abbreviation attribute-list that are children of the entry
    for _, attr := range ae.Attributes {
        builder.WriteString(attr.String())
        builder.WriteByte('\n')
    }

	// zero terminator
    builder.WriteString(fmt.Sprintf("%v 0x00  # end of attribute-list\n", Uleb128))

	// the abbreviation entry string representation ends with a newline
    return builder.String()
}

// String representation of an abbreviation attribute.
func (aa AbbreviationAttribute) String() string {
    return fmt.Sprintf("    %v 0x%02x %v 0x%02x", Uleb128, aa.Attribute, Uleb128, aa.Form)
}

// String representation of a DWARF string item.
func (i *StringItem) String() string {
	const labelWidth = 30
	const directiveWidth = 10

	return fmt.Sprintf(
		"%v%-*v: %-*v \"%v\"",
		debugStringPrefix,
		labelWidth, i.Label,
		directiveWidth, i.Directive,
		i.Operand,
	)
}

// String representation of a directive.
func (dd *Directive) String() string {
	var comments string

	// optional comments to be emitted before the directive
	for _, comment := range dd.Comments {
		comments += fmt.Sprintf(commentFormat, comment)
	}

	// join symbol and arguments with commas for multi-argument directives
	parts := strings.Join(append(dd.Symbols, dd.Arguments...), ", ")
	return strings.TrimSpace(fmt.Sprintf("%v%v %v", comments, dd.Directive, parts))
}

// Append content to the assembly section.
func (s *ElfSection[T]) Append(content T) {
	s.Content = append(s.Content, content)
}
