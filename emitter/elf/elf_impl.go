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
const commentFormat = "# %v"

// Provide a format for a string representation of a descriptor label.
const descriptorLabel = "%v.desc"

// Prefix and postfix for label names.
const labelPrefix = "."
const labelPostfix = ":"

// Provide formats for labels that are used in ELF sections.
const labelFormat = labelPrefix + "L%v" + labelPostfix
const startLabelFormat = labelPrefix + "L%v_start" + labelPostfix
const endLabelFormat = labelPrefix + "L%v_end" + labelPostfix

// Prefix for DWARF string items.
const debugStringPrefix = labelPrefix + "str_"

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
		Short:             ".short",
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

	// Map DWARF code names to their string representation.
	dwarfCodeNames = map[DwarfCode]string{
		DW_CODE_termination:      "DW_CODE_termination",
		DW_CODE_compilation_unit: "DW_CODE_compilation_unit",
		DW_CODE_base_type:        "DW_CODE_base_type",
		DW_CODE_subprogram:       "DW_CODE_subprogram",
		DW_CODE_variable:         "DW_CODE_variable",
	}

	// Map DWARF tag names to their string representation.
	dwarfTagNames = map[DwarfTag]string{
		DW_TAG_array_type:           "DW_TAG_array_type",
		DW_TAG_class_type:           "DW_TAG_class_type",
		DW_TAG_enumeration_type:     "DW_TAG_enumeration_type",
		DW_TAG_pointer_type:         "DW_TAG_pointer_type",
		DW_TAG_reference_type:       "DW_TAG_reference_type",
		DW_TAG_compile_unit:         "DW_TAG_compile_unit",
		DW_TAG_string_type:          "DW_TAG_string_type",
		DW_TAG_structure_type:       "DW_TAG_structure_type",
		DW_TAG_subroutine_type:      "DW_TAG_subroutine_type",
		DW_TAG_typedef:              "DW_TAG_typedef",
		DW_TAG_union_type:           "DW_TAG_union_type",
		DW_TAG_subrange_type:        "DW_TAG_subrange_type",
		DW_TAG_base_type:            "DW_TAG_base_type",
		DW_TAG_const_type:           "DW_TAG_const_type",
		DW_TAG_volatile_type:        "DW_TAG_volatile_type",
		DW_TAG_restrict_type:        "DW_TAG_restrict_type",
		DW_TAG_shared_type:          "DW_TAG_shared_type",
		DW_TAG_enumerator:           "DW_TAG_enumerator",
		DW_TAG_constant:             "DW_TAG_constant",
		DW_TAG_variable:             "DW_TAG_variable",
		DW_TAG_member:               "DW_TAG_member",
		DW_TAG_formal_parameter:     "DW_TAG_formal_parameter",
		DW_TAG_subprogram:           "DW_TAG_subprogram",
		DW_TAG_inlined_subroutine:   "DW_TAG_inlined_subroutine",
		DW_TAG_lexical_block:        "DW_TAG_lexical_block",
		DW_TAG_try_block:            "DW_TAG_try_block",
		DW_TAG_catch_block:          "DW_TAG_catch_block",
		DW_TAG_namespace:            "DW_TAG_namespace",
		DW_TAG_module:               "DW_TAG_module",
		DW_TAG_imported_unit:        "DW_TAG_imported_unit",
		DW_TAG_imported_declaration: "DW_TAG_imported_declaration",
		DW_TAG_common_block:         "DW_TAG_common_block",
		DW_TAG_common_inclusion:     "DW_TAG_common_inclusion",
		DW_TAG_entry_point:          "DW_TAG_entry_point",
		DW_TAG_unspecified_type:     "DW_TAG_unspecified_type",
		DW_TAG_lo_user:              "DW_TAG_lo_user",
		DW_TAG_hi_user:              "DW_TAG_hi_user",
	}

	// Map DWARF attribute names to their string representation.
	dwarfAttributeNames = map[DwarfAttribute]string{
		DW_AT_sibling:                 "DW_AT_sibling",
		DW_AT_location:                "DW_AT_location",
		DW_AT_name:                    "DW_AT_name",
		DW_AT_ordering:                "DW_AT_ordering",
		DW_AT_byte_size:               "DW_AT_byte_size",
		DW_AT_bit_offset:              "DW_AT_bit_offset",
		DW_AT_bit_size:                "DW_AT_bit_size",
		DW_AT_stmt_list:               "DW_AT_stmt_list",
		DW_AT_low_pc:                  "DW_AT_low_pc",
		DW_AT_high_pc:                 "DW_AT_high_pc",
		DW_AT_language:                "DW_AT_language",
		DW_AT_discr:                   "DW_AT_discr",
		DW_AT_discr_value:             "DW_AT_discr_value",
		DW_AT_visibility:              "DW_AT_visibility",
		DW_AT_import:                  "DW_AT_import",
		DW_AT_string_length:           "DW_AT_string_length",
		DW_AT_common_reference:        "DW_AT_common_reference",
		DW_AT_comp_dir:                "DW_AT_comp_dir",
		DW_AT_const_value:             "DW_AT_const_value",
		DW_AT_containing_type:         "DW_AT_containing_type",
		DW_AT_default_value:           "DW_AT_default_value",
		DW_AT_inline:                  "DW_AT_inline",
		DW_AT_is_optional:             "DW_AT_is_optional",
		DW_AT_lower_bound:             "DW_AT_lower_bound",
		DW_AT_producer:                "DW_AT_producer",
		DW_AT_prototyped:              "DW_AT_prototyped",
		DW_AT_return_addr:             "DW_AT_return_addr",
		DW_AT_start_scope:             "DW_AT_start_scope",
		DW_AT_stride:                  "DW_AT_stride",
		DW_AT_upper_bound:             "DW_AT_upper_bound",
		DW_AT_abstract_origin:         "DW_AT_abstract_origin",
		DW_AT_accessibility:           "DW_AT_accessibility",
		DW_AT_address_class:           "DW_AT_address_class",
		DW_AT_artificial:              "DW_AT_artificial",
		DW_AT_base_types:              "DW_AT_base_types",
		DW_AT_calling_convention:      "DW_AT_calling_convention",
		DW_AT_count:                   "DW_AT_count",
		DW_AT_data_member_location:    "DW_AT_data_member_location",
		DW_AT_decl_column:             "DW_AT_decl_column",
		DW_AT_decl_file:               "DW_AT_decl_file",
		DW_AT_decl_line:               "DW_AT_decl_line",
		DW_AT_declaration:             "DW_AT_declaration",
		DW_AT_discr_list:              "DW_AT_discr_list",
		DW_AT_encoding:                "DW_AT_encoding",
		DW_AT_decimal_sign:            "DW_AT_decimal_sign",
		DW_AT_external:                "DW_AT_external",
		DW_AT_frame_base:              "DW_AT_frame_base",
		DW_AT_friend:                  "DW_AT_friend",
		DW_AT_identifier_case:         "DW_AT_identifier_case",
		DW_AT_macro_info:              "DW_AT_macro_info",
		DW_AT_namelist_item:           "DW_AT_namelist_item",
		DW_AT_priority:                "DW_AT_priority",
		DW_AT_segment:                 "DW_AT_segment",
		DW_AT_specification:           "DW_AT_specification",
		DW_AT_static_link:             "DW_AT_static_link",
		DW_AT_type:                    "DW_AT_type",
		DW_AT_use_location:            "DW_AT_use_location",
		DW_AT_variable_parameter:      "DW_AT_variable_parameter",
		DW_AT_virtuality:              "DW_AT_virtuality",
		DW_AT_vtable_elem_location:    "DW_AT_vtable_elem_location",
		DW_AT_allocated:               "DW_AT_allocated",
		DW_AT_associated:              "DW_AT_associated",
		DW_AT_data_location:           "DW_AT_data_location",
		DW_AT_byte_stride:             "DW_AT_byte_stride",
		DW_AT_entry_pc:                "DW_AT_entry_pc",
		DW_AT_use_UTF8:                "DW_AT_use_UTF8",
		DW_AT_extension:               "DW_AT_extension",
		DW_AT_ranges:                  "DW_AT_ranges",
		DW_AT_trampoline:              "DW_AT_trampoline",
		DW_AT_call_column:             "DW_AT_call_column",
		DW_AT_call_file:               "DW_AT_call_file",
		DW_AT_call_line:               "DW_AT_call_line",
		DW_AT_description:             "DW_AT_description",
		DW_AT_binary_scale:            "DW_AT_binary_scale",
		DW_AT_decimal_scale:           "DW_AT_decimal_scale",
		DW_AT_small:                   "DW_AT_small",
		DW_AT_digit_count:             "DW_AT_digit_count",
		DW_AT_picture_string:          "DW_AT_picture_string",
		DW_AT_mutable:                 "DW_AT_mutable",
		DW_AT_threads_scaled:          "DW_AT_threads_scaled",
		DW_AT_explicit:                "DW_AT_explicit",
		DW_AT_object_pointer:          "DW_AT_object_pointer",
		DW_AT_endianity:               "DW_AT_endianity",
		DW_AT_elemental:               "DW_AT_elemental",
		DW_AT_pure:                    "DW_AT_pure",
		DW_AT_recursive:               "DW_AT_recursive",
		DW_AT_signature:               "DW_AT_signature",
		DW_AT_main_subprogram:         "DW_AT_main_subprogram",
		DW_AT_data_bit_offset:         "DW_AT_data_bit_offset",
		DW_AT_const_expr:              "DW_AT_const_expr",
		DW_AT_enum_class:              "DW_AT_enum_class",
		DW_AT_linkage_name:            "DW_AT_linkage_name",
		DW_AT_string_length_bit_size:  "DW_AT_string_length_bit_size",
		DW_AT_string_length_byte_size: "DW_AT_string_length_byte_size",
		DW_AT_rank:                    "DW_AT_rank",
		DW_AT_str_offsets_base:        "DW_AT_str_offsets_base",
		DW_AT_addr_base:               "DW_AT_addr_base",
		DW_AT_rnglists_base:           "DW_AT_rnglists_base",
		DW_AT_dwo_name:                "DW_AT_dwo_name",
		DW_AT_reference:               "DW_AT_reference",
		DW_AT_dwo_id:                  "DW_AT_dwo_id",
		DW_AT_dwo_file:                "DW_AT_dwo_file",
		DW_AT_dwo_line:                "DW_AT_dwo_line",
		DW_AT_stmt_list_base:          "DW_AT_stmt_list_base",
		DW_AT_loclists_base:           "DW_AT_loclists_base",
		DW_AT_lo_user:                 "DW_AT_lo_user",
		DW_AT_hi_user:                 "DW_AT_hi_user",
	}

	// Map DWARF form names to their string representation.
	dwarfFormNames = map[DwarfForm]string{
		DW_FORM_addr:           "DW_FORM_addr",
		DW_FORM_block2:         "DW_FORM_block2",
		DW_FORM_block4:         "DW_FORM_block4",
		DW_FORM_data2:          "DW_FORM_data2",
		DW_FORM_data4:          "DW_FORM_data4",
		DW_FORM_data8:          "DW_FORM_data8",
		DW_FORM_string:         "DW_FORM_string",
		DW_FORM_block:          "DW_FORM_block",
		DW_FORM_block1:         "DW_FORM_block1",
		DW_FORM_data1:          "DW_FORM_data1",
		DW_FORM_flag:           "DW_FORM_flag",
		DW_FORM_sdata:          "DW_FORM_sdata",
		DW_FORM_strp:           "DW_FORM_strp",
		DW_FORM_udata:          "DW_FORM_udata",
		DW_FORM_ref_addr:       "DW_FORM_ref_addr",
		DW_FORM_ref1:           "DW_FORM_ref1",
		DW_FORM_ref2:           "DW_FORM_ref2",
		DW_FORM_ref4:           "DW_FORM_ref4",
		DW_FORM_ref8:           "DW_FORM_ref8",
		DW_FORM_ref_udata:      "DW_FORM_ref_udata",
		DW_FORM_indirect:       "DW_FORM_indirect",
		DW_FORM_sec_offset:     "DW_FORM_sec_offset",
		DW_FORM_exprloc:        "DW_FORM_exprloc",
		DW_FORM_flag_present:   "DW_FORM_flag_present",
		DW_FORM_strx:           "DW_FORM_strx",
		DW_FORM_addrx:          "DW_FORM_addrx",
		DW_FORM_ref_sup4:       "DW_FORM_ref_sup4",
		DW_FORM_strp_sup:       "DW_FORM_strp_sup",
		DW_FORM_data16:         "DW_FORM_data16",
		DW_FORM_line_strp:      "DW_FORM_line_strp",
		DW_FORM_ref_sig8:       "DW_FORM_ref_sig8",
		DW_FORM_implicit_const: "DW_FORM_implicit_const",
		DW_FORM_loclistx:       "DW_FORM_loclistx",
		DW_FORM_rnglistx:       "DW_FORM_rnglistx",
		DW_FORM_ref_sup8:       "DW_FORM_ref_sup8",
		DW_FORM_strx1:          "DW_FORM_strx1",
		DW_FORM_strx2:          "DW_FORM_strx2",
		DW_FORM_strx3:          "DW_FORM_strx3",
		DW_FORM_strx4:          "DW_FORM_strx4",
	}

	// Map DWARF unit-type names to their string representation.
	dwarfUnitTypeNames = map[DwarfUnitType]string{
		DW_UT_compile:       "DW_UT_compile",
		DW_UT_type:          "DW_UT_type",
		DW_UT_partial:       "DW_UT_partial",
		DW_UT_skeleton:      "DW_UT_skeleton",
		DW_UT_split_compile: "DW_UT_split_compile",
		DW_UT_split_type:    "DW_UT_split_type",
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
		builder.WriteString(s.Directives[1].StartLabel())
		builder.WriteString("\n")
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
		builder.WriteString("\n")
		builder.WriteString(s.Directives[1].EndLabel())
	}

	// the section string representation does not end with a newline
	return builder.String()
}

// String representation of a directive.
func (d *Directive) String() string {
	var comments string

	// optional comments to be emitted before the directive
	for _, comment := range d.Comments {
		comments += fmt.Sprintf(commentFormat, comment) + "\n"
	}

	// join symbol and arguments with commas for multi-argument directives
	parts := strings.Join(append(d.Symbols, d.Arguments...), ", ")
	return strings.TrimSpace(fmt.Sprintf("%v%v %v", comments, d.Directive, parts))
}

// String representation of a read-only data item.
func (rdi *ReadOnlyDataItem) String() string {
	var builder strings.Builder

	// write the literal data labels first and mark them with a local label prefix
	for _, label := range rdi.Labels {
		builder.WriteString(fmt.Sprintf(labelFormat, label))
		builder.WriteString("\n")
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
			builder.WriteString(fmt.Sprintf("%v%v %#008x\n", DefaultIndentation, Long, uint32(r)))
		}

		// write the zero terminator to the UTF-32 string
		builder.WriteString(fmt.Sprintf("%v%v %#008x", DefaultIndentation, Long, uint32(0)))

	case ReadOnlyInt64:
		// encode to 64-bit integers based on supported Go data types
		switch values := rdi.Values.(type) {
		case int64:
			builder.WriteString(fmt.Sprintf("%v%v %#016x", DefaultIndentation, Quad, uint64(values)))

		case uint64:
			builder.WriteString(fmt.Sprintf("%v%v %#016x", DefaultIndentation, Quad, values))

		case []int64:
			for i, value := range values {
				builder.WriteString(fmt.Sprintf("%v%v %#016x", DefaultIndentation, Quad, uint64(value)))

				if i < len(values)-1 {
					builder.WriteString("\n")
				}
			}

		case []uint64:
			for i, value := range values {
				builder.WriteString(fmt.Sprintf("%v%v %#016x", DefaultIndentation, Quad, value))

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
			builder.WriteString(fmt.Sprintf("%v%v .L%v", DefaultIndentation, Quad, values))

		case uint64:
			builder.WriteString(fmt.Sprintf("%v%v %#016x", DefaultIndentation, Quad, values))

		case []any:
			for i, value := range values {
				switch desc := value.(type) {
				case string:
					builder.WriteString(fmt.Sprintf("%v%v .L%v", DefaultIndentation, Quad, desc))

				case uint64:
					builder.WriteString(fmt.Sprintf("%v%v %#016x", DefaultIndentation, Quad, desc))

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

// String representation of a DWARF string item.
func (i *StringItem) String() string {
	const labelWidth = 30
	const directiveWidth = 10

	return fmt.Sprintf(
		"%v%-*v: %-*v\"%v\"",
		debugStringPrefix,
		labelWidth, i.Label,
		directiveWidth, i.Directive,
		i.Operand,
	)
}

// String representation of an DWARF attribute form.
func (a *AttributeForm) String() string {
	const commentWidth = 20
	const encodingWidth = 10

	comment := strings.TrimSpace(fmt.Sprintf(
		commentFormat,
		fmt.Sprintf(
			"%-*v%v%-*v",
			commentWidth,
			a.Attribute,
			DefaultIndentation,
			commentWidth,
			a.Form,
		)))

	return fmt.Sprintf(
		"%-*v%#002x %-*v%#002x%v%v",
		encodingWidth, Uleb128,
		uint8(a.Attribute),
		encodingWidth, Uleb128,
		uint8(a.Form),
		DefaultIndentation,
		comment,
	)
}

// String representation of a DWARF attribute item.
func (a *AttributeItem) String() string {
	const directiveWidth = 10

	switch operand := a.Operand.(type) {
	case uint8:
		return fmt.Sprintf("%-*v%#002x", directiveWidth, a.Directive, operand)

	case uint16:
		return fmt.Sprintf("%-*v%#004x", directiveWidth, a.Directive, operand)

	default:
		return fmt.Sprintf("%-*v%v", directiveWidth, a.Directive, a.Operand)
	}
}

// String representation of a DWARF abbreviation entry.
func (e *AbbreviationEntry) String() string {
	const EncodingWidth = 10
	var builder strings.Builder

	// write the abbreviation code
	builder.WriteString(fmt.Sprintf(
		"%-*v%#002x%v"+commentFormat,
		EncodingWidth, Uleb128,
		uint8(e.Code),
		DefaultIndentation,
		e.Code,
	))

	// if the abbreviation code is the termination code, only return the representation of the code without a newline
	if e.Code == DW_CODE_termination {
		return builder.String()
	}

	// write the abbreviation tag
	builder.WriteString(fmt.Sprintf(
		"\n%-*v%#002x%v"+commentFormat,
		EncodingWidth, Uleb128,
		uint8(e.Tag),
		DefaultIndentation,
		e.Tag))

	// has children flag
	builder.WriteString(fmt.Sprintf(
		"\n%-*v%#002x",
		EncodingWidth, Byte,
		boolToIntMap[e.HasChildren],
	))

	// write the attribute-list
	for _, attribute := range e.Attributes {
		builder.WriteString(fmt.Sprintf(
			"\n%v%v",
			DefaultIndentation,
			attribute,
		))
	}

	// zero terminator
	builder.WriteString(fmt.Sprintf(
		"\n%-*v%#002x",
		EncodingWidth, Uleb128, 0,
	))

	// the abbreviation entry string representation does not end with a newline
	return builder.String()
}

// String representation of a DWARF debugging information entry.
func (e *DebuggingInformationEntry) String() string {
	const EncodingWidth = 10
	var builder strings.Builder

	// write the abbreviation code only if it's not the suppression code
	if e.Code != DW_CODE_suppression {
		builder.WriteString(fmt.Sprintf(
			"%-*v%#002x\n",
			EncodingWidth, Uleb128,
			uint8(e.Code),
		))
	}

	// write the attribute-list of the debugging information entry
	for i, attribute := range e.Attributes {
		builder.WriteString(attribute.String())

		if i < len(e.Attributes)-1 {
			builder.WriteString("\n")
		}
	}

	// the debugging information entry string representation does not end with a newline
	return builder.String()
}

// Append content to the assembly section.
func (s *ElfSection[T]) Append(content T) {
	s.Content = append(s.Content, content)
}
