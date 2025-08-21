// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package elf

import (
	"fmt"
	"strings"

	cor "github.com/petersen65/PL0/v2/core"
)

// Prefix and postfix for label names.
const labelPrefix = "."
const labelPostfix = ":"
const labelStartPostfix = "_start"
const labelEndPostfix = "_end"
const localPrefix = "L"

// Provide formats for labels that are used in ELF sections.
const labelFormat = labelPrefix + localPrefix + "%v" + labelPostfix
const startLabelFormat = labelPrefix + localPrefix + "%v" + labelStartPostfix + labelPostfix
const endLabelFormat = labelPrefix + localPrefix + "%v" + labelEndPostfix + labelPostfix

// Prefix for DWARF debugging information entries.
const debugEntryPrefix = labelPrefix + localPrefix + "die_"

// Prefix for DWARF string items.
const debugStringPrefix = labelPrefix + "str_"

// Provide a format for a string representation of a descriptor label.
const descriptorLabel = "%v.desc"

var (
	// Map DWARF code names to their string representation.
	dwarfCodeNames = map[DwarfCode]string{
		DW_CODE_compilation_unit: "DW_CODE_compilation_unit",
		DW_CODE_base_type:        "DW_CODE_base_type",
		DW_CODE_pointer_type:     "DW_CODE_pointer_type",
		DW_CODE_structure_type:   "DW_CODE_structure_type",
		DW_CODE_member:           "DW_CODE_member",
		DW_CODE_subprogram:       "DW_CODE_subprogram",
		DW_CODE_constant:         "DW_CODE_constant",
		DW_CODE_variable:         "DW_CODE_variable",
		DW_CODE_subprogram_main:  "DW_CODE_subprogram_main",
		DW_CODE_suppression:      "DW_CODE_suppression",
		DW_CODE_termination:      "DW_CODE_termination",
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

	// Map DWARF language names to their string representation.
	dwarfLanguageNames = map[DwarfLanguage]string{
		DW_LANG_C89:            "DW_LANG_C89",
		DW_LANG_C:              "DW_LANG_C",
		DW_LANG_Ada83:          "DW_LANG_Ada83",
		DW_LANG_C_plus_plus:    "DW_LANG_C_plus_plus",
		DW_LANG_Cobol74:        "DW_LANG_Cobol74",
		DW_LANG_Cobol85:        "DW_LANG_Cobol85",
		DW_LANG_Fortran77:      "DW_LANG_Fortran77",
		DW_LANG_Fortran90:      "DW_LANG_Fortran90",
		DW_LANG_Pascal83:       "DW_LANG_Pascal83",
		DW_LANG_Modula2:        "DW_LANG_Modula2",
		DW_LANG_Java:           "DW_LANG_Java",
		DW_LANG_C99:            "DW_LANG_C99",
		DW_LANG_Ada95:          "DW_LANG_Ada95",
		DW_LANG_Fortran95:      "DW_LANG_Fortran95",
		DW_LANG_PLI:            "DW_LANG_PLI",
		DW_LANG_ObjC:           "DW_LANG_ObjC",
		DW_LANG_ObjC_plus_plus: "DW_LANG_ObjC_plus_plus",
		DW_LANG_UPC:            "DW_LANG_UPC",
		DW_LANG_D:              "DW_LANG_D",
		DW_LANG_Python:         "DW_LANG_Python",
		DW_LANG_OpenCL:         "DW_LANG_OpenCL",
		DW_LANG_Go:             "DW_LANG_Go",
		DW_LANG_Modula3:        "DW_LANG_Modula3",
		DW_LANG_Haskell:        "DW_LANG_Haskell",
		DW_LANG_C_plus_plus_03: "DW_LANG_C_plus_plus_03",
		DW_LANG_C_plus_plus_11: "DW_LANG_C_plus_plus_11",
		DW_LANG_OCaml:          "DW_LANG_OCaml",
		DW_LANG_Rust:           "DW_LANG_Rust",
		DW_LANG_C11:            "DW_LANG_C11",
		DW_LANG_Swift:          "DW_LANG_Swift",
		DW_LANG_Julia:          "DW_LANG_Julia",
		DW_LANG_Dylan:          "DW_LANG_Dylan",
		DW_LANG_C_plus_plus_14: "DW_LANG_C_plus_plus_14",
		DW_LANG_Fortran03:      "DW_LANG_Fortran03",
		DW_LANG_Fortran08:      "DW_LANG_Fortran08",
		DW_LANG_RenderScript:   "DW_LANG_RenderScript",
		DW_LANG_Blend2D:        "DW_LANG_Blend2D",
		DW_LANG_C17:            "DW_LANG_C17",
		DW_LANG_C_plus_plus_17: "DW_LANG_C_plus_plus_17",
		DW_LANG_C_plus_plus_20: "DW_LANG_C_plus_plus_20",
		DW_LANG_C23:            "DW_LANG_C23",
		DW_LANG_C_plus_plus_23: "DW_LANG_C_plus_plus_23",
		DW_LANG_lo_user:        "DW_LANG_lo_user",
		DW_LANG_hi_user:        "DW_LANG_hi_user",
		DW_LANG_PL0_76:         "DW_LANG_PL0_76",
	}

	// Map DWARF attribute encodings to their string representation.
	dwarfAttributeEncodingNames = map[DwarfAttributeEncoding]string{
		DW_ATE_address:               "DW_ATE_address",
		DW_ATE_boolean:               "DW_ATE_boolean",
		DW_ATE_complex_float:         "DW_ATE_complex_float",
		DW_ATE_float:                 "DW_ATE_float",
		DW_ATE_signed:                "DW_ATE_signed",
		DW_ATE_signed_char:           "DW_ATE_signed_char",
		DW_ATE_unsigned:              "DW_ATE_unsigned",
		DW_ATE_unsigned_char:         "DW_ATE_unsigned_char",
		DW_ATE_imaginary_float:       "DW_ATE_imaginary_float",
		DW_ATE_packed_decimal:        "DW_ATE_packed_decimal",
		DW_ATE_numeric_string:        "DW_ATE_numeric_string",
		DW_ATE_edited:                "DW_ATE_edited",
		DW_ATE_signed_fixed:          "DW_ATE_signed_fixed",
		DW_ATE_unsigned_fixed:        "DW_ATE_unsigned_fixed",
		DW_ATE_decimal_float:         "DW_ATE_decimal_float",
		DW_ATE_UTF:                   "DW_ATE_UTF",
		DW_ATE_UCS:                   "DW_ATE_UCS",
		DW_ATE_ASCII:                 "DW_ATE_ASCII",
		DW_ATE_lo_user:               "DW_ATE_lo_user",
		DW_ATE_hi_user:               "DW_ATE_hi_user",
		DW_ATE_composite_no_encoding: "DW_ATE_composite_no_encoding",
	}

	// Map DWARF opcodes to their string representation.
	dwarfOpcodeNames = map[DwarfOpcode]string{
		DW_OP_consts:         "DW_OP_consts",
		DW_OP_constu:         "DW_OP_constu",
		DW_OP_fbreg:          "DW_OP_fbreg",
		DW_OP_breg0:          "DW_OP_bregN",
		DW_OP_call_frame_cfa: "DW_OP_call_frame_cfa",
		DW_OP_plus:           "DW_OP_plus",
		DW_OP_minus:          "DW_OP_minus",
		DW_OP_deref:          "DW_OP_deref",
		DW_OP_stack_value:    "DW_OP_stack_value",
	}
)

// String representation of a DWARF string item.
func (i *StringItem) String() string {
	const labelWidth = 30
	const directiveWidth = 10

	return fmt.Sprintf(
		"%-*v: %-*v\"%v\"",
		labelWidth, ToStringItemLabel(i.Label),
		directiveWidth, i.Directive,
		i.Operand,
	)
}

// String representation of an DWARF attribute form.
func (a *AttributeForm) String() string {
	const commentWidth = 25
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

	case uint32:
		return fmt.Sprintf("%-*v%#008x", directiveWidth, a.Directive, operand)

	case int64:
		return fmt.Sprintf("%-*v%v", directiveWidth, a.Directive, operand)

	case []byte:
		representation := make([]string, len(operand))

		for i, raw := range operand {
			representation[i] = fmt.Sprintf("%#002x", uint8(raw))
		}

		return fmt.Sprintf("%-*v%v", directiveWidth, a.Directive, strings.Join(representation, " "))

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

	// write the label only if it's not empty
	if e.Label != "" {
		builder.WriteString(fmt.Sprintf("%v\n", e.Label))
	}

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

// Create the byte sequence for a DWARF frame base register expression location.
func ToFrameBaseRegisterExpressionLocation(rbpVariableOffset, dwarfCfaOffset int64) []byte {
	// convert RBP based variable offset to CFA based offset
	cfaRelativeOffset := rbpVariableOffset - dwarfCfaOffset

	// encode signed fbreg offset to SLEB128
	slebCfaRelativeOffset := EncodeSleb128(cfaRelativeOffset)

	// the frame base register expression location contains: <ULEB128 length> <DWARF fbreg opcode> <SLEB128(d-16)>
	fbregExprLocLength := 1 + len(slebCfaRelativeOffset)

	// allocate the expression location byte slice with length 0 and capacity "fbregExprLoc"
	fbregExprLoc := make([]byte, 0, fbregExprLocLength)

	// encode unsigned fbreg expression location length to ULEB128
	ulebFbregExprLocLength := EncodeUleb128(uint64(fbregExprLocLength))

	// append the ULEB encoded length, the DWARF fbreg opcode, and the SLEB encoded signed offset to the frame base register expression location
	fbregExprLoc = append(fbregExprLoc, ulebFbregExprLocLength...)
	fbregExprLoc = append(fbregExprLoc, byte(DW_OP_fbreg))
	fbregExprLoc = append(fbregExprLoc, slebCfaRelativeOffset...)
	return fbregExprLoc
}

// EncodeSleb128 encodes a signed integer using DWARF SLEB128.
func EncodeSleb128(v int64) []byte {
	var out []byte
	more := true

	for more {
		// extract the low 7 bits of the current value
		b := byte(v & 0x7f)

		// check if the 6th bit (sign bit in this 7-bit chunk) is set
		// this is used to determine if we've encoded enough bits to represent the sign
		sign := (b & 0x40) != 0

		// shift the value right by 7 bits to prepare the next chunk
		v >>= 7

		// determine if more bytes are needed:
		//   - if v is 0 and the sign bit of the last chunk is clear (positive), encoding is complete
		//   - if v is -1 and the sign bit of the last chunk is set (negative), encoding is complete

		// this ensures proper sign extension when decoding
		if (v == 0 && !sign) || (v == -1 && sign) {
			more = false
		} else {
			// set the continuation bit (bit 7) to indicate more bytes follow
			b |= 0x80
		}

		out = append(out, b)
	}

	return out
}

// EncodeUleb128 encodes an unsigned integer using DWARF ULEB128.
func EncodeUleb128(u uint64) []byte {
	var out []byte

	for {
		// extract the low 7 bits of the current value
		b := byte(u & 0x7f)

		// shift the value right by 7 bits to prepare the next chunk
		u >>= 7

		if u != 0 {
			// more bytes are needed, set the continuation bit (bit 7)
			b |= 0x80
			out = append(out, b)
		} else {
			// this is the last byte, no continuation bit needed
			out = append(out, b)
			break
		}
	}

	return out
}

// DecodeSleb128 decodes a signed LEB128 value from a given byte sequence.
func DecodeSleb128(b []byte) (int64, int, error) {
	var result int64
	var shift uint
	var size = 64

	for i, v := range b {
		// extract the low 7 bits (data bits) from the current byte
		byteVal := int64(v & 0x7f)

		// shift the extracted bits to their proper position and OR them into the result
		result |= byteVal << shift

		// prepare for the next byte (each byte contributes 7 bits)
		shift += 7

		// check if this is the last byte (continuation bit is 0)
		if v&0x80 == 0 {
			// check if sign extension is needed: if bit 6 of the last byte is set (0x40), the number is negative and all higher bits must be filled with 1
			if (v&0x40) != 0 && shift < uint(size) {
				// sign extend by setting all bits above the current shift position to 1
				result |= -(1 << shift)
			}

			// return the decoded value and the number of bytes consumed
			return result, i + 1, nil
		}

		// check for overflow: cannot shift more than 64 bits for int64
		if shift >= uint(size) {
			return 0, 0, cor.NewGeneralError(cor.ExecutableLinkableFormat, failureMap, cor.Error, sleb128DecodingOverflow, len(b), nil)
		}
	}

	// if all bytes were exhausted without finding a terminating byte (continuation bit = 0), the decoding is incomplete
	return 0, 0, cor.NewGeneralError(cor.ExecutableLinkableFormat, failureMap, cor.Error, sleb128DecodingIncomplete, len(b), nil)
}

// DecodeUleb128 decodes an unsigned LEB128 value from a given byte sequence.
func DecodeUleb128(b []byte) (uint64, int, error) {
	var result uint64
	var shift uint
	var size = uint(64)

	for i, v := range b {
		// extract the low 7 bits (data bits) from the current byte and shift them to their proper position, then OR into the result
		result |= uint64(v&0x7f) << shift

		// check if this is the last byte (continuation bit is 0)
		if v&0x80 == 0 {
			// return the decoded value and the number of bytes consumed
			return result, i + 1, nil
		}

		// prepare for the next byte (each byte contributes 7 bits)
		shift += 7

		// check for overflow: cannot shift more than 64 bits for uint64
		if shift >= size {
			return 0, 0, cor.NewGeneralError(cor.ExecutableLinkableFormat, failureMap, cor.Error, uleb128DecodingOverflow, len(b), nil)
		}
	}

	// if all bytes were exhausted without finding a terminating byte (continuation bit = 0), the decoding is incomplete
	return 0, 0, cor.NewGeneralError(cor.ExecutableLinkableFormat, failureMap, cor.Error, uleb128DecodingIncomplete, len(b), nil)
}
