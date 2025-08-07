// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package elf defines constants and types for the executable and linkable format (ELF) used in assembly code generation.
// Note: ELF is part the System V ABI for x86_64 architecture and is used for linking and loading executable files.
package elf

import (
	"fmt"
	"strings"
)

// Provide a default indentation for directives and instructions.
const DefaultIndentation = "  "

// Standard DWARF label names for ELF sections.
const (
	CompilationUnitLabel = "compilation_unit" // label for the compilation unit in .debug_str section
	ProducerLabel        = "producer"         // label for the producer in .debug_str section
)

// Debugger flags augment the directive generation with additional information (bit-mask).
const (
	DebuggerNone          Debugger = 0         // no debugger flags set
	DebuggerPrologueEnd   Debugger = 1 << iota // last instruction of prologue was generated
	DebuggerEpilogueBegin                      // first instruction of epilogue was generated
)

// Assembler directives for the ELF format supported by various assemblers on Linux.
const (
	// assembly language syntax directives
	IntelSyntax DirectiveKind = iota // switches the assembler to Intel syntax (.intel_syntax)
	AttSyntax                        // switches the assembler to AT&T syntax (.att_syntax)

	// section management directives
	PushSection // pushes a section on the stack (.pushsection)
	PopSection  // pops a section from the stack (.popsection)
	Previous    // switches back to the previous section (.previous)

	// symbol declaration directives
	Global // declares a global symbol visible to the linker (.globl <name>)
	Extern // declares an external symbol defined elsewhere (.extern <name>)
	Type   // specifies the symbol type for ELF (.type <name>, @function/@object/...)
	Size   // declares the size of a symbol in bytes (.size <name>, .-<name>)
	Weak   // marks a symbol as weak; can be overridden (.weak <name>)
	Hidden // makes a symbol hidden from external linking (.hidden <name>)
	Ident  // embeds an identification string (.ident)

	// section control directives
	Section // changes or creates a named section (.section <name>[, "flags", @type])
	Text    // switches to the .text (code) section (.text)
	Data    // switches to the .data (writable data) section (.data)
	Rodata  // switches to the .rodata (read-only data) section (.rodata)
	Bss     // switches to the .bss (zero-initialized data) section (.bss)

	// read-only data directives with specific encodings
	Utf32   // switches to the .rodata.str4.4 (UTF-32 strings) section (.rodata.str4.4,"a",@progbits)
	Int64   // switches to the .rodata.int8.8 (64-bit integers) section (.rodata.int8.8,"a",@progbits)
	StrDesc // switches to the .rodata.strdesc (string descriptors) section (.rodata.strdesc,"a",@progbits)

	// DWARF debug information directives
	DebugAbbrev // switches to the .debug_abbrev (debug abbreviation) section (.debug_abbrev,"",@progbits)
	DebugInfo   // switches to the .debug_info (debug information) section (.debug_info,"",@progbits)
	DebugStr    // switches to the .debug_str (debug strings) section (.debug_str,"MS",@progbits)

	// alignment and layout directives
	P2align // aligns to 2^n bytes (.p2align <n>) — used for ABI alignment
	Align   // aligns to n bytes (.align <n>) — less portable across platforms
	Balign  // aligns using byte-boundary logic (.balign <n>) — same as .align but stricter in ELF

	// value emission directives
	Byte    // emits a single byte value (.byte 0xFF)
	Word    // emits a 16-bit word (.word 0xABCD)
	Long    // emits a 32-bit value (.long 0x12345678)
	Quad    // emits a 64-bit value (.quad 0xDEADBEEFCAFEBABE)
	Zero    // emits a run of zero bytes (.zero <count>)
	String  // emits a null-terminated ASCII string (.string "text")
	Ascii   // emits an unterminated ASCII string (.ascii "text")
	Uleb128 // emits an unsigned LEB128 value (.uleb128 0x1234, unsigned little-endian base 128 7-bit encoding)
	Sleb128 // emits a signed LEB128 value (.sleb128 -1234, signed little-endian base 128 7-bit encoding)

	// debug info and DWARF metadata
	File              // specifies the source file name for debug info (.file "source.c")
	Loc               // marks source location in debug info (.loc file line column)
	Line              // specifies source line number (legacy) (.line <n>)
	CfiStartProc      // begins a CFI frame for a function (.cfi_startproc)
	CfiEndProc        // ends a CFI frame for a function (.cfi_endproc)
	CfiDefCfa         // defines CFA canonical frame address rule (.cfi_def_cfa reg offset)
	CfiOffset         // defines register offset in CFI (.cfi_offset reg offset)
	CfiDefCfaOffset   // adjusts CFA offset (.cfi_def_cfa_offset)
	CfiDefCfaRegister // changes the CFA register (.cfi_def_cfa_register)
	CfiRestore        // restores a register from saved state (.cfi_restore)
	CfiUndefined      // marks a register as undefined (.cfi_undefined)
	CfiEscape         // emits raw bytes into the CFI stream (.cfi_escape)
)

// Prefix attributes for the .intel_syntax directive.
const (
	IntelPrefix   PrefixAttribute = iota // syntax with prefixes (.intel_syntax prefix)
	IntelNoPrefix                        // syntax without prefixes (.intel_syntax noprefix)
)

// Section attributes for the .section directive.
const (
	SectionNone             SectionAttribute = iota // no special attributes for the section
	SectionAllocatable                              // section is allocatable in memory (flag "a")
	SectionWritable                                 // section is writable at runtime (flag "w")
	SectionExecutable                               // section contains executable code (flag "x")
	SectionMergeable                                // section can be merged with other sections (flag "M")
	SectionStrings                                  // section contains zero-terminated strings (flag "S")
	SectionMergeableStrings                         // section contains mergeable zero-terminated strings (flag "MS")
	SectionProgramBits                              // section contains data or instruction program bits (section kind "@progbits")
	SectionNoBits                                   // section does not occupy space in the file (section kind "@nobits")
)

// File attributes for the .file directive.
const (
	FileId        FileAttribute = iota // file identifier
	FileName                           // source code file name
	FileDelimiter                      // delimiter for file attributes
)

// Location attributes for the .loc directive.
const (
	LocationFileId                     LocationAttribute = iota // source file identifier (file index)
	LocationLine                                                // source line number
	LocationColumn                                              // source column number
	LocationBasicBlock                                          // basic block identifier
	LocationPrologueEnd                                         // end of function prologue
	LocationEpilogueBegin                                       // beginning of function epilogue
	LocationIsStatement                                         // indicates a statement location
	LocationInstructionSetArchitecture                          // instruction set architecture (e.g., "x86_64")
	LocationDiscriminator                                       // discriminator for distinguishing multiple locations
	LocationDelimiter                                           // delimiter for location attributes
)

// Symbol type attributes for the .type directive.
const (
	TypeFunction TypeAttribute = iota // symbol is a function (@function)
	TypeObject                        // symbol is a data object (@object)
	TypeCommon                        // symbol is a common data object (@common)
	TypeTls                           // symbol is a thread-local storage object (@tls_object)
	TypeIfunc                         // symbol is an indirect function (@gnu_indirect_function)
)

// Size calculation attributes for the .size directive.
const (
	SizeLabel      SizeAttribute = iota // calculate size from current location minus label (.-label)
	SizeAbsolute                        // use absolute size value
	SizeExpression                      // use arbitrary expression for size calculation
)

// Call frame information attributes for the .cfi directives.
const (
	CallFrameInformationOffset   CallFrameInformationAttribute = iota // sets the CFA to be at RSP + offset after a "push rbp" instruction
	CallFrameInformationRegister                                      // canonical frame address is now based on register (e.g., RSP or RBP)
)

// Power-of-2 alignment values for the .p2align directive.
const (
	P2align1  = 0 // 1-byte alignment (no alignment)
	P2align2  = 1 // 2-byte alignment
	P2align4  = 2 // 4-byte alignment
	P2align8  = 3 // 8-byte alignment
	P2align16 = 4 // 16-byte alignment
	P2align32 = 5 // 32-byte alignment
)

// Kind of read-only data to be stored in a read-only section.
const (
	ReadOnlyUtf32   ReadOnlyDataKind = iota // UTF-32 encoded strings
	ReadOnlyInt64                           // 64-bit integer literals (signed and unsigned)
	ReadOnlyStrDesc                         // string descriptor for UTF encoded strings (literal data label 64-bit pointer, 64-bit length)
)

// DWARF codes are used to identify debugging information entries and are not formally part of the DWARF specification.
const (
	DW_CODE_termination      DwarfCode = iota // termination code (0 for ending a DWARF section)
	DW_CODE_compilation_unit                  // compilation unit (e.g., a source file name)
	DW_CODE_base_type                         // base type (e.g., int8_t, float)
	DW_CODE_subprogram                        // subprogram (e.g., function)
	DW_CODE_variable                          // variable (e.g., local variable in a function)
)

// DWARF tags define the structure of debugging information entries.
const (
	// type constructors
	DW_TAG_array_type       DwarfTag = 0x01 // array type
	DW_TAG_class_type       DwarfTag = 0x02 // class (C++) type
	DW_TAG_entry_point      DwarfTag = 0x03 // entry point of program
	DW_TAG_enumeration_type DwarfTag = 0x04 // enumeration type
	DW_TAG_subroutine_type  DwarfTag = 0x15 // subroutine type (function type)
	DW_TAG_base_type        DwarfTag = 0x24 // base type (e.g. int, float)
	DW_TAG_pointer_type     DwarfTag = 0x0f // pointer type
	DW_TAG_reference_type   DwarfTag = 0x10 // reference type (& in C++)
	DW_TAG_const_type       DwarfTag = 0x26 // const-qualified type
	DW_TAG_volatile_type    DwarfTag = 0x35 // volatile-qualified type
	DW_TAG_restrict_type    DwarfTag = 0x37 // restrict-qualified type
	DW_TAG_typedef          DwarfTag = 0x16 // typedef
	DW_TAG_subrange_type    DwarfTag = 0x21 // subrange (array bounds)
	DW_TAG_string_type      DwarfTag = 0x12 // string type
	DW_TAG_structure_type   DwarfTag = 0x13 // struct type
	DW_TAG_union_type       DwarfTag = 0x17 // union type

	// program structure
	DW_TAG_compile_unit       DwarfTag = 0x11 // compile unit (source file)
	DW_TAG_namespace          DwarfTag = 0x39 // C++ namespace
	DW_TAG_module             DwarfTag = 0x1e // module or file-scope grouping
	DW_TAG_imported_unit      DwarfTag = 0x3d // imported (split) unit
	DW_TAG_lexical_block      DwarfTag = 0x0b // lexical block ({} scope)
	DW_TAG_try_block          DwarfTag = 0x32 // try/catch block
	DW_TAG_catch_block        DwarfTag = 0x25 // catch block
	DW_TAG_inlined_subroutine DwarfTag = 0x1d // inlined function instance

	// functions and parameters
	DW_TAG_subprogram       DwarfTag = 0x2e // function or procedure
	DW_TAG_formal_parameter DwarfTag = 0x05 // function parameter

	// data objects
	DW_TAG_variable DwarfTag = 0x34 // variable (local, global)
	DW_TAG_constant DwarfTag = 0x27 // constant value
	DW_TAG_label    DwarfTag = 0x0a // code label

	// class/struct members & enumeration values
	DW_TAG_member     DwarfTag = 0x0d // member of struct/class/union
	DW_TAG_enumerator DwarfTag = 0x28 // enumerator (value of enum)

	// miscellaneous
	DW_TAG_imported_declaration DwarfTag = 0x08   // imported declaration
	DW_TAG_common_block         DwarfTag = 0x1a   // common block (Fortran)
	DW_TAG_common_inclusion     DwarfTag = 0x1b   // common inclusion
	DW_TAG_sibling              DwarfTag = 0x04   // see DW_TAG_enumeration_type (used in certain schemas)
	DW_TAG_lo_user              DwarfTag = 0x4080 // start of user-defined tag range
	DW_TAG_hi_user              DwarfTag = 0xffff // end of user-defined tag range
)

// DWARF attributes define the properties of debugging information entries.
const (
	// compile unit–level attributes
	DW_AT_name         DwarfAttribute = 0x03 // DW_AT_name: source file or entity name
	DW_AT_comp_dir     DwarfAttribute = 0x1b // DW_AT_comp_dir: compilation directory
	DW_AT_language     DwarfAttribute = 0x13 // DW_AT_language: source language (DW_LANG_…)
	DW_AT_stmt_list    DwarfAttribute = 0x10 // DW_AT_stmt_list: offset into .debug_line
	DW_AT_producer     DwarfAttribute = 0x3e // DW_AT_producer: producing compiler/toolchain
	DW_AT_split_dwarf  DwarfAttribute = 0x3f // DW_AT_split_dwarf: indicates Split-DWARF support
	DW_AT_GNU_dwo_name DwarfAttribute = 0x3d // DW_AT_GNU_dwo_name: .dwo file name for Split-DWARF
	DW_AT_optimized    DwarfAttribute = 0x3a // DW_AT_optimized: whether optimizations were applied
	DW_AT_ranges       DwarfAttribute = 0x55 // DW_AT_ranges: .debug_ranges offset
	DW_AT_GNU_pubnames DwarfAttribute = 0x3b // DW_AT_GNU_pubnames: offset into .debug_pubnames

	// subprogram (function/procedure) attributes
	DW_AT_linkage_name       DwarfAttribute = 0x1d // DW_AT_linkage_name: linker/mangled name
	DW_AT_low_pc             DwarfAttribute = 0x11 // DW_AT_low_pc: start address
	DW_AT_high_pc            DwarfAttribute = 0x12 // DW_AT_high_pc: end address or size
	DW_AT_frame_base         DwarfAttribute = 0x13 // DW_AT_frame_base: base for location expressions
	DW_AT_prototyped         DwarfAttribute = 0x3f // DW_AT_prototyped: has function prototype
	DW_AT_calling_convention DwarfAttribute = 0x52 // DW_AT_calling_convention: calling convention
	DW_AT_inline             DwarfAttribute = 0x20 // DW_AT_inline: inlining status
	DW_AT_artificial         DwarfAttribute = 0x34 // DW_AT_artificial: compiler-generated entity
	DW_AT_accessibility      DwarfAttribute = 0x32 // DW_AT_accessibility: C++ access (public/prot/priv)
	DW_AT_decl_file          DwarfAttribute = 0x3c // DW_AT_decl_file: file index in .debug_line file table
	DW_AT_decl_line          DwarfAttribute = 0x3d // DW_AT_decl_line: line number of declaration
	DW_AT_decl_column        DwarfAttribute = 0x3e // DW_AT_decl_column: column number of declaration
	DW_AT_description        DwarfAttribute = 0x08 // DW_AT_description: free-text description

	// variable / parameter attributes
	DW_AT_type           DwarfAttribute = 0x49 // DW_AT_type: reference to type DIE
	DW_AT_location       DwarfAttribute = 0x2e // DW_AT_location: location expression
	DW_AT_declaration    DwarfAttribute = 0x3b // DW_AT_declaration: only declared vs. defined
	DW_AT_visibility     DwarfAttribute = 0x3f // DW_AT_visibility: default/protected/private
	DW_AT_start_scope    DwarfAttribute = 0x62 // DW_AT_start_scope: starting PC offset
	DW_AT_constant_value DwarfAttribute = 0x3c // DW_AT_constant_value: constant literal value
	DW_AT_external       DwarfAttribute = 0x3e // DW_AT_external: external linkage

	// type attributes (base, struct, array, pointer, etc.)
	DW_AT_byte_size            DwarfAttribute = 0x0b // DW_AT_byte_size: size in bytes
	DW_AT_encoding             DwarfAttribute = 0x3e // DW_AT_encoding: base type encoding
	DW_AT_bit_size             DwarfAttribute = 0x0c // DW_AT_bit_size: size of bitfield in bits
	DW_AT_bit_offset           DwarfAttribute = 0x0d // DW_AT_bit_offset: bit offset within containing type
	DW_AT_upper_bound          DwarfAttribute = 0x2f // DW_AT_upper_bound: array upper bound
	DW_AT_lower_bound          DwarfAttribute = 0x30 // DW_AT_lower_bound: array lower bound
	DW_AT_abstract_origin      DwarfAttribute = 0x31 // DW_AT_abstract_origin: refer to abstract DIE
	DW_AT_specification        DwarfAttribute = 0x32 // DW_AT_specification: refer to declaration DIE
	DW_AT_sibling              DwarfAttribute = 0x33 // DW_AT_sibling: offset to next sibling DIE
	DW_AT_containing_type      DwarfAttribute = 0x34 // DW_AT_containing_type: parent struct/class DIE
	DW_AT_data_member_location DwarfAttribute = 0x38 // DW_AT_data_member_location: struct member offset

	// miscellaneous / vendor-defined attributes
	DW_AT_MACRO_INFO         DwarfAttribute = 0x2c // DW_AT_MACRO_INFO: offset into .debug_macro
	DW_AT_CALL_LINE          DwarfAttribute = 0x56 // DW_AT_CALL_LINE: call-site line number
	DW_AT_CALL_FILE          DwarfAttribute = 0x57 // DW_AT_CALL_FILE: call-site file index
	DW_AT_CALL_COLUMN        DwarfAttribute = 0x58 // DW_AT_CALL_COLUMN: call-site column number
	DW_AT_object_pointer     DwarfAttribute = 0x50 // DW_AT_object_pointer: "this" for methods
	DW_AT_allocated          DwarfAttribute = 0x21 // DW_AT_allocated: heap allocation info
	DW_AT_associated         DwarfAttribute = 0x22 // DW_AT_associated: GC/heap association
	DW_AT_GNU_old_call_frame DwarfAttribute = 0x40 // DW_AT_GNU_old_call_frame: legacy CFI format
)

// DWARF form codes specify the on-disk encoding of a DWARF attribute’s value.
const (
	// fixed-size data forms
	DW_FORM_data1  DwarfForm = 0x0b // 1-byte unsigned constant
	DW_FORM_data2  DwarfForm = 0x05 // 2-byte unsigned constant (little-endian)
	DW_FORM_data4  DwarfForm = 0x06 // 4-byte unsigned constant
	DW_FORM_data8  DwarfForm = 0x07 // 8-byte unsigned constant
	DW_FORM_data16 DwarfForm = 0x1e // 16-byte constant (for 128-bit data)

	// address and offset forms
	DW_FORM_addr       DwarfForm = 0x01 // address (size = target address size)
	DW_FORM_sec_offset DwarfForm = 0x17 // 4-byte section offset (e.g. into .debug_line)

	// block (binary blob) forms
	DW_FORM_block1 DwarfForm = 0x0a // block: length is 1-byte constant
	DW_FORM_block2 DwarfForm = 0x03 // block: length is 2-byte constant
	DW_FORM_block4 DwarfForm = 0x04 // block: length is 4-byte constant
	DW_FORM_block  DwarfForm = 0x09 // block: ULEB128 length prefix

	// literal and flag forms
	DW_FORM_flag         DwarfForm = 0x0c // 1-byte flag (0=false, non-zero=true)
	DW_FORM_flag_present DwarfForm = 0x19 // no value bytes; presence alone = true
	DW_FORM_sdata        DwarfForm = 0x0d // signed LEB128 constant
	DW_FORM_udata        DwarfForm = 0x0f // unsigned LEB128 constant

	// string forms
	DW_FORM_string DwarfForm = 0x08 // null-terminated string inline in .debug_info
	DW_FORM_strp   DwarfForm = 0x0e // 4-byte offset into .debug_str

	// DWARF5 string‐offset table forms
	DW_FORM_strx  DwarfForm = 0x1a // ULEB128 index into .debug_str_offsets
	DW_FORM_strx1 DwarfForm = 0x25 // 1-byte  index into .debug_str_offsets
	DW_FORM_strx2 DwarfForm = 0x26 // 2-byte  index into .debug_str_offsets
	DW_FORM_strx3 DwarfForm = 0x27 // 3-byte  index into .debug_str_offsets
	DW_FORM_strx4 DwarfForm = 0x28 // 4-byte  index into .debug_str_offsets

	// reference forms (point to another DIE within the same CU)
	DW_FORM_ref1      DwarfForm = 0x11 // 1-byte  offset to a DIE
	DW_FORM_ref2      DwarfForm = 0x12 // 2-byte  offset to a DIE
	DW_FORM_ref4      DwarfForm = 0x13 // 4-byte  offset to a DIE
	DW_FORM_ref8      DwarfForm = 0x14 // 8-byte  offset to a DIE
	DW_FORM_ref_udata DwarfForm = 0x15 // ULEB128 offset to a DIE
	DW_FORM_ref_addr  DwarfForm = 0x10 // address-sized offset to a DIE (à la ref4/ref8)

	// expression and indirect forms
	DW_FORM_exprloc  DwarfForm = 0x18 // ULEB128 length + DW_OP-stream (location expression)
	DW_FORM_indirect DwarfForm = 0x16 // ULEB128 form code follows, then value in that form

	// implicit constant form
	DW_FORM_implicit_const DwarfForm = 0x20 // ULEB128 constant encoded indirectly

	// address-indexed form (DWARF5):
	DW_FORM_addrx DwarfForm = 0x1b // ULEB128 index into .debug_addr

	// line-string form (DWARF5):
	DW_FORM_line_strp DwarfForm = 0x1f // 4-byte offset into .debug_line_str (introduced in DWARF5)
)

type (
	// Represents debugger flags (bit-mask).
	Debugger uint64

	// Represents an assembler directive (pseudo-op).
	DirectiveKind int

	// Represents a prefix attribute for the .intel_syntax directive.
	PrefixAttribute int

	// Represents an attribute of the .section directive.
	SectionAttribute int

	// Represents a file attribute for the .file directive.
	FileAttribute int

	// Represents a location attribute for the .loc directive.
	LocationAttribute int

	// Represents a symbol type for the .type directive.
	TypeAttribute int

	// Represents a size calculation method for the .size directive.
	SizeAttribute int

	// Represents a call frame information (CFI) attribute for the .cfi directives.
	CallFrameInformationAttribute int

	// Kind of read-only static data.
	ReadOnlyDataKind int

	// Represents a DWARF abbreviation entry code (ULEB128-encoded).
	DwarfCode uint64

	// Represents a DWARF tag (ULEB128-encoded).
	DwarfTag uint32

	// Represents a DWARF attribute (ULEB128-encoded).
	DwarfAttribute uint64

	// Represents a DWARF form (ULEB128-encoded).
	DwarfForm uint32

	// ElfSection represents a generic ELF section with typed line-contents.
	ElfSection[T fmt.Stringer] struct {
		Directives []DirectiveKind    `json:"directives"` // directives for building the section (e.g., .section, .p2align)
		Attributes []SectionAttribute `json:"attributes"` // attributes of the section (e.g., allocatable, writable, executable)
		Alignment  int                `json:"alignment"`  // power-of-2 alignment for section contents (used with ".p2align")
		Offsets    bool               `json:"offsets"`    // whether the section requires labels for offset calculations (e.g., .debug_str)
		Content    []T                `json:"content"`    // typed contents of this section (e.g., read-only data items, instructions)
	}

	// A read-only data item holds one or several constant values that are not modified during program execution.
	ReadOnlyDataItem struct {
		Kind   ReadOnlyDataKind `json:"kind"`   // kind of the read-only data item
		Labels []string         `json:"labels"` // literal data labels to access the read-only data item
		Values any              `json:"values"` // the values will be stored in a read-only section and encoded based on its kind
	}

	// A DWARF abbreviation entry defines a structure for debugging information entries (DIEs) used in the .debug_abbrev section.
	AbbreviationEntry struct {
		Code        DwarfCode                // abbreviation code used to reference this DIE (ULEB128)
		Tag         DwarfTag                 // DW_TAG_* code for the entry used to identify the type of DIE (ULEB128)
		HasChildren bool                     // 1 byte indicating if this DIE can have children
		Attributes  []*AbbreviationAttribute // list of <attribute,form> children, implicitly ends before 0
	}

	// A DWARF abbreviation attribute defines a single child attribute of a debugging information entry (DIE).
	AbbreviationAttribute struct {
		Attribute DwarfAttribute // DW_AT_* code defines what this attribute represents (ULEB128)
		Form      DwarfForm      // DW_FORM_* code specifies how the attribute value is encoded (ULEB128)
	}

	// A DWARF string item represents a string literal stored in the .debug_str section.
	StringItem struct {
		Label     string        `json:"label"`     // literal data label to access the string item
		Directive DirectiveKind `json:"directive"` // directive to use for the string item (e.g., .string, .asciz)
		Operand   string        `json:"operand"`   // the string value to be stored in the item
	}

	// Directive represents a structured assembler directive with formal syntax.
	Directive struct {
		Directive DirectiveKind `json:"kind"`      // the directive kind (e.g., .type, .size, .global)
		Symbols   []string      `json:"symbols"`   // the symbol names this directive applies to
		Arguments []string      `json:"arguments"` // directive-specific arguments (e.g., "@function", ".-symbol")
		Comments  []string      `json:"comments"`  // comments associated with this directive
	}
)

// Create a new ELF section with the specified directives, attributes, alignment, and offset calculation support.
func NewSection[T fmt.Stringer](directives []DirectiveKind, attributes []SectionAttribute, alignment int, offsets bool) *ElfSection[T] {
	return &ElfSection[T]{Directives: directives, Attributes: attributes, Alignment: alignment, Offsets: offsets, Content: make([]T, 0)}
}

// Create a new read-only data item with literal data labels for a read-only section.
func NewReadOnlyDataItem(kind ReadOnlyDataKind, labels []string, values any) *ReadOnlyDataItem {
	return &ReadOnlyDataItem{Kind: kind, Labels: labels, Values: values}
}

// Create a new abbreviation entry for the .debug_abbrev section.
func NewAbbreviationEntry(code DwarfCode, tag DwarfTag, hasChildren bool, attributes []*AbbreviationAttribute) *AbbreviationEntry {
	return &AbbreviationEntry{Code: code, Tag: tag, HasChildren: hasChildren, Attributes: attributes}
}

// Create a new abbreviation attribute for a debugging information entry (DIE).
func NewAbbreviationAttribute(attribute DwarfAttribute, form DwarfForm) *AbbreviationAttribute {
	return &AbbreviationAttribute{Attribute: attribute, Form: form}
}

// Create a new string item for the .debug_str section.
func NewStringItem(label string, directive DirectiveKind, operand string) *StringItem {
	return &StringItem{Label: label, Directive: directive, Operand: operand}
}

// Create a new directive for the assembler.
func NewDirective(directive DirectiveKind, symbols []string, args ...string) *Directive {
	return &Directive{Directive: directive, Symbols: symbols, Arguments: args, Comments: make([]string, 0)}
}

// Create a .intel_syntax directive for Intel assembly syntax.
func NewIntel(prefix PrefixAttribute) *Directive {
	return NewDirective(IntelSyntax, nil, prefix.String())
}

// Create a .att_syntax directive for AT&T assembly syntax.
func NewAtt() *Directive {
	return NewDirective(AttSyntax, nil)
}

// Create a .global directive for global symbols.
func NewGlobal(symbols []string) *Directive {
	return NewDirective(Global, symbols)
}

// Create a .extern directive for external symbols.
func NewExtern(symbols []string) *Directive {
	return NewDirective(Extern, symbols)
}

// Create a .file directive with a file identifier and name.
func NewFile(id int, name string) *Directive {
	return newFile(id, name)
}

// Create a .loc directive for source locations in debug info.
func NewLocation(id, line, column int, debugger Debugger, attributes ...string) *Directive {
	return newLocation(id, line, column, debugger, attributes...)
}

// Create a .type directive for a function symbol.
func NewTypeFunction(symbol string) *Directive {
	return NewDirective(Type, []string{symbol}, TypeFunction.String())
}

// Create a .type directive for an object symbol.
func NewTypeObject(symbol string) *Directive {
	return NewDirective(Type, []string{symbol}, TypeObject.String())
}

// Create a .size directive using the standard ".-symbol" calculation.
func NewSizeLabel(symbol string) *Directive {
	return NewDirective(Size, []string{symbol}, fmt.Sprintf(SizeLabel.String(), symbol))
}

// Create a .size directive with an absolute size value.
func NewSizeAbsolute(symbol string, size int) *Directive {
	return NewDirective(Size, []string{symbol}, fmt.Sprintf(SizeAbsolute.String(), size))
}

// Create a .cfi_startproc directive to begin a CFI (call frame information).
func NewCfiStartProcedure() *Directive {
	return NewDirective(CfiStartProc, nil)
}

// Create a .cfi_endproc directive to end a CFI (call frame information).
func NewCfiEndProcedure() *Directive {
	return NewDirective(CfiEndProc, nil)
}

// Create a .cfi_def_cfa_offset directive to define the CFA offset (canonical frame address offset).
func NewCfiDefCfaOffset(offset int) *Directive {
	return NewDirective(CfiDefCfaOffset, nil, fmt.Sprintf(CallFrameInformationOffset.String(), offset))
}

// Create a .cfi_offset directive to define a register offset in CFI (call frame information).
func NewCfiOffset(register string, offset int) *Directive {
	return NewDirective(CfiOffset, nil, register, fmt.Sprintf(CallFrameInformationOffset.String(), offset))
}

// Create a .cfi_def_cfa_register directive to change the CFA register (canonical frame address register).
func NewCfiDefCfaRegister(register string) *Directive {
	return NewDirective(CfiDefCfaRegister, nil, register)
}

// String representation of a directive.
func (dk DirectiveKind) String() string {
	return directiveNames[dk]
}

// String representation of a prefix attribute.
func (pa PrefixAttribute) String() string {
	return prefixAttributeNames[pa]
}

// String representation of a section attribute.
func (sa SectionAttribute) String() string {
	return sectionAttributeNames[sa]
}

// String representation of a file attribute.
func (fa FileAttribute) String() string {
	return fileAttributeNames[fa]
}

// String representation of a location attribute.
func (la LocationAttribute) String() string {
	return locationAttributeNames[la]
}

// String representation of a type attribute.
func (ta TypeAttribute) String() string {
	return typeAttributeNames[ta]
}

// String representation of a size attribute.
func (sa SizeAttribute) String() string {
	return sizeAttributeNames[sa]
}

// String representation of a call frame information attribute.
func (cfi CallFrameInformationAttribute) String() string {
	return callFrameInformationAttributeNames[cfi]
}

// Append a comment to a directive that will be emitted before the directive.
func (d *Directive) AppendComment(comment string) {
	if comment = strings.TrimSpace(comment); comment != "" {
		d.Comments = append(d.Comments, comment)
	}
}

// String representation of a descriptor label.
func ToDescriptor(label string) string {
	return fmt.Sprintf(descriptorLabel, label)
}
