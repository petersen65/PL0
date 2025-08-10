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

// DWARF version number used for debugging information in ELF files.
const DwarfVersion = 5

// Standard DWARF label names for ELF sections.
const (
	CompilationUnitLabel      = "compilation_unit"      // label for the compilation unit in .debug_str section
	CompilationDirectoryLabel = "compilation_directory" // label for the compilation directory in .debug_str section
	ProducerLabel             = "producer"              // label for the producer in .debug_str section
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
	DebugLine   // switches to the .debug_line (debug line numbers) section (.debug_line,"",@progbits)

	// alignment and layout directives
	P2align // aligns to 2^n bytes (.p2align <n>) — used for ABI alignment
	Align   // aligns to n bytes (.align <n>) — less portable across platforms
	Balign  // aligns using byte-boundary logic (.balign <n>) — same as .align but stricter in ELF

	// value emission directives
	Byte    // emits a single byte value (.byte 0xFF)
	Short   // emits a 16-bit word (.short 0xABCD)
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
	_                        DwarfCode = iota
	DW_CODE_compilation_unit           // compilation unit (e.g., a source file name)
	DW_CODE_base_type                  // base type (e.g., int8_t, float)
	DW_CODE_subprogram                 // subprogram (e.g., function)
	DW_CODE_variable                   // variable (e.g., local variable in a function)

	// DWARF codes with special meaning
	DW_CODE_suppression DwarfCode = -1 // suppression code (code will not be emitted)
	DW_CODE_termination DwarfCode = 0  // termination code (0 for ending a DWARF section)
)

// DwarfTag represents DWARF v5 tag values for debugging information entries.
const (
	// base/type tags
	DW_TAG_array_type       DwarfTag = 0x01 // array type
	DW_TAG_class_type       DwarfTag = 0x02 // class (C++), supports methods/member access
	DW_TAG_enumeration_type DwarfTag = 0x04 // enumeration type
	DW_TAG_pointer_type     DwarfTag = 0x0f // pointer type (T*)
	DW_TAG_reference_type   DwarfTag = 0x10 // reference type (T&)
	DW_TAG_compile_unit     DwarfTag = 0x11 // compilation unit root
	DW_TAG_string_type      DwarfTag = 0x12 // string type
	DW_TAG_structure_type   DwarfTag = 0x13 // struct type
	DW_TAG_subroutine_type  DwarfTag = 0x15 // function type (signature)
	DW_TAG_typedef          DwarfTag = 0x16 // typedef or alias
	DW_TAG_union_type       DwarfTag = 0x17 // union type

	// aggregate/derived types
	DW_TAG_subrange_type DwarfTag = 0x21 // subrange (array bounds)
	DW_TAG_base_type     DwarfTag = 0x24 // built-in type (int, float)
	DW_TAG_const_type    DwarfTag = 0x26 // const-qualified type
	DW_TAG_volatile_type DwarfTag = 0x35 // volatile-qualified type
	DW_TAG_restrict_type DwarfTag = 0x37 // restrict-qualified type (C99)
	DW_TAG_shared_type   DwarfTag = 0x3e // shared type across compilation units

	// data/value descriptors
	DW_TAG_enumerator       DwarfTag = 0x28 // enumerator (enum value)
	DW_TAG_constant         DwarfTag = 0x27 // constant value
	DW_TAG_variable         DwarfTag = 0x34 // variable (global or local)
	DW_TAG_member           DwarfTag = 0x0d // member of struct/class/union
	DW_TAG_formal_parameter DwarfTag = 0x05 // function/method parameter

	// function/procedure entries
	DW_TAG_subprogram         DwarfTag = 0x2e // function definition or declaration
	DW_TAG_inlined_subroutine DwarfTag = 0x1d // inlined subroutine instance

	// scope and control flow
	DW_TAG_lexical_block DwarfTag = 0x0b // anonymous block ({} scope)
	DW_TAG_try_block     DwarfTag = 0x32 // try-catch block
	DW_TAG_catch_block   DwarfTag = 0x25 // catch block for exception handling

	// modules, namespaces, imports
	DW_TAG_namespace            DwarfTag = 0x39 // C++ namespace
	DW_TAG_module               DwarfTag = 0x1e // module or file grouping
	DW_TAG_imported_unit        DwarfTag = 0x3d // split or imported compilation unit
	DW_TAG_imported_declaration DwarfTag = 0x08 // imported declaration (using/import)
	DW_TAG_common_block         DwarfTag = 0x1a // fortran COMMON block
	DW_TAG_common_inclusion     DwarfTag = 0x1b // fortran COMMON inclusion

	// others
	DW_TAG_entry_point      DwarfTag = 0x03 // program’s entry point (legacy)
	DW_TAG_unspecified_type DwarfTag = 0x3f // unspecified type ('void' or unknown)

	// user-defined tag range
	DW_TAG_lo_user DwarfTag = 0x4080 // start of user-defined tags
	DW_TAG_hi_user DwarfTag = 0xffff // end of user-defined tags
)

// DwarfAttribute represents DWARF v5 attribute values for debugging information entries.
const (
	DW_AT_sibling              DwarfAttribute = 0x01 // reference to sibling DIE
	DW_AT_location             DwarfAttribute = 0x02 // location of object
	DW_AT_name                 DwarfAttribute = 0x03 // name of entity
	DW_AT_ordering             DwarfAttribute = 0x09 // array element ordering (row/column-major)
	DW_AT_byte_size            DwarfAttribute = 0x0b // size in bytes
	DW_AT_bit_offset           DwarfAttribute = 0x0c // bit offset from container
	DW_AT_bit_size             DwarfAttribute = 0x0d // bit size of type or field
	DW_AT_stmt_list            DwarfAttribute = 0x10 // offset into .debug_line
	DW_AT_low_pc               DwarfAttribute = 0x11 // start address of code range
	DW_AT_high_pc              DwarfAttribute = 0x12 // end address or size from low_pc
	DW_AT_language             DwarfAttribute = 0x13 // source language (DW_LANG_*)
	DW_AT_discr                DwarfAttribute = 0x15 // discriminant reference
	DW_AT_discr_value          DwarfAttribute = 0x16 // discriminant value
	DW_AT_visibility           DwarfAttribute = 0x17 // access level (public/private/etc.)
	DW_AT_import               DwarfAttribute = 0x18 // reference to imported entity
	DW_AT_string_length        DwarfAttribute = 0x19 // location of string length
	DW_AT_common_reference     DwarfAttribute = 0x1a // reference to common block
	DW_AT_comp_dir             DwarfAttribute = 0x1b // compilation directory
	DW_AT_const_value          DwarfAttribute = 0x1c // constant value
	DW_AT_containing_type      DwarfAttribute = 0x1d // reference to enclosing type
	DW_AT_default_value        DwarfAttribute = 0x1e // default template or parameter value
	DW_AT_inline               DwarfAttribute = 0x20 // inlining info
	DW_AT_is_optional          DwarfAttribute = 0x21 // optional parameter
	DW_AT_lower_bound          DwarfAttribute = 0x22 // array lower bound
	DW_AT_producer             DwarfAttribute = 0x25 // compiler identification string
	DW_AT_prototyped           DwarfAttribute = 0x27 // function has prototype
	DW_AT_return_addr          DwarfAttribute = 0x2a // return address location
	DW_AT_start_scope          DwarfAttribute = 0x2c // lifetime of variable
	DW_AT_stride               DwarfAttribute = 0x2e // array stride
	DW_AT_upper_bound          DwarfAttribute = 0x2f // array upper bound
	DW_AT_abstract_origin      DwarfAttribute = 0x31 // reference to canonical definition
	DW_AT_accessibility        DwarfAttribute = 0x32 // class member access level
	DW_AT_address_class        DwarfAttribute = 0x33 // memory segment / address space
	DW_AT_artificial           DwarfAttribute = 0x34 // compiler-generated flag
	DW_AT_base_types           DwarfAttribute = 0x35 // base type list (Fortran-style)
	DW_AT_calling_convention   DwarfAttribute = 0x36 // calling convention
	DW_AT_count                DwarfAttribute = 0x37 // number of elements
	DW_AT_data_member_location DwarfAttribute = 0x38 // member offset (struct/class)
	DW_AT_decl_column          DwarfAttribute = 0x39 // declaration source column
	DW_AT_decl_file            DwarfAttribute = 0x3a // declaration file index
	DW_AT_decl_line            DwarfAttribute = 0x3b // declaration line number
	DW_AT_declaration          DwarfAttribute = 0x3c // declares but does not define
	DW_AT_discr_list           DwarfAttribute = 0x3d // discriminant case ranges
	DW_AT_encoding             DwarfAttribute = 0x3e // type encoding (e.g. signed, float)
	DW_AT_external             DwarfAttribute = 0x3f // symbol is externally visible
	DW_AT_frame_base           DwarfAttribute = 0x40 // location of frame base (CFA)
	DW_AT_friend               DwarfAttribute = 0x41 // reference to friend declaration
	DW_AT_identifier_case      DwarfAttribute = 0x42 // identifier case sensitivity
	DW_AT_macro_info           DwarfAttribute = 0x43 // offset into .debug_macinfo
	DW_AT_namelist_item        DwarfAttribute = 0x44 // fortran namelist item
	DW_AT_priority             DwarfAttribute = 0x45 // task scheduling priority
	DW_AT_segment              DwarfAttribute = 0x46 // segment selector
	DW_AT_specification        DwarfAttribute = 0x47 // reference to a specification DIE
	DW_AT_static_link          DwarfAttribute = 0x48 // static link (closure environment)
	DW_AT_type                 DwarfAttribute = 0x49 // reference to type DIE
	DW_AT_use_location         DwarfAttribute = 0x4a // reference to usage location
	DW_AT_variable_parameter   DwarfAttribute = 0x4b // fortran: parameter passed by ref
	DW_AT_virtuality           DwarfAttribute = 0x4c // virtual/pure virtual member
	DW_AT_vtable_elem_location DwarfAttribute = 0x4d // offset in vtable

	// DWARF v5-specific
	DW_AT_allocated       DwarfAttribute = 0x4f // fortran: storage allocated indicator
	DW_AT_associated      DwarfAttribute = 0x50 // fortran: pointer association status
	DW_AT_data_location   DwarfAttribute = 0x51 // memory location
	DW_AT_byte_stride     DwarfAttribute = 0x52 // distance between array elements
	DW_AT_entry_pc        DwarfAttribute = 0x53 // entry address
	DW_AT_use_UTF8        DwarfAttribute = 0x54 // strings are UTF-8
	DW_AT_extension       DwarfAttribute = 0x55 // reference to DIE for extension
	DW_AT_ranges          DwarfAttribute = 0x56 // offset into .debug_ranges
	DW_AT_trampoline      DwarfAttribute = 0x57 // trampoline function reference
	DW_AT_call_column     DwarfAttribute = 0x58 // call site column
	DW_AT_call_file       DwarfAttribute = 0x59 // call site file
	DW_AT_call_line       DwarfAttribute = 0x5a // call site line
	DW_AT_description     DwarfAttribute = 0x5b // description of type or object
	DW_AT_binary_scale    DwarfAttribute = 0x5c // scale for fixed-point type
	DW_AT_decimal_scale   DwarfAttribute = 0x5d // decimal scale
	DW_AT_small           DwarfAttribute = 0x5e // indicates use of small register class
	DW_AT_decimal_sign    DwarfAttribute = 0x5f // decimal sign style (leading, trailing, overpunch)
	DW_AT_digit_count     DwarfAttribute = 0x60 // number of decimal digits
	DW_AT_picture_string  DwarfAttribute = 0x61 // picture formatting string
	DW_AT_mutable         DwarfAttribute = 0x62 // c++ mutable keyword
	DW_AT_threads_scaled  DwarfAttribute = 0x63 // indicates thread-scaling applied
	DW_AT_explicit        DwarfAttribute = 0x64 // explicit instantiation
	DW_AT_object_pointer  DwarfAttribute = 0x65 // `this` pointer or closure self
	DW_AT_endianity       DwarfAttribute = 0x66 // endianess of the data
	DW_AT_elemental       DwarfAttribute = 0x67 // fortran elemental procedure
	DW_AT_pure            DwarfAttribute = 0x68 // fortran pure procedure
	DW_AT_recursive       DwarfAttribute = 0x69 // fortran recursive procedure
	DW_AT_signature       DwarfAttribute = 0x6a // signature reference (for type units)
	DW_AT_main_subprogram DwarfAttribute = 0x6b // entry point of main subprogram
	DW_AT_data_bit_offset DwarfAttribute = 0x6c // bit offset from storage unit
	DW_AT_const_expr      DwarfAttribute = 0x6d // value is constant expression
	DW_AT_enum_class      DwarfAttribute = 0x6e // strongly typed enum (C++11)
	DW_AT_linkage_name    DwarfAttribute = 0x6f // linker-visible (mangled) name

	// split DWARF / debug fission
	DW_AT_string_length_bit_size  DwarfAttribute = 0x70 // bit size of string length field
	DW_AT_string_length_byte_size DwarfAttribute = 0x71 // byte size of string length field
	DW_AT_rank                    DwarfAttribute = 0x72 // rank expression for arrays
	DW_AT_str_offsets_base        DwarfAttribute = 0x73 // offset into .debug_str_offsets
	DW_AT_addr_base               DwarfAttribute = 0x74 // offset into .debug_addr
	DW_AT_rnglists_base           DwarfAttribute = 0x75 // offset into .debug_rnglists
	DW_AT_dwo_name                DwarfAttribute = 0x76 // dWO filename
	DW_AT_reference               DwarfAttribute = 0x77 // split unit reference
	DW_AT_dwo_id                  DwarfAttribute = 0x78 // unique ID for DWO
	DW_AT_dwo_file                DwarfAttribute = 0x79 // file index in DWO
	DW_AT_dwo_line                DwarfAttribute = 0x7a // offset into .debug_line in DWO
	DW_AT_stmt_list_base          DwarfAttribute = 0x7b // offset to stmt list in DWO
	DW_AT_loclists_base           DwarfAttribute = 0x7c // offset into .debug_loclists

	// reserved for user extensions
	DW_AT_lo_user DwarfAttribute = 0x2000 // start of user-defined attributes
	DW_AT_hi_user DwarfAttribute = 0x3fff // end of user-defined attributes
)

// DWARF form codes specify the on-disk encoding of a DWARF v5 attribute's value.
const (
	DW_FORM_addr      DwarfForm = 0x01 // absolute address
	DW_FORM_block2    DwarfForm = 0x02 // block with 2-byte length (MISSING in your code)
	DW_FORM_block4    DwarfForm = 0x03 // block with 4-byte length
	DW_FORM_data2     DwarfForm = 0x05 // 2-byte constant data
	DW_FORM_data4     DwarfForm = 0x06 // 4-byte constant data
	DW_FORM_data8     DwarfForm = 0x07 // 8-byte constant data
	DW_FORM_string    DwarfForm = 0x08 // null-terminated string
	DW_FORM_block     DwarfForm = 0x09 // block with ULEB128 length
	DW_FORM_block1    DwarfForm = 0x0a // block with 1-byte length
	DW_FORM_data1     DwarfForm = 0x0b // 1-byte constant data
	DW_FORM_flag      DwarfForm = 0x0c // 1-byte flag (boolean)
	DW_FORM_sdata     DwarfForm = 0x0d // sLEB128 constant
	DW_FORM_strp      DwarfForm = 0x0e // offset into .debug_str (string section)
	DW_FORM_udata     DwarfForm = 0x0f // uLEB128 constant
	DW_FORM_ref_addr  DwarfForm = 0x10 // address-sized reference to another DIE
	DW_FORM_ref1      DwarfForm = 0x11 // 1-byte reference to another DIE
	DW_FORM_ref2      DwarfForm = 0x12 // 2-byte reference to another DIE
	DW_FORM_ref4      DwarfForm = 0x13 // 4-byte reference to another DIE
	DW_FORM_ref8      DwarfForm = 0x14 // 8-byte reference to another DIE
	DW_FORM_ref_udata DwarfForm = 0x15 // uLEB128 reference to another DIE
	DW_FORM_indirect  DwarfForm = 0x16 // indirect form; next ULEB128 is the real form

	// DWARF v4/v5-specific
	DW_FORM_sec_offset     DwarfForm = 0x17 // offset into section (e.g. for ranges)
	DW_FORM_exprloc        DwarfForm = 0x18 // location expression (block)
	DW_FORM_flag_present   DwarfForm = 0x19 // implicit 'true' flag (no data)
	DW_FORM_strx           DwarfForm = 0x1a // string via .debug_str_offsets
	DW_FORM_addrx          DwarfForm = 0x1b // address via .debug_addr section
	DW_FORM_ref_sup4       DwarfForm = 0x1c // 4-byte reference to supplementary file
	DW_FORM_strp_sup       DwarfForm = 0x1d // string in supplementary .debug_str section
	DW_FORM_data16         DwarfForm = 0x1e // 16-byte constant data
	DW_FORM_line_strp      DwarfForm = 0x1f // offset into .debug_line_str
	DW_FORM_ref_sig8       DwarfForm = 0x20 // 8-byte signature reference (type units)
	DW_FORM_implicit_const DwarfForm = 0x21 // no data; value is implicit constant
	DW_FORM_loclistx       DwarfForm = 0x22 // index into location list
	DW_FORM_rnglistx       DwarfForm = 0x23 // index into range list
	DW_FORM_ref_sup8       DwarfForm = 0x24 // 8-byte reference to supplementary file
	DW_FORM_strx1          DwarfForm = 0x25 // strx with 1-byte index
	DW_FORM_strx2          DwarfForm = 0x26 // strx with 2-byte index
	DW_FORM_strx3          DwarfForm = 0x27 // strx with 3-byte index
	DW_FORM_strx4          DwarfForm = 0x28 // strx with 4-byte index
)

// DwarfUnitType represents the type of a DWARF v5 compilation unit or type unit.
const (
	DW_UT_compile       DwarfUnitType = 0x01 // full (non-split) compilation unit
	DW_UT_type          DwarfUnitType = 0x02 // type unit (conventional, lives in .debug_info in v5)
	DW_UT_partial       DwarfUnitType = 0x03 // partial compilation unit (non-split)
	DW_UT_skeleton      DwarfUnitType = 0x04 // skeleton compilation unit (paired with a split full CU)
	DW_UT_split_compile DwarfUnitType = 0x05 // split full compilation unit (.dwo / supplementary object)
	DW_UT_split_type    DwarfUnitType = 0x06 // split type unit (.dwo / supplementary object)

	// vendor range
	DW_UT_lo_user DwarfUnitType = 0x80 // start of user-defined unit-types
	DW_UT_hi_user DwarfUnitType = 0xff // end of user-defined unit-types
)

// DwarfLanguage represents DWARF v5 language codes for source languages.
const (
	DW_LANG_C89            DwarfLanguage = 0x0001 // ansi c
	DW_LANG_C              DwarfLanguage = 0x0002 // c
	DW_LANG_Ada83          DwarfLanguage = 0x0003 // ada 83
	DW_LANG_C_plus_plus    DwarfLanguage = 0x0004 // c++
	DW_LANG_Cobol74        DwarfLanguage = 0x0005 // cobol 74
	DW_LANG_Cobol85        DwarfLanguage = 0x0006 // cobol 85
	DW_LANG_Fortran77      DwarfLanguage = 0x0007 // fortran 77
	DW_LANG_Fortran90      DwarfLanguage = 0x0008 // fortran 90
	DW_LANG_Pascal83       DwarfLanguage = 0x0009 // pascal 83
	DW_LANG_Modula2        DwarfLanguage = 0x000a // modula-2
	DW_LANG_Java           DwarfLanguage = 0x000b // java
	DW_LANG_C99            DwarfLanguage = 0x000c // c99
	DW_LANG_Ada95          DwarfLanguage = 0x000d // ada 95
	DW_LANG_Fortran95      DwarfLanguage = 0x000e // fortran 95
	DW_LANG_PLI            DwarfLanguage = 0x000f // pl/i
	DW_LANG_ObjC           DwarfLanguage = 0x0010 // objective-c
	DW_LANG_ObjC_plus_plus DwarfLanguage = 0x0011 // objective-c++
	DW_LANG_UPC            DwarfLanguage = 0x0012 // upc
	DW_LANG_D              DwarfLanguage = 0x0013 // d
	DW_LANG_Python         DwarfLanguage = 0x0014 // python
	DW_LANG_OpenCL         DwarfLanguage = 0x0015 // opencl
	DW_LANG_Go             DwarfLanguage = 0x0016 // go
	DW_LANG_Modula3        DwarfLanguage = 0x0017 // modula-3
	DW_LANG_Haskell        DwarfLanguage = 0x0018 // haskell
	DW_LANG_C_plus_plus_03 DwarfLanguage = 0x0019 // c++03
	DW_LANG_C_plus_plus_11 DwarfLanguage = 0x001a // c++11
	DW_LANG_OCaml          DwarfLanguage = 0x001b // ocaml
	DW_LANG_Rust           DwarfLanguage = 0x001c // rust
	DW_LANG_C11            DwarfLanguage = 0x001d // c11
	DW_LANG_Swift          DwarfLanguage = 0x001e // swift
	DW_LANG_Julia          DwarfLanguage = 0x001f // julia
	DW_LANG_Dylan          DwarfLanguage = 0x0020 // dylan
	DW_LANG_C_plus_plus_14 DwarfLanguage = 0x0021 // c++14
	DW_LANG_Fortran03      DwarfLanguage = 0x0022 // fortran 2003
	DW_LANG_Fortran08      DwarfLanguage = 0x0023 // fortran 2008
	DW_LANG_RenderScript   DwarfLanguage = 0x0024 // renderscript
	DW_LANG_Blend2D        DwarfLanguage = 0x0025 // blend2d
	DW_LANG_C17            DwarfLanguage = 0x0026 // c17
	DW_LANG_C_plus_plus_17 DwarfLanguage = 0x0027 // c++17
	DW_LANG_C_plus_plus_20 DwarfLanguage = 0x0028 // c++20
	DW_LANG_C23            DwarfLanguage = 0x0029 // c23
	DW_LANG_C_plus_plus_23 DwarfLanguage = 0x002a // c++23

	// Reserved vendor range
	DW_LANG_lo_user DwarfLanguage = 0x8000 // lo user
	DW_LANG_hi_user DwarfLanguage = 0xffff // hi user
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
	DwarfCode int

	// Represents a DWARF tag (ULEB128-encoded).
	DwarfTag int

	// Represents a DWARF attribute (ULEB128-encoded).
	DwarfAttribute int

	// Represents a DWARF form (ULEB128-encoded).
	DwarfForm int

	// Represents a DWARF unit type (byte-encoded).
	DwarfUnitType int

	// Represents  a DWARF source language encoding (short-encoded).
	DwarfLanguage int

	// ElfSection represents a generic ELF section with typed line-contents.
	ElfSection[T fmt.Stringer] struct {
		Directives []DirectiveKind    `json:"directives"` // directives for building the section (e.g., .section, .p2align)
		Attributes []SectionAttribute `json:"attributes"` // attributes of the section (e.g., allocatable, writable, executable)
		Alignment  int                `json:"alignment"`  // power-of-2 alignment for section contents (used with ".p2align")
		Offsets    bool               `json:"offsets"`    // whether the section requires labels for offset calculations (e.g., .debug_str)
		Content    []T                `json:"content"`    // typed contents of this section (e.g., read-only data items, instructions)
	}

	// Directive represents a structured assembler directive with formal syntax.
	Directive struct {
		Directive DirectiveKind `json:"kind"`      // the directive kind (e.g., .type, .size, .global)
		Symbols   []string      `json:"symbols"`   // the symbol names this directive applies to
		Arguments []string      `json:"arguments"` // directive-specific arguments (e.g., "@function", ".-symbol")
		Comments  []string      `json:"comments"`  // comments associated with this directive
	}

	// A read-only data item holds one or several constant values that are not modified during program execution in an .rodata section.
	ReadOnlyDataItem struct {
		Kind   ReadOnlyDataKind `json:"kind"`   // kind of the read-only data item
		Labels []string         `json:"labels"` // literal data labels to access the read-only data item
		Values any              `json:"values"` // the values will be stored in a read-only section and encoded based on its kind
	}

	// A DWARF string item represents a string literal stored in the .debug_str section.
	StringItem struct {
		Label     string        `json:"label"`     // literal data label to access the string item
		Directive DirectiveKind `json:"directive"` // directive to use for the string item (e.g., .string, .asciz)
		Operand   string        `json:"operand"`   // the string value to be stored in the item
	}

	// A DWARF attribute form defines a single attribute in the .debug_abbrev section.
	AttributeForm struct {
		Attribute DwarfAttribute `json:"attribute"` // DW_AT_* code defines what this attribute represents (ULEB128)
		Form      DwarfForm      `json:"form"`      // DW_FORM_* code specifies how the attribute value is encoded (ULEB128)
	}

	// A DWARF attribute item defines a single attribute of a debugging information entry in the .debug_info section.
	AttributeItem struct {
		Directive DirectiveKind `json:"directive"` // directive to use for the attribute (e.g., .byte, .uleb128)
		Operand   any           `json:"operand"`   // the value to be stored in the attribute item (e.g., 0x00, .str_producer)
	}

	// A DWARF abbreviation entry defines a structure for debugging information entries in the .debug_abbrev section.
	AbbreviationEntry struct {
		Code        DwarfCode        `json:"code"`         // abbreviation code to be referenced by a DIE (ULEB128)
		Tag         DwarfTag         `json:"tag"`          // DW_TAG_* code for the entry used to identify the type of a DIE (ULEB128)
		HasChildren bool             `json:"has_children"` // 1 byte indicating if a DIE can have children
		Attributes  []*AttributeForm `json:"attributes"`   // list of attributes, implicitly ends before 0x00
	}

	// A DWARF debugging information entry represents a single unit of debugging information in the .debug_info section.
	DebuggingInformationEntry struct {
		Code       DwarfCode        `json:"dwarf_code"` // identify the debugging information entry
		Attributes []*AttributeItem `json:"attributes"` // attributes associated with the entry
	}
)

// Create a new ELF section with the specified directives, attributes, alignment, and offset calculation support.
func NewSection[T fmt.Stringer](directives []DirectiveKind, attributes []SectionAttribute, alignment int, offsets bool) *ElfSection[T] {
	return &ElfSection[T]{Directives: directives, Attributes: attributes, Alignment: alignment, Offsets: offsets, Content: make([]T, 0)}
}

// Create a new directive for the assembler.
func NewDirective(directive DirectiveKind, symbols []string, args ...string) *Directive {
	return &Directive{Directive: directive, Symbols: symbols, Arguments: args, Comments: make([]string, 0)}
}

// Create a new read-only data item with literal data labels for a read-only section.
func NewReadOnlyDataItem(kind ReadOnlyDataKind, labels []string, values any) *ReadOnlyDataItem {
	return &ReadOnlyDataItem{Kind: kind, Labels: labels, Values: values}
}

// Create a new string item for the .debug_str section.
func NewStringItem(label string, directive DirectiveKind, operand string) *StringItem {
	return &StringItem{Label: label, Directive: directive, Operand: operand}
}

// Create a new attribute form for the .debug_abbrev section.
func NewAttributeForm(attribute DwarfAttribute, form DwarfForm) *AttributeForm {
	return &AttributeForm{Attribute: attribute, Form: form}
}

// Create a new attribute item for the .debug_info section.
func NewAttributeItem(directive DirectiveKind, operand any) *AttributeItem {
	return &AttributeItem{Directive: directive, Operand: operand}
}

// Create a new abbreviation entry for the .debug_abbrev section.
func NewAbbreviationEntry(code DwarfCode, tag DwarfTag, hasChildren bool, attributes []*AttributeForm) *AbbreviationEntry {
	return &AbbreviationEntry{Code: code, Tag: tag, HasChildren: hasChildren, Attributes: attributes}
}

// Create a new debugging information entry for the .debug_info section.
func NewDebuggingInformationEntry(code DwarfCode, attributes []*AttributeItem) *DebuggingInformationEntry {
	return &DebuggingInformationEntry{Code: code, Attributes: attributes}
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

// String representation of a directive kind.
func (d DirectiveKind) String() string {
	return directiveNames[d]
}

// String representation of a prefix attribute.
func (a PrefixAttribute) String() string {
	return prefixAttributeNames[a]
}

// String representation of a section attribute.
func (a SectionAttribute) String() string {
	return sectionAttributeNames[a]
}

// String representation of a file attribute.
func (a FileAttribute) String() string {
	return fileAttributeNames[a]
}

// String representation of a location attribute.
func (a LocationAttribute) String() string {
	return locationAttributeNames[a]
}

// String representation of a type attribute.
func (a TypeAttribute) String() string {
	return typeAttributeNames[a]
}

// String representation of a size attribute.
func (a SizeAttribute) String() string {
	return sizeAttributeNames[a]
}

// String representation of a call frame information attribute.
func (a CallFrameInformationAttribute) String() string {
	return callFrameInformationAttributeNames[a]
}

// String representation of DWARF codes.
func (c DwarfCode) String() string {
	return dwarfCodeNames[c]
}

// String representation of DWARF tags.
func (t DwarfTag) String() string {
	return dwarfTagNames[t]
}

// String representation of DWARF attributes.
func (a DwarfAttribute) String() string {
	return dwarfAttributeNames[a]
}

// String representation of DWARF forms.
func (f DwarfForm) String() string {
	return dwarfFormNames[f]
}

// String representation of DWARF unit types.
func (t DwarfUnitType) String() string {
	return dwarfUnitTypeNames[t]
}

// String representation of DWARF languages.
func (l DwarfLanguage) String() string {
	return dwarfLanguageNames[l]
}

// Append a comment to a directive that will be emitted before the directive.
func (d *Directive) AppendComment(comment string) {
	if comment = strings.TrimSpace(comment); comment != "" {
		d.Comments = append(d.Comments, comment)
	}
}

// Create a start label for a directive.
func (d DirectiveKind) StartLabel() string {
	return fmt.Sprintf(startLabelFormat, strings.TrimLeft(d.String(), labelPrefix))
}

// Create an end label for a directive.
func (d DirectiveKind) EndLabel() string {
	return fmt.Sprintf(endLabelFormat, strings.TrimLeft(d.String(), labelPrefix))
}

// String representation of a section address based on its start label.
func (d DirectiveKind) ToSectionLabel() string {
	return strings.TrimRight(d.StartLabel(), labelPostfix)
}

// String representation of a section length calculation based on its start and end labels.
func ToSectionLength(endLabel, startLabel string) string {
	return fmt.Sprintf("%v - %v - 4", strings.TrimRight(endLabel, labelPostfix), strings.TrimRight(startLabel, labelPostfix))
}

// String representation of a string item address based on its label.
func ToStringItemLabel(item string) string {
	return fmt.Sprintf("%v%v", debugStringPrefix, item)
}

// String representation of a descriptor label.
func ToDescriptor(label string) string {
	return fmt.Sprintf(descriptorLabel, label)
}
