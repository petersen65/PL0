.intel_syntax noprefix
.section .debug_str                              # String Section für DW_FORM_strp
.Ldebug_str_start:
    .asciz "example.pl0"                         # CU name
    .asciz "pl0-go-compiler v0.1"                # producer
    .asciz "P"                                   # Funktion P
    .asciz "Q"                                   # Funktion Q
    .asciz "a"                                   # lokale Variable a
    .asciz "b"                                   # lokale Variable b
    .asciz "int64"                               # Basistyp
.Ldebug_str_end:

# Abbrev section ------------------------------------------------------------
.section .debug_abbrev
.Labbrev_start:
    # abbrev code 1: compile unit
    .uleb128 1                      # abbrev code
    .uleb128 0x11                   # DW_TAG_compile_unit
    .byte 1                         # has children: yes

        # attributes for compile unit
        .uleb128 0x03              # DW_AT_name
        .uleb128 0x08              # DW_FORM_strp
        .uleb128 0x25              # DW_AT_comp_dir (we omit, skip by not providing)
        .uleb128 0x00              # placeholder (we leave it out to simplify)
        .uleb128 0x1b              # DW_AT_language
        .uleb128 0x0b              # DW_FORM_data2
        .uleb128 0x10              # DW_AT_stmt_list
        .uleb128 0x0e              # DW_FORM_sec_offset
        .uleb128 0x3e              # DW_AT_producer
        .uleb128 0x08              # DW_FORM_strp
        .uleb128 0x11              # DW_AT_low_pc
        .uleb128 0x01              # DW_FORM_addr
        .uleb128 0x12              # DW_AT_high_pc
        .uleb128 0x0b              # DW_FORM_data4
    .uleb128 0                      # end of attributes for abbrev 1

    # abbrev code 2: base type (int64)
    .uleb128 2
    .uleb128 0x24                  # DW_TAG_base_type
    .byte 0                        # has children: no

        .uleb128 0x03             # DW_AT_name
        .uleb128 0x08             # DW_FORM_strp
        .uleb128 0x3e             # DW_AT_encoding
        .uleb128 0x01             # DW_FORM_data1
        .uleb128 0x0b             # DW_AT_byte_size
        .uleb128 0x01             # DW_FORM_data1
    .uleb128 0

    # abbrev code 3: subprogram (P or Q)
    .uleb128 3
    .uleb128 0x2e                  # DW_TAG_subprogram
    .byte 1                        # has children: yes

        .uleb128 0x03             # DW_AT_name
        .uleb128 0x08             # DW_FORM_strp
        .uleb128 0x1d             # DW_AT_linkage_name
        .uleb128 0x08             # DW_FORM_strp
        .uleb128 0x11             # DW_AT_low_pc
        .uleb128 0x01             # DW_FORM_addr
        .uleb128 0x12             # DW_AT_high_pc
        .uleb128 0x0b             # DW_FORM_data4   # high_pc as size
        .uleb128 0x13             # DW_AT_frame_base
        .uleb128 0x19             # DW_FORM_exprloc
    .uleb128 0

    # abbrev code 4: variable (lokal)
    .uleb128 4
    .uleb128 0x34                  # DW_TAG_variable
    .byte 0                        # has children: no

        .uleb128 0x03             # DW_AT_name
        .uleb128 0x08             # DW_FORM_strp
        .uleb128 0x49             # DW_AT_type
        .uleb128 0x0e             # DW_FORM_ref4
        .uleb128 0x2e             # DW_AT_location
        .uleb128 0x19             # DW_FORM_exprloc
    .uleb128 0

    # terminator
    .uleb128 0
.Labbrev_end:

# Info section --------------------------------------------------------------
.section .debug_info
.Linfo_start:
    # ---- Compile Unit Header ----
    # unit_length (placeholder, 4 bytes) -> will be fixed by linker if using dwarf5 with 64-bit length we would need more complex; GAS will adjust if using .long with final size not easily computable here, so use DWARF32 style with dummy then fixup not critical for example
    .long . - . + 0                 # placeholder; some linkers tolerate
    .short 5                        # version (DWARF v5)
    .long 0                         # abbrev offset (start of .debug_abbrev) -> 0 for simplicity if same relocation domain; advanced emitter would compute actual offset
    .byte 8                         # address size (8)

    # ---- DIE: Compile Unit (abbrev code 1) ----
    .uleb128 1                      # abbrev code
        # DW_AT_name -> strp to "example.pl0"
        .long (.Ldebug_str_start - .Ldebug_str_start)   # offset 0
        # DW_AT_language -> DW_LANG_C99 (0x0002)
        .short 0x0002
        # DW_AT_stmt_list -> 0 (let GAS fill line table reference)
        .long 0
        # DW_AT_producer -> strp "pl0-go-compiler v0.1"
        .long ( ( .Ldebug_str_start + 1 ) - .Ldebug_str_start )  # offset to second string (null-terminated after first)
        # DW_AT_low_pc -> address of P_start (we'll put as absolute)
        .quad P_start
        # DW_AT_high_pc -> size of the CU code span (we approximate)
        .long _cu_high_pc - P_start

    # ---- DIE: Base Type int64 (abbrev code 2) ----
    .uleb128 2
        # DW_AT_name -> "int64" (string index is after first and second and others; we can calculate naive)
        .long ( ( .Ldebug_str_start + 1 + 1 + 1 + 1 + 1 ) - .Ldebug_str_start )  # very rough; in real emitter compute precise offsets
        # DW_AT_encoding -> DW_ATE_signed (0x05)
        .byte 0x05
        # DW_AT_byte_size -> 8
        .byte 8

    # ---- DIE: Subprogram P (abbrev code 3) ----
    .uleb128 3
        # name "P"
        .long ( ( .Ldebug_str_start + 1 + 1 ) - .Ldebug_str_start )
        # linkage name "P"
        .long ( ( .Ldebug_str_start + 1 + 1 ) - .Ldebug_str_start )
        # low_pc
        .quad P_start
        # high_pc (size) = P_end - P_start
        .long P_end - P_start
        # frame_base: DW_OP_call_frame_cfa (0x9c) length 1
        .uleb128 1
            .byte 0x0c             # DW_OP_call_frame_cfa
        # child: variable a
        .uleb128 4
            # name "a"
            .long ( ( .Ldebug_str_start + 1 + 1 + 1 ) - .Ldebug_str_start )
            # type -> reference to base type (we assume DIE offset of base type is right before; here naive  ???)
            .long 0   # would be ref4 to base type DIE offset
            # location: exprloc with fbreg -8
            .uleb128 2              # length of expression: DW_OP_fbreg + SLEB128(-8) (1 + encoded)
                .byte 0x91          # DW_OP_fbreg
                .sleb128 -8
        # variable b
        .uleb128 4
            # name "b"
            .long ( ( .Ldebug_str_start + 1 + 1 + 2 ) - .Ldebug_str_start )
            .long 0
            .uleb128 2
                .byte 0x91
                .sleb128 -16

    # end of children for P
    .uleb128 0

    # ---- DIE: Subprogram Q (abbrev code 3) ----
    .uleb128 3
        # name "Q"
        .long ( ( .Ldebug_str_start + 1 + 2 ) - .Ldebug_str_start )
        # linkage name "Q"
        .long ( ( .Ldebug_str_start + 1 + 2 ) - .Ldebug_str_start )
        # low_pc
        .quad Q_start
        # high_pc (size)
        .long Q_end - Q_start
        # frame base
        .uleb128 1
            .byte 0x0c
        # variable a
        .uleb128 4
            .long ( ( .Ldebug_str_start + 1 + 2 + 1 ) - .Ldebug_str_start )
            .long 0
            .uleb128 2
                .byte 0x91
                .sleb128 -8
        # variable b
        .uleb128 4
            .long ( ( .Ldebug_str_start + 1 + 2 + 2 ) - .Ldebug_str_start )
            .long 0
            .uleb128 2
                .byte 0x91
                .sleb128 -16
    .uleb128 0  # end of children for Q
.Linfo_end:

# Text section mit actual code ------------------------------------------------
.section .text
.global main
.type main, @function
.extern P, Q

main:
    .cfi_startproc
    .loc 1 1 1
    push rbp
    .cfi_def_cfa_offset 16
    .cfi_offset rbp, -16
    mov rbp, rsp
    .cfi_def_cfa_register rbp

    # allocate locals (z.B. Platz für nichts hier)
    sub rsp, 32

    # Aufruf P
    call P
    # Aufruf Q
    call Q

    leave
    ret
    .cfi_endproc
.size main, .-main

# Funktion P
.global P
.type P, @function
P:
    .cfi_startproc
    .loc 1 10 5
    push rbp
    .cfi_def_cfa_offset 16
    .cfi_offset rbp, -16
    mov rbp, rsp
    .cfi_def_cfa_register rbp

    # Lokale int64 a und b: reserviert auf dem Stack, z.B. 16 bytes
    sub rsp, 32

    # (keine Operation, nur Dummy)
    nop

    leave
    ret
    .cfi_endproc
.size P, .-P

# Funktion Q
.global Q
.type Q, @function
Q:
    .cfi_startproc
    .loc 1 20 5
    push rbp
    .cfi_def_cfa_offset 16
    .cfi_offset rbp, -16
    mov rbp, rsp
    .cfi_def_cfa_register rbp

    sub rsp, 32
    nop

    leave
    ret
    .cfi_endproc
.size Q, .-Q

# EOF