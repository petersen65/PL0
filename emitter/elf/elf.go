// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package elf defines constants and types for the executable and linkable format (ELF) used in assembly code generation.
// Note: ELF is part the System V ABI for x86_64 architecture and is used for linking and loading executable files.
package elf

import "fmt"

// Assembler directives for the ELF format supported by various assemblers on Linux.
const (
	// symbol declaration directives
	Global Directive = iota // declares a global symbol visible to the linker (.globl <name>)
	Extern                  // declares an external symbol defined elsewhere (.extern <name>)
	Type                    // specifies the symbol type for ELF (.type <name>, @function/@object/...)
	Size                    // declares the size of a symbol in bytes (.size <name>, .-<name>)
	Weak                    // marks a symbol as weak; can be overridden (.weak <name>)
	Hidden                  // makes a symbol hidden from external linking (.hidden <name>)

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

	// alignment and layout directives
	P2align // aligns to 2^n bytes (.p2align <n>) — used for ABI alignment
	Align   // aligns to n bytes (.align <n>) — less portable across platforms
	Balign  // aligns using byte-boundary logic (.balign <n>) — same as .align but stricter in ELF

	// value emission directives
	Byte   // emits a single byte value (.byte 0xFF)
	Word   // emits a 16-bit word (.word 0xABCD)
	Long   // emits a 32-bit value (.long 0x12345678)
	Quad   // emits a 64-bit value (.quad 0xDEADBEEFCAFEBABE)
	Zero   // emits a run of zero bytes (.zero <count>)
	String // emits a null-terminated ASCII string (.string "text")
	Ascii  // emits an unterminated ASCII string (.ascii "text")

	// debug info and DWARF metadata
	File         // specifies the source file name for debug info (.file "source.c")
	Loc          // marks source location in debug info (.loc file line column)
	Line         // specifies source line number (legacy) (.line <n>)
	CfiStartProc // begins a CFI frame for a function (.cfi_startproc)
	CfiEndProc   // ends a CFI frame for a function (.cfi_endproc)
	CfiDefCfa    // defines call frame address rule (.cfi_def_cfa reg offset)
	CfiOffset    // defines register offset in CFI (.cfi_offset reg offset)
)

// Section attributes for the .section directive.
const (
	SectionAllocatable SectionAttribute = iota // section is allocated in memory (flag "a")
	SectionWritable                            // section is writable at runtime (flag "w")
	SectionExecutable                          // section contains executable code (flag "x")
	SectionProgramBits                         // section contains data or instruction program bits (section kind "@progbits")
	SectionNoBits                              // section does not occupy space in the file (section kind "@nobits")
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
	SizeFromLabel  SizeAttribute = iota // calculate size from current location minus label (.-label)
	SizeAbsolute                        // use absolute size value
	SizeExpression                      // use arbitrary expression for size calculation
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

type (
	// Represents an assembler directive (pseudo-op).
	Directive int

	// Represents an attribute of the .section directive.
	SectionAttribute int

	// Represents a symbol type for the .type directive.
	TypeAttribute int

	// Represents a size calculation method for the .size directive.
	SizeAttribute int

	// Kind of read-only static data.
	ReadOnlyDataKind int

	// AssemblerSection represents a generic assembler section with typed contents.
	AssemblerSection[T fmt.Stringer] struct {
		Directives []Directive        // directives for building the section (e.g., .section, .p2align)
		Attributes []SectionAttribute // attributes of the section (e.g., allocatable, writable, executable)
		Alignment  int                // power-of-2 alignment for section contents (used with ".p2align")
		Content    []T                // typed contents of this section (e.g., read-only data items, instructions)
	}

	// A read-only data item holds one or several constant values that are not modified during program execution.
	ReadOnlyDataItem struct {
		Kind   ReadOnlyDataKind `json:"kind"`   // kind of the read-only data item
		Labels []string         `json:"labels"` // literal data labels to access the read-only data item
		Values any              `json:"values"` // the values will be stored in a read-only section and encoded based on its kind
	}

	// DirectiveDetail represents a structured assembler directive with formal syntax.
	DirectiveDetail struct {
		Directive Directive `json:"kind"`      // the directive (e.g., .type, .size, .global)
		Symbols   []string  `json:"symbols"`   // the symbol names this directive applies to
		Arguments []string  `json:"arguments"` // directive-specific arguments (e.g., "@function", ".-symbol")
	}
)

// Create a new assembler section with the specified directives, attributes, and alignment.
func NewAssemblerSection[T fmt.Stringer](directives []Directive, attributes []SectionAttribute, alignment int) *AssemblerSection[T] {
	return &AssemblerSection[T]{Directives: directives, Attributes: attributes, Alignment: alignment, Content: make([]T, 0)}
}

// Create a new read-only data item with literal data labels for a read-only section.
func NewReadOnlyDataItem(kind ReadOnlyDataKind, labels []string, values any) *ReadOnlyDataItem {
	return &ReadOnlyDataItem{Kind: kind, Labels: labels, Values: values}
}

// Create a new directive detail for symbol declarations.
func NewDirectiveDetail(directive Directive, symbols []string, args ...string) *DirectiveDetail {
	return &DirectiveDetail{Directive: directive, Symbols: symbols, Arguments: args}
}

// Create a .global directive for global symbols.
func NewGlobal(symbols []string) *DirectiveDetail {
	return NewDirectiveDetail(Global, symbols)
}

// Create a .extern directive for external symbols.
func NewExtern(symbols []string) *DirectiveDetail {
	return NewDirectiveDetail(Extern, symbols)
}

// Create a .type directive for a function symbol.
func NewTypeFunction(symbol string) *DirectiveDetail {
	return NewDirectiveDetail(Type, []string{symbol}, TypeFunction.String())
}

// Create a .type directive for an object symbol.
func NewTypeObject(symbol string) *DirectiveDetail {
	return NewDirectiveDetail(Type, []string{symbol}, TypeObject.String())
}

// Create a .size directive using the standard ".-symbol" calculation.
func NewSizeLabel(symbol string) *DirectiveDetail {
	return NewDirectiveDetail(Size, []string{symbol}, fmt.Sprintf(SizeFromLabel.String(), symbol))
}

// Create a .size directive with an absolute size value.
func NewSizeAbsolute(symbol string, size int) *DirectiveDetail {
	return NewDirectiveDetail(Size, []string{symbol}, fmt.Sprintf(SizeAbsolute.String(), size))
}

// String representation of a directive.
func (dk Directive) String() string {
	return directiveNames[dk]
}

// String representation of a section attribute.
func (sa SectionAttribute) String() string {
	return sectionAttributeNames[sa]
}

// String representation of a type attribute.
func (ta TypeAttribute) String() string {
	return typeAttributeNames[ta]
}

// String representation of a size attribute.
func (sa SizeAttribute) String() string {
	return sizeAttributeNames[sa]
}

// String representation of a descriptor label.
func ToDescriptor(label string) string {
	return fmt.Sprintf(descriptorLabel, label)
}
