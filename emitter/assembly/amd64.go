// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package assembly

// Operation codes for assembly instructions of the AMD64 CPU.
const (
	// used as empty prefix for the operation code
	None OperationCode = iota

	// instruction set architecture: ISA_Base
	Push    // pushes a register or immediate value onto the stack; decrements RSP by operand size
	Pop     // pops the top value from the stack into a register or memory; increments RSP by operand size
	Cmp     // subtracts source from destination, updates CPU flags for comparisons; result is discarded
	Test    // performs bitwise AND between operands, updates CPU flags for bit tests; result is discarded
	Cld     // clears the Direction Flag (DF) to make string operations increment address registers
	Rep     // repeats the following string instruction until RCX == 0
	Stosq   // stores RAX to [RDI]; increments or decrements RDI by 8 depending on DF
	Mov     // copies data from source to destination without modifying the source
	MovAbs  // moves a 64-bit immediate constant into a 64-bit register (required for full 64-bit immediates)
	Movsxd  // sign-extends a 32-bit value to 64 bits when moving into a 64-bit register
	Movsx   // sign-extends a smaller integer operand to a larger register size
	Movzx   // zero-extends a smaller integer operand to a larger register size
	Jmp     // performs an unconditional jump to a label or address
	Je      // jumps if Zero Flag (ZF) is set, meaning operands were equal
	Jne     // jumps if Zero Flag (ZF) is clear, meaning operands were not equal
	Jl      // jumps if Sign Flag ≠ Overflow Flag, meaning destination < source (signed)
	Jle     // jumps if Zero Flag is set or Sign Flag ≠ Overflow Flag, meaning destination ≤ source (signed)
	Jg      // jumps if Zero Flag is clear and Sign Flag == Overflow Flag, meaning destination > source (signed)
	Jge     // jumps if Sign Flag == Overflow Flag, meaning destination ≥ source (signed)
	Jb      // jumps if Carry Flag (CF) is set, meaning destination < source (unsigned)
	Jbe     // jumps if Zero Flag is set or Carry Flag is set, meaning destination ≤ source (unsigned)
	Ja      // jumps if Zero Flag is clear and Carry Flag is clear, meaning destination > source (unsigned)
	Jae     // jumps if Carry Flag is clear, meaning destination ≥ source (unsigned)
	Neg     // negates the operand (two’s complement), equivalent to subtracting it from zero
	And     // performs bitwise AND between destination and source; result stored in destination
	Xor     // performs bitwise XOR between destination and source; result stored in destination
	Cqo     // sign-extend RAX into the 128-bit dividend in RDX:RAX; required before IDIV for signed 64-bit division
	Add     // adds source to destination; result stored in destination
	Sub     // subtracts source from destination; result stored in destination
	Imul    // multiplies two signed integers; result stored in destination
	Div     // divides the 128-bit unsigned dividend in RDX:RAX by the source operand; quotient in RAX, remainder in RDX
	Idiv    // divides the 128-bit signed dividend in RDX:RAX by the source operand; quotient in RAX, remainder in RDX
	Call    // pushes return address onto the stack and jumps to a subroutine
	Ret     // pops return address from the stack and jumps to it, returning from a subroutine

	// instruction set architecture: ISA_SSE2
	Ucomisd // compares scalar double-precision floats and sets CPU flags
	Ucomiss // compares scalar single-precision floats and sets CPU flags
	Movsd   // moves a scalar 64-bit double-precision float between XMM registers or memory
	Movss   // moves a scalar 32-bit single-precision float between XMM registers or memory
	Movq    // moves a 64-bit integer or float between XMM registers or memory
	Movd    // moves a 32-bit integer between a general-purpose register and an XMM register
	Xorpd   // performs bitwise XOR on double-precision values in XMM registers
	Xorps   // performs bitwise XOR on single-precision values in XMM registers
	Addsd   // adds two scalar double-precision floats; result stored in destination XMM register
	Addss   // adds two scalar single-precision floats; result stored in destination XMM register
	Subsd   // subtracts one scalar double-precision float from another
	Subss   // subtracts one scalar single-precision float from another
	Mulsd   // multiplies two scalar double-precision floats
	Mulss   // multiplies two scalar single-precision floats
	Divsd   // divides one scalar double-precision float by another
	Divss   // divides one scalar single-precision float by another

	StdCall // to be removed, used for calling standard library functions
)

// Register enumeration for the 256 bit, 128-bit, 64-bit, 32-bit, 16-bit, and 8-bit registers of the AMD64 CPU.
const (
	_ Register = iota

	// 64-bit flags and pointer registers of the AMD64 CPU (used for control flow).
	Rflags // flags register contains the current state of the CPU and reflects the result of arithmetic operations
	Rip    // instruction pointer is pointing to the next instruction to be executed
	Rsp    // stack pointer is pointing to the top of the control stack
	Rbp    // base pointer is pointing to the base of an activation record

	// 64-bit general purpose registers of the AMD64 CPU (used for arithmetic and logical operations).
	Rax // accumulator is used for intermediate results of arithmetic operations
	Rbx // base register can be used for addressing variables
	Rcx // counter register can be used for counting iterations of loops
	Rdx // data register can be used for addressing variables
	Rsi // source index register used in string and array operations as a pointer to source data
	Rdi // destination index register is used used in string and array operations as a pointer to destination data
	R8  // 64-bit general purpose register
	R9  // 64-bit general purpose register
	R10 // 64-bit general purpose register
	R11 // 64-bit general purpose register
	R12 // 64-bit general purpose register
	R13 // 64-bit general purpose register
	R14 // 64-bit general purpose register
	R15 // 64-bit general purpose register

	// 32-bit general purpose registers of the AMD64 CPU (used for arithmetic and logical operations).
	Eax  // accumulator is used for intermediate results of arithmetic operations (bits 0-31 of Rax)
	Ebx  // base register can be used for addressing variables (bits 0-31 of Rbx)
	Ecx  // counter register can be used for counting iterations of loops (bits 0-31 of Rcx)
	Edx  // data register can be used for addressing variables (bits 0-31 of Rdx)
	Esi  // source index register used in string and array operations as a pointer to source data (bits 0-31 of Rsi)
	Edi  // destination index register is used used in string and array operations as a pointer to destination data (bits 0-31 of Rdi)
	R8d  // 32-bit general purpose register (bits 0-31 of R8)
	R9d  // 32-bit general purpose register (bits 0-31 of R9)
	R10d // 32-bit general purpose register (bits 0-31 of R10)
	R11d // 32-bit general purpose register (bits 0-31 of R11)
	R12d // 32-bit general purpose register (bits 0-31 of R12)
	R13d // 32-bit general purpose register (bits 0-31 of R13)
	R14d // 32-bit general purpose register (bits 0-31 of R14)
	R15d // 32-bit general purpose register (bits 0-31 of R15)

	// 16-bit general purpose registers of the AMD64 CPU (used for arithmetic and logical operations).
	Ax   // accumulator is used for intermediate results of arithmetic operations (bits 0-15 of Rax)
	Bx   // base register can be used for addressing variables (bits 0-15 of Rbx)
	Cx   // counter register can be used for counting iterations of loops (bits 0-15 of Rcx)
	Dx   // data register can be used for addressing variables (bits 0-15 of Rdx)
	Si   // source index register used in string and array operations as a pointer to source data (bits 0-15 of Rsi)
	Di   // destination index register is used used in string and array operations as a pointer to destination data (bits 0-15 of Rdi)
	R8w  // 16-bit general purpose register (bits 0-15 of R8)
	R9w  // 16-bit general purpose register (bits 0-15 of R9)
	R10w // 16-bit general purpose register (bits 0-15 of R10)
	R11w // 16-bit general purpose register (bits 0-15 of R11)
	R12w // 16-bit general purpose register (bits 0-15 of R12)
	R13w // 16-bit general purpose register (bits 0-15 of R13)
	R14w // 16-bit general purpose register (bits 0-15 of R14)
	R15w // 16-bit general purpose register (bits 0-15 of R15)

	// 8-bit general purpose registers of the AMD64 CPU (used for arithmetic and logical operations).
	Al   // accumulator is used for intermediate results of arithmetic operations (bits 0-7 of Rax)
	Bl   // base register can be used for addressing variables (bits 0-7 of Rbx)
	Cl   // counter register can be used for counting iterations of loops (bits 0-7 of Rcx)
	Dl   // data register can be used for addressing variables (bits 0-7 of Rdx)
	Ah   // accumulator is used for intermediate results of arithmetic operations (bits 8-15 of Rax)
	Bh   // base register can be used for addressing variables (bits 8-15 of Rbx)
	Ch   // counter register can be used for counting iterations of loops (bits 8-15 of Rcx)
	Dh   // data register can be used for addressing variables (bits 8-15 of Rdx)
	R8b  // 8-bit general purpose register (bits 0-7 of R8)
	R9b  // 8-bit general purpose register (bits 0-7 of R9)
	R10b // 8-bit general purpose register (bits 0-7 of R10)
	R11b // 8-bit general purpose register (bits 0-7 of R11)
	R12b // 8-bit general purpose register (bits 0-7 of R12)
	R13b // 8-bit general purpose register (bits 0-7 of R13)
	R14b // 8-bit general purpose register (bits 0-7 of R14)
	R15b // 8-bit general purpose register (bits 0-7 of R15)

	// 128-bit SSE registers of the AMD64 CPU (streaming single instructions multiple data extensions, SSE).
	Xmm0  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers)
	Xmm1  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers)
	Xmm2  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers)
	Xmm3  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers)
	Xmm4  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers)
	Xmm5  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers)
	Xmm6  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers)
	Xmm7  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers)
	Xmm8  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers)
	Xmm9  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers)
	Xmm10 // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers)
	Xmm11 // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers)
	Xmm12 // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers)
	Xmm13 // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers)
	Xmm14 // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers)
	Xmm15 // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers)
)

// Check if the operation code is an ISA_Base operation code.
func (o OperationCode) IsIsaBase() bool {
	return o >= Push && o <= Ret 
}

// Check if the operation code is an ISA_SSE2 operation code.
func (o OperationCode) IsIsaSse2() bool {
	return o >= Ucomisd && o <= Divss
}

// Check if the register is a general purpose 64-bit register.
func (r Register) IsGeneralPurpose64() bool {
	return r >= Rax && r <= R15 || r == Rsp || r == Rbp
}

// Check if the register is a general purpose 32-bit register.
func (r Register) IsGeneralPurpose32() bool {
	return r >= Eax && r <= R15d
}

// Check if the register is a general purpose 16-bit register.
func (r Register) IsGeneralPurpose16() bool {
	return r >= Ax && r <= R15w
}

// Check if the register is a general purpose 8-bit register.
func (r Register) IsGeneralPurpose8() bool {
	return r >= Al && r <= R15b
}

// Check if the register is a general purpose low 8-bit or 8-bit register.
func (r Register) IsGeneralPurposeLow8() bool {
	return r >= Al && r <= Dl || r >= R8b && r <= R15b
}

// Check if the register is a general purpose high 8-bit register.
func (r Register) IsGeneralPurposeHigh8() bool {
	return r >= Ah && r <= Dh
}

// Check if the register is any general purpose register.
func (r Register) IsGeneralPurpose() bool {
	return r.IsGeneralPurpose64() || r.IsGeneralPurpose32() || r.IsGeneralPurpose16() || r.IsGeneralPurpose8()
}

// Check if the register is a streaming single instructions multiple data extensions register.
func (r Register) IsSse() bool {
	return r >= Xmm0 && r <= Xmm15
}

// Check if the register is the flags 64-bit register.
func (r Register) IsFlags() bool {
	return r == Rflags
}

// Check if the register is the stack pointer 64-bit register.
func (r Register) IsStackPointer() bool {
	return r == Rsp
}

// Check if the register is the base pointer 64-bit register.
func (r Register) IsBasePointer() bool {
	return r == Rbp
}

// Check if the register is the instruction pointer 64-bit register.
func (r Register) IsInstructionPointer() bool {
	return r == Rip
}

// Depending on the bit size of the operand, calculate the alignment of a positive or negativ offset (offset has a +/- 2GB limit).
func (size OperandSize) Alignment(offset int32) int32 {
	switch size {
	case Bits64:
		return Align(offset, 8)

	case Bits32:
		return Align(offset, 4)

	case Bits16:
		return Align(offset, 2)

	case Bits8:
		return Align(offset, 1)

	default:
		return 0
	}
}

// Round up or down an offset to its next alignment boundary. If something is wrong with the offset or alignment, return 0 (offset has a +/- 2GB limit).
func Align(offset, alignment int32) int32 {
	// validate and make sure the alignment is a power of 2
	if offset == 0 || alignment <= 0 || alignment&(alignment-1) != 0 {
		return 0
	}

	// calculate the alignment of the offset depending on its sign (round up or down)
	if offset > 0 {
		return (offset + alignment - 1) & ^(alignment - 1)
	} else {
		return -(((-offset) + alignment - 1) & ^(alignment - 1))
	}
}
