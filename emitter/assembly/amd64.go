// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package assembly

// Operation codes for assembly instructions of the AMD64 CPU.
const (
	_ = OperationCode(iota)

	// assembly instructions for data copy operations
	Push
	Pop
	Mov

	// comparison assembly instruction for all relational operators and conditional jumps
	Cmp

	// unconditional and conditional jump assembly instructions
	Jmp
	Je
	Jne
	Jl
	Jle
	Jg
	Jge

	// arithmetic assembly instructions with one operand
	Neg
	And

	// arithmetic assembly instructions with two operands
	Add
	Sub
	Imul
	Idiv

	// subroutine assembly instructions for procedure calls
	Call
	Ret
	StdCall
)

// Register enumeration for the 256 bit, 128-bit, 64-bit, 32-bit, 16-bit, and 8-bit registers of the AMD64 CPU.
const (
	_ = Register(iota)

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

	// 256 bit AVX registers of the AMD64 CPU (advanced vector extensions, AVX).
	Ymm0  // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm1  // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm2  // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm3  // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm4  // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm5  // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm6  // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm7  // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm8  // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm9  // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm10 // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm11 // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm12 // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm13 // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm14 // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)
	Ymm15 // floating point register (256 bits, 8 x 32-bits or 4 x 64-bits floating point numbers)

	// 128-bit SSE registers of the AMD64 CPU (streaming single instructions multiple data extensions, SSE).
	Xmm0  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm0)
	Xmm1  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm1)
	Xmm2  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm2)
	Xmm3  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm3)
	Xmm4  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm4)
	Xmm5  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm5)
	Xmm6  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm6)
	Xmm7  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm7)
	Xmm8  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm8)
	Xmm9  // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm9)
	Xmm10 // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm10)
	Xmm11 // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm11)
	Xmm12 // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm12)
	Xmm13 // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm13)
	Xmm14 // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm14)
	Xmm15 // floating point register (4 x 32-bits or 2 x 64-bits floating point numbers, bits 0-127 of Ymm15)
)

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

// Check if the register is a advanced vector extensions register.
func (r Register) IsAvx() bool {
	return r >= Ymm0 && r <= Ymm15
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

// Depending on the bit size of the operand, calculate the alignment of a positive or negativ offset. 
func (size OperandSize) Alignment(offset int64) int64 {
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

// Round up or down an offset to its next alignment boundary. If something is wrong with the offset or alignment, return 0.
func Align(offset, alignment int64) int64 {
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
