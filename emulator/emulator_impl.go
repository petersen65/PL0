// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emulator

import (
	"fmt"
	"io"
	"math"
	"unsafe"

	cor "github.com/petersen65/PL0/v2/core"
	ac "github.com/petersen65/PL0/v2/emitter/assembly"
)

const (
	memorySize         = uint64(65536) // memory entries are 8-bit unsigned bytes (64KB)
	stackSize          = uint64(16384) // control stack entries are 8-bit unsigned bytes (16KB)
	stackForbiddenZone = uint64(1024)  // control stack entries below this address are forbidden to be used
	zero               = uint64(0)     // zero as 64-bit unsigned integer
)

const (
	_  = flag(iota)
	zf = 0x0000000000000040 // zero flag is set if the result of an arithmetic operation is zero
	sf = 0x0000000000000080 // sign flag is set if the result of an arithmetic operation is negative
	of = 0x0000000000000800 // overflow flag is set if the result of an arithmetic operation is too large to fit in the register
)

type (
	// Flags that reflect the state of the CPU and the result of arithmetic operations.
	flag int32

	// Supported data types for accessing memory and CPU registers.
	raw interface {
		~uint8 | ~uint16 | ~uint32 | ~uint64
	}

	// Process that holds assembly code instructions.
	process struct {
		assemblyCode ac.AssemblyCodeUnit // assembly code of the process
	}

	// CPU with its registers.
	cpu struct {
		registers map[ac.Register]uint64 // Registers of the CPU
	}

	// Emulation machine that can run processes.
	machine struct {
		cpu     cpu     // CPU of the machine
		memory  []byte  // memory of the machine
		process process // process running on the machine
	}
)

var (
	// Map any register to its bit size.
	registerSize = map[ac.Register]ac.OperandSize{
		ac.Rflags: ac.Bits64,
		ac.Rip:    ac.Bits64,
		ac.Rsp:    ac.Bits64,
		ac.Rbp:    ac.Bits64,
		ac.Rax:    ac.Bits64,
		ac.Rbx:    ac.Bits64,
		ac.Rcx:    ac.Bits64,
		ac.Rdx:    ac.Bits64,
		ac.Rsi:    ac.Bits64,
		ac.Rdi:    ac.Bits64,
		ac.R8:     ac.Bits64,
		ac.R9:     ac.Bits64,
		ac.R10:    ac.Bits64,
		ac.R11:    ac.Bits64,
		ac.R12:    ac.Bits64,
		ac.R13:    ac.Bits64,
		ac.R14:    ac.Bits64,
		ac.R15:    ac.Bits64,
		ac.Eax:    ac.Bits32,
		ac.Ebx:    ac.Bits32,
		ac.Ecx:    ac.Bits32,
		ac.Edx:    ac.Bits32,
		ac.Esi:    ac.Bits32,
		ac.Edi:    ac.Bits32,
		ac.R8d:    ac.Bits32,
		ac.R9d:    ac.Bits32,
		ac.R10d:   ac.Bits32,
		ac.R11d:   ac.Bits32,
		ac.R12d:   ac.Bits32,
		ac.R13d:   ac.Bits32,
		ac.R14d:   ac.Bits32,
		ac.R15d:   ac.Bits32,
		ac.Ax:     ac.Bits16,
		ac.Bx:     ac.Bits16,
		ac.Cx:     ac.Bits16,
		ac.Dx:     ac.Bits16,
		ac.Si:     ac.Bits16,
		ac.Di:     ac.Bits16,
		ac.R8w:    ac.Bits16,
		ac.R9w:    ac.Bits16,
		ac.R10w:   ac.Bits16,
		ac.R11w:   ac.Bits16,
		ac.R12w:   ac.Bits16,
		ac.R13w:   ac.Bits16,
		ac.R14w:   ac.Bits16,
		ac.R15w:   ac.Bits16,
		ac.Al:     ac.Bits8,
		ac.Bl:     ac.Bits8,
		ac.Cl:     ac.Bits8,
		ac.Dl:     ac.Bits8,
		ac.Ah:     ac.Bits8,
		ac.Bh:     ac.Bits8,
		ac.Ch:     ac.Bits8,
		ac.Dh:     ac.Bits8,
		ac.R8b:    ac.Bits8,
		ac.R9b:    ac.Bits8,
		ac.R10b:   ac.Bits8,
		ac.R11b:   ac.Bits8,
		ac.R12b:   ac.Bits8,
		ac.R13b:   ac.Bits8,
		ac.R14b:   ac.Bits8,
		ac.R15b:   ac.Bits8,
	}

	// Map 32-bit general purpose registers to corresponding 64-bit registers (32-bit registers are zero-extended to 64-bit).
	generalPurpose32to64 = map[ac.Register]ac.Register{
		ac.Eax:  ac.Rax, // bits 0-31 of Rax, set bits 32-63 to 0
		ac.Ebx:  ac.Rbx, // bits 0-31 of Rbx, set bits 32-63 to 0
		ac.Ecx:  ac.Rcx, // bits 0-31 of Rcx, set bits 32-63 to 0
		ac.Edx:  ac.Rdx, // bits 0-31 of Rdx, set bits 32-63 to 0
		ac.Esi:  ac.Rsi, // bits 0-31 of Rsi, set bits 32-63 to 0
		ac.Edi:  ac.Rdi, // bits 0-31 of Rdi, set bits 32-63 to 0
		ac.R8d:  ac.R8,  // bits 0-31 of R8, set bits 32-63 to 0
		ac.R9d:  ac.R9,  // bits 0-31 of R9, set bits 32-63 to 0
		ac.R10d: ac.R10, // bits 0-31 of R10, set bits 32-63 to 0
		ac.R11d: ac.R11, // bits 0-31 of R11, set bits 32-63 to 0
		ac.R12d: ac.R12, // bits 0-31 of R12, set bits 32-63 to 0
		ac.R13d: ac.R13, // bits 0-31 of R13, set bits 32-63 to 0
		ac.R14d: ac.R14, // bits 0-31 of R14, set bits 32-63 to 0
		ac.R15d: ac.R15, // bits 0-31 of R15, set bits 32-63 to 0
	}

	// Map 16-bit general purpose registers to corresponding 64-bit registers (16-bit registers leave bits 16-63 unchanged).
	generalPurpose16to64 = map[ac.Register]ac.Register{
		ac.Ax:   ac.Rax, // bits 0-15 of Rax
		ac.Bx:   ac.Rbx, // bits 0-15 of Rbx
		ac.Cx:   ac.Rcx, // bits 0-15 of Rcx
		ac.Dx:   ac.Rdx, // bits 0-15 of Rdx
		ac.Si:   ac.Rsi, // bits 0-15 of Rsi
		ac.Di:   ac.Rdi, // bits 0-15 of Rdi
		ac.R8w:  ac.R8,  // bits 0-15 of R8
		ac.R9w:  ac.R9,  // bits 0-15 of R9
		ac.R10w: ac.R10, // bits 0-15 of R10
		ac.R11w: ac.R11, // bits 0-15 of R11
		ac.R12w: ac.R12, // bits 0-15 of R12
		ac.R13w: ac.R13, // bits 0-15 of R13
		ac.R14w: ac.R14, // bits 0-15 of R14
		ac.R15w: ac.R15, // bits 0-15 of R15
	}

	// Map 8-bit general purpose registers to corresponding 64-bit registers (8-bit registers leave bits 8-63 or bits 16-63 unchanged).
	generalPurpose8to64 = map[ac.Register]ac.Register{
		ac.Al:   ac.Rax, // bits 0-7 of Rax
		ac.Bl:   ac.Rbx, // bits 0-7 of Rbx
		ac.Cl:   ac.Rcx, // bits 0-7 of Rcx
		ac.Dl:   ac.Rdx, // bits 0-7 of Rdx
		ac.Ah:   ac.Rax, // bits 8-15 of Rax
		ac.Bh:   ac.Rbx, // bits 8-15 of Rbx
		ac.Ch:   ac.Rcx, // bits 8-15 of Rcx
		ac.Dh:   ac.Rdx, // bits 8-15 of Rdx
		ac.R8b:  ac.R8,  // bits 0-7 of R8
		ac.R9b:  ac.R9,  // bits 0-7 of R9
		ac.R10b: ac.R10, // bits 0-7 of R10
		ac.R11b: ac.R11, // bits 0-7 of R11
		ac.R12b: ac.R12, // bits 0-7 of R12
		ac.R13b: ac.R13, // bits 0-7 of R13
		ac.R14b: ac.R14, // bits 0-7 of R14
		ac.R15b: ac.R15, // bits 0-7 of R15
	}
)

// Create a new emulation machine with CPU, registers, memory, and stack.
func newMachine() Machine {
	return &machine{
		cpu: cpu{
			registers: make(map[ac.Register]uint64),
		},
		memory: make([]byte, memorySize),
		process: process{
			assemblyCode: ac.NewAssemblyCodeUnit(),
		},
	}
}

// Load an assembly code unit and return an error if the import fails.
func (m *machine) Load(scan io.Reader) error {
	if err := m.process.assemblyCode.Import(cor.Binary, scan); err != nil {
		return err
	}

	return nil
}

// Run a process and return an error if the process fails to execute.
func (m *machine) RunProcess() error {
	// initialize 64-bit general purpose registers of the CPU
	m.cpu.set_gp(ac.Rax, zero) // accumulator register
	m.cpu.set_gp(ac.Rbx, zero) // base register
	m.cpu.set_gp(ac.Rcx, zero) // counter register
	m.cpu.set_gp(ac.Rdx, zero) // data register
	m.cpu.set_gp(ac.Rsi, zero) // source index register
	m.cpu.set_gp(ac.Rdi, zero) // destination index register
	m.cpu.set_gp(ac.R8, zero)  // general purpose register
	m.cpu.set_gp(ac.R9, zero)  // general purpose register
	m.cpu.set_gp(ac.R10, zero) // general purpose register
	m.cpu.set_gp(ac.R11, zero) // general purpose register
	m.cpu.set_gp(ac.R12, zero) // general purpose register
	m.cpu.set_gp(ac.R13, zero) // general purpose register
	m.cpu.set_gp(ac.R14, zero) // general purpose register
	m.cpu.set_gp(ac.R15, zero) // general purpose register

	// initialize 64-bit flags and instruction pointer registers of the CPU
	m.cpu.set_flg(zero)         // flags is a bit field that contains the status of the CPU and the result of arithmetic operations
	m.cpu.set_ptr(ac.Rip, zero) // instruction pointer points to the next instruction to be executed

	// initialize activation record descriptor of the main block
	set_mem(m, memorySize-ac.PointerSize, zero)   // static link (access link for compile-time block nesting hierarchy)
	set_mem(m, memorySize-2*ac.PointerSize, zero) // return address (to caller)

	// initialize stack pointer and base pointer registers of the CPU
	m.cpu.set_ptr(ac.Rsp, memorySize-2*ac.PointerSize) // points to return address before main block's prelude runs
	m.cpu.set_ptr(ac.Rbp, zero)                        // intentionnaly, there is no valid base pointer yet (from a caller)

	// the stack pointer and the base pointer point to 64-bit memory addresses (byte memory of the machine)
	// the instruction pointer points to 64-bit code addresses (assembly code of the process)
	for {
		// stack address space is byte memory from 'memorySize-1' down to 'memorySize-stackSize' excluding a forbidden zone
		if m.cpu.get_ptr(ac.Rsp) > memorySize-1 || m.cpu.get_ptr(ac.Rsp) < memorySize-stackSize+stackForbiddenZone {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, stackOverflow, m.cpu.get_ptr(ac.Rsp), nil)
		}

		// instruction address space is text section memory from '0' up to 'textSectionSize-1'
		if m.cpu.get_ptr(ac.Rip) >= uint64(m.process.assemblyCode.Length()) {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, addressOutOfRange, m.cpu.get_ptr(ac.Rip), nil)
		}

		// fetch non-nil instruction from text section memory using the instruction pointer
		instr := m.process.assemblyCode.GetInstruction(int(m.cpu.get_ptr(ac.Rip)))

		// increment the instruction pointer to point to the next instruction
		m.cpu.set_ptr(ac.Rip, m.cpu.get_ptr(ac.Rip)+1)

		// execute instructions until main block returns to external code
		switch instr.Operation {
		case ac.Push: // push operand on top of stack
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Push, nil)
			}

			if err := m.push(instr.Operands[0]); err != nil {
				return err
			}

		case ac.Pop: // pop element from top of stack into operand
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Pop, nil)
			}

			if err := m.pop(instr.Operands[0]); err != nil {
				return err
			}

		case ac.Mov: // copy second operand to first operand
			if len(instr.Operands) != 2 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Mov, nil)
			}

			if err := m.mov(instr.Operands[0], instr.Operands[1]); err != nil {
				return err
			}

		case ac.Jmp: // unconditionally jump to uint64 address
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Jmp, nil)
			}

			if err := m.cpu.jmp(instr.Operands[0]); err != nil {
				return err
			}

		case ac.Je: // jump to uint64 address if last comparison was equal
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Je, nil)
			}

			if err := m.cpu.je(instr.Operands[0]); err != nil {
				return err
			}

		case ac.Jne: // jump to uint64 address if last comparison was not equal
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Jne, nil)
			}

			if err := m.cpu.jne(instr.Operands[0]); err != nil {
				return err
			}

		case ac.Jl: // jump to uint64 address if last comparison was less than
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Jl, nil)
			}

			if err := m.cpu.jl(instr.Operands[0]); err != nil {
				return err
			}

		case ac.Jle: // jump to uint64 address if last comparison was less than or equal to
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Jle, nil)
			}

			if err := m.cpu.jle(instr.Operands[0]); err != nil {
				return err
			}

		case ac.Jg: // jump to uint64 address if last comparison was greater than
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Jg, nil)
			}

			if err := m.cpu.jg(instr.Operands[0]); err != nil {
				return err
			}

		case ac.Jge: // jump to uint64 address if last comparison was greater than or equal to
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Jge, nil)
			}

			if err := m.cpu.jge(instr.Operands[0]); err != nil {
				return err
			}

		case ac.Neg: // negate int64 element
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Neg, nil)
			}

			if err := m.cpu.neg(instr.Operands[0]); err != nil {
				return err
			}

		case ac.And: // test if element is odd and set zero flag if it is
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.And, nil)
			}

			if err := m.cpu.and(instr.Operands[0], 1); err != nil {
				return err
			}

		case ac.Add: // add two int64 elements and store the result
			if len(instr.Operands) != 2 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Add, nil)
			}

			if err := m.cpu.add(instr.Operands[0], instr.Operands[1]); err != nil {
				return err
			}

		case ac.Sub: // subtract two int64 elements and store the result
			if len(instr.Operands) != 2 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Sub, nil)
			}

			if err := m.cpu.sub(instr.Operands[0], instr.Operands[1]); err != nil {
				return err
			}

		case ac.Imul: // multiply two int64 elements and store the result
			if len(instr.Operands) != 2 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Imul, nil)
			}

			if err := m.cpu.imul(instr.Operands[0], instr.Operands[1]); err != nil {
				return err
			}

		case ac.Idiv: // divide two int64 elements and store the result
			if len(instr.Operands) != 2 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Idiv, nil)
			}

			if err := m.cpu.idiv(instr.Operands[0], instr.Operands[1]); err != nil {
				return err
			}

		case ac.Cmp: // compare two int64 elements and set flags register based on result
			if len(instr.Operands) != 2 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Cmp, nil)
			}

			if err := m.cpu.cmp(instr.Operands[0], instr.Operands[1]); err != nil {
				return err
			}

		case ac.Call: // caller function calls callee function
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Call, nil)
			}

			// call function at uint64 address
			if err := m.call(instr.Operands[0]); err != nil {
				return err
			}

		case ac.Ret: // callee function returns to caller function
			if err := m.ret(); err != nil {
				return err
			}

			// returning from the entrypoint of the program exits the program
			if m.cpu.get_ptr(ac.Rip) == 0 {
				return nil
			}

		case ac.StdCall: // call to programming language standard library based on standard call code
			if len(instr.Operands) != 1 || instr.Operands[0].Kind != ac.ImmediateOperand {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.StdCall, nil)
			}

			// call standard library function via call code
			if err := m.standard(ac.StandardCall(instr.Operands[0].Immediate.Value.(int64))); err != nil {
				return err
			}

		default:
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unknownOperation, m.cpu.get_ptr(ac.Rip)-1, nil)
		}
	}
}

// Call a programming language standard library function.
func (m *machine) standard(code ac.StandardCall) error {
	switch code {
	case ac.Readln:
		// read integer from stdin
		var input int64

		for {
			fmt.Print("> ")
			_, err := fmt.Scanln(&input)

			if err == nil {
				m.push(ac.NewImmediateOperand(ac.Bits64, input))
				break
			}
		}

	case ac.Writeln:
		// write integer to stdout
		m.pop(ac.NewRegisterOperand(ac.Rax))
		fmt.Printf("%v\n", m.cpu.get_gp(ac.Rax).(int64))

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unknownStandardCallCode, code, nil)
	}

	return nil
}

// Push operand on top of stack, top of stack points to new operand.
func (m *machine) push(op *ac.Operand) error {
	var value any

	switch op.Kind {
	case ac.RegisterOperand:
		// push the value of a register to the top of the stack
		if op.Register.IsGeneralPurpose() {
			value = m.cpu.get_gp(op.Register)
		} else {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Push, nil)
		}

	case ac.ImmediateOperand:
		// push an immediate value to the top of the stack
		value = op.Immediate

	case ac.JumpOperand:
		// push a jump address to the top of the stack
		value = op.Jump

	case ac.MemoryOperand:
		// push the content of a memory address to the top of the stack
		if op.Register.IsGeneralPurpose64() {
			// read the memory address from a 64-bit register
			address := m.cpu.get_gp(op.Register).(uint64)

			// calculate the memory address by adding the displacement to the register value
			address = uint64(int64(address) + op.Memory.Displacement)

			// get the value from the memory address
			value = get_mem_by_op(m, address, op.Memory.Size)
		} else {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Push, nil)
		}

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Push, nil)
	}

	// decrement the stack pointer (stack grows downwards)
	m.cpu.set_ptr(ac.Rsp, m.cpu.get_ptr(ac.Rsp)-get_mem_sz(value))

	// store the content at the new top of the stack
	set_mem_by_tp(m, m.cpu.get_ptr(ac.Rsp), value)

	return nil
}

// Pop element from top of stack into operand, top of stack points to previous element.
func (m *machine) pop(op *ac.Operand) error {
	switch op.Kind {
	case ac.RegisterOperand:
		// check if the register is a general purpose register
		if !op.Register.IsGeneralPurpose() {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Pop, nil)
		}

		// pop the value on the top of the stack into a register
		value := get_mem_by_op(m, m.cpu.get_ptr(ac.Rsp), registerSize[op.Register])
		m.cpu.set_gp(op.Register, value)

		// increment the stack pointer (stack shrinks upwards)
		m.cpu.set_ptr(ac.Rsp, m.cpu.get_ptr(ac.Rsp)+get_mem_sz(value))

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Pop, nil)
	}

	return nil
}

// Caller function calls callee function.
func (m *machine) call(op *ac.Operand) error {
	// push return address (instruction pointer of caller + 1)
	m.push(ac.NewJumpOperand(m.cpu.registers[ac.Rip]))

	// jump to function at uint64 address
	return m.cpu.jmp(op)
}

// Callee function returns to caller function.
func (m *machine) ret() error {
	// restore callers instruction pointer
	return m.pop(ac.NewRegisterOperand(ac.Rip))
}

// Copy second operand to first operand.
func (m *machine) mov(a, b *ac.Operand) error {
	switch {
	case a.Kind == ac.RegisterOperand && b.Kind == ac.RegisterOperand:
		// copy the value of the second register to the first register
		if a.Register.IsGeneralPurpose64() && b.Register.IsGeneralPurpose64() ||
			a.Register.IsGeneralPurpose32() && b.Register.IsGeneralPurpose32() ||
			a.Register.IsGeneralPurpose16() && b.Register.IsGeneralPurpose16() ||
			a.Register.IsGeneralPurpose8() && b.Register.IsGeneralPurpose8() {
			m.cpu.set_gp(a.Register, m.cpu.get_gp(b.Register))
		} else {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Mov, nil)
		}

	case a.Kind == ac.RegisterOperand && b.Kind == ac.ImmediateOperand:
		// copy the immediate value to the register
		if a.Register.IsGeneralPurpose64() {
			m.cpu.set_gp(a.Register, uint64(b.Immediate.Value.(int64)))
		} else {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Mov, nil)
		}

	case a.Kind == ac.RegisterOperand && b.Kind == ac.MemoryOperand:
		// copy the value from memory to the register
		if b.Register.IsGeneralPurpose64() && b.Memory.Size == registerSize[a.Register] {
			// read the memory address from a 64-bit register
			address := m.cpu.get_gp(b.Register).(uint64)

			// calculate the memory address by adding the displacement to the register value
			address = uint64(int64(address) + b.Memory.Displacement)

			// get the value from the memory address and store it in the register
			value := get_mem_by_op(m, address, b.Memory.Size)
			m.cpu.set_gp(a.Register, value)
		} else {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Mov, nil)
		}

	case a.Kind == ac.MemoryOperand && b.Kind == ac.RegisterOperand:
		// copy the value from the register to memory
		if a.Register.IsGeneralPurpose64() && a.Memory.Size == registerSize[b.Register] {
			// read the memory address from a 64-bit register
			address := m.cpu.get_gp(a.Register).(uint64)

			// calculate the memory address by adding the displacement to the register value
			address = uint64(int64(address) + a.Memory.Displacement)

			// store the value from the register to the memory address
			value := m.cpu.get_gp(b.Register)
			set_mem_by_tp(m, address, value)
		} else {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Mov, nil)
		}

	case a.Kind == ac.MemoryOperand && b.Kind == ac.ImmediateOperand:
		// copy the immediate value to memory
		if a.Register.IsGeneralPurpose64() {
			// read the memory address from a 64-bit register
			address := m.cpu.get_gp(a.Register).(uint64)

			// calculate the memory address by adding the displacement to the register value
			address = uint64(int64(address) + a.Memory.Displacement)

			// store the immediate value to the memory address
			set_mem_by_tp(m, address, uint64(b.Immediate.Value.(int64)))
		} else {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Mov, nil)
		}

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Mov, nil)
	}

	return nil
}

// Negate int64 element.
func (c *cpu) neg(op *ac.Operand) error {
	switch op.Kind {
	case ac.RegisterOperand:
		c.set_of_neg(int64(c.registers[op.Register]))

		if c.test_of() {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, arithmeticOverflowNegation, c.registers[ac.Rip]-1, nil)
		}

		c.registers[op.Register] = uint64(-int64(c.registers[op.Register]))
		c.set_zf(int64(c.registers[op.Register]))
		c.set_sf(int64(c.registers[op.Register]))

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Neg, nil)
	}

	return nil
}

// Perform bitwise 'and' operation with uint64 element and uint64 argument.
func (c *cpu) and(op *ac.Operand, arg uint64) error {
	switch op.Kind {
	case ac.RegisterOperand:
		c.registers[op.Register] = c.registers[op.Register] & arg

		c.set_zf(int64(c.registers[op.Register]))
		c.set_sf(int64(c.registers[op.Register]))
		c.unset_of()

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.And, nil)
	}

	return nil
}

// Add two int64 elements and store the result.
func (c *cpu) add(a, b *ac.Operand) error {
	var arg_b int64

	switch {
	case a.Kind == ac.RegisterOperand && b.Kind == ac.RegisterOperand:
		arg_b = int64(c.registers[b.Register])

	case a.Kind == ac.RegisterOperand && b.Kind == ac.ImmediateOperand:
		arg_b = b.Immediate.Value.(int64)

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Add, nil)
	}

	c.set_of_add(int64(c.registers[a.Register]), arg_b)

	if c.test_of() {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, arithmeticOverflowAddition, c.registers[ac.Rip]-1, nil)
	}

	c.registers[a.Register] = uint64(int64(c.registers[a.Register]) + arg_b)
	c.set_zf(int64(c.registers[a.Register]))
	c.set_sf(int64(c.registers[a.Register]))
	return nil
}

// Subtract two int64 elements and store the result.
func (c *cpu) sub(a, b *ac.Operand) error {
	var arg_b int64

	switch {
	case a.Kind == ac.RegisterOperand && b.Kind == ac.RegisterOperand:
		arg_b = int64(c.registers[b.Register])

	case a.Kind == ac.RegisterOperand && b.Kind == ac.ImmediateOperand:
		arg_b = b.Immediate.Value.(int64)

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Sub, nil)
	}

	c.set_of_sub(int64(c.registers[a.Register]), arg_b)

	if c.test_of() {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, arithmeticOverflowSubtraction, c.registers[ac.Rip]-1, nil)

	}

	c.registers[a.Register] = uint64(int64(c.registers[a.Register]) - arg_b)
	c.set_zf(int64(c.registers[a.Register]))
	c.set_sf(int64(c.registers[a.Register]))
	return nil
}

// Multiply two int64 elements and store the result.
func (c *cpu) imul(a, b *ac.Operand) error {
	var arg_b int64

	switch {
	case a.Kind == ac.RegisterOperand && b.Kind == ac.RegisterOperand:
		arg_b = int64(c.registers[b.Register])

	case a.Kind == ac.RegisterOperand && b.Kind == ac.ImmediateOperand:
		arg_b = b.Immediate.Value.(int64)

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Imul, nil)
	}

	c.set_of_mul(int64(c.registers[a.Register]), arg_b)

	if c.test_of() {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, arithmeticOverflowMultiplication, c.registers[ac.Rip]-1, nil)
	}

	c.registers[a.Register] = uint64(int64(c.registers[a.Register]) * arg_b)
	c.set_zf(int64(c.registers[a.Register]))
	c.set_sf(int64(c.registers[a.Register]))
	return nil
}

// Divide two int64 elements and store the result.
func (c *cpu) idiv(a, b *ac.Operand) error {
	var arg_b int64

	switch {
	case a.Kind == ac.RegisterOperand && b.Kind == ac.RegisterOperand:
		arg_b = int64(c.registers[b.Register])

	case a.Kind == ac.RegisterOperand && b.Kind == ac.ImmediateOperand:
		arg_b = b.Immediate.Value.(int64)

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Idiv, nil)
	}

	c.set_of_div(int64(c.registers[a.Register]), arg_b)

	if arg_b == 0 {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, divisionByZero, c.registers[ac.Rip]-1, nil)
	}

	if c.test_of() {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, arithmeticOverflowDivision, c.registers[ac.Rip]-1, nil)
	}

	c.registers[a.Register] = uint64(int64(c.registers[a.Register]) / arg_b)
	c.set_zf(int64(c.registers[a.Register]))
	c.set_sf(int64(c.registers[a.Register]))
	return nil
}

// Compare two int64 elements from top of stack and set flags register based on result (zero zf, sign sf, overflow of).
func (c *cpu) cmp(a, b *ac.Operand) error {
	var arg_b int64

	switch {
	case a.Kind == ac.RegisterOperand && b.Kind == ac.RegisterOperand:
		arg_b = int64(c.registers[b.Register])

	case a.Kind == ac.RegisterOperand && b.Kind == ac.ImmediateOperand:
		arg_b = b.Immediate.Value.(int64)

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Cmp, nil)
	}

	c.set_of_sub(int64(c.registers[a.Register]), arg_b)

	c.registers[a.Register] = uint64(int64(c.registers[a.Register]) - arg_b)
	c.set_zf(int64(c.registers[a.Register]))
	c.set_sf(int64(c.registers[a.Register]))
	return nil
}

// Unconditionally jump to uint64 address.
func (c *cpu) jmp(op *ac.Operand) error {
	switch op.Kind {
	case ac.RegisterOperand:
		c.registers[ac.Rip] = c.registers[op.Register]

	case ac.JumpOperand:
		c.registers[ac.Rip] = op.Jump

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, ac.Jmp, nil)
	}

	return nil
}

// Jump to uint64 address if zero flag is set, nz (not zero).
func (c *cpu) je(op *ac.Operand) error {
	if c.registers[ac.Rflags]&uint64(zf) != 0 {
		return c.jmp(op)
	}

	return nil
}

// Jump to uint64 address if zero flag is not set, zr (zero).
func (c *cpu) jne(op *ac.Operand) error {
	if c.registers[ac.Rflags]&uint64(zf) == 0 {
		return c.jmp(op)
	}

	return nil
}

// Jump to uint64 address if sign flag is not equal to overflow flag (sf != of).
func (c *cpu) jl(op *ac.Operand) error {
	if c.registers[ac.Rflags]&uint64(sf) != c.registers[ac.Rflags]&uint64(of) {
		return c.jmp(op)
	}

	return nil
}

// Jump to uint64 address if zero flag is set and sign flag is not equal to overflow flag (zf != 0, sf != of).
func (c *cpu) jle(op *ac.Operand) error {
	if c.registers[ac.Rflags]&uint64(zf) != 0 || c.registers[ac.Rflags]&uint64(sf) != c.registers[ac.Rflags]&uint64(of) {
		return c.jmp(op)
	}

	return nil
}

// Jump to uint64 address if zero flag is not set and sign flag is equal to overflow flag (zf == 0, sf == of).
func (c *cpu) jg(op *ac.Operand) error {
	if c.registers[ac.Rflags]&uint64(zf) == 0 && c.registers[ac.Rflags]&uint64(sf) == c.registers[ac.Rflags]&uint64(of) {
		return c.jmp(op)
	}

	return nil
}

// Jump to uint64 address if sign flag is equal to overflow flag (sf == of).
func (c *cpu) jge(op *ac.Operand) error {
	if c.registers[ac.Rflags]&uint64(sf) == c.registers[ac.Rflags]&uint64(of) {
		return c.jmp(op)
	}

	return nil
}

// Set zero flag if int64 element is zero.
func (c *cpu) set_zf(a int64) {
	if a == 0 {
		c.registers[ac.Rflags] |= uint64(zf)
	} else {
		c.registers[ac.Rflags] &= ^uint64(zf)
	}
}

// Set sign flag if int64 element is negative.
func (c *cpu) set_sf(a int64) {
	if a < 0 {
		c.registers[ac.Rflags] |= uint64(sf)
	} else {
		c.registers[ac.Rflags] &= ^uint64(sf)
	}
}

// Set overflow flag if negation of the int64 element overflows.
func (c *cpu) set_of_neg(a int64) {
	if a == math.MinInt64 {
		c.registers[ac.Rflags] |= uint64(of)
	} else {
		c.registers[ac.Rflags] &= ^uint64(of)
	}
}

// Set overflow flag if addition of two int64 elements overflows.
func (c *cpu) set_of_add(a, b int64) {
	s := a + b

	if (a > 0 && b > 0 && s < a) || (a < 0 && b < 0 && s > a) {
		c.registers[ac.Rflags] |= uint64(of)
	} else if (a > 0 && b < 0 && s > a) || (a < 0 && b > 0 && s < a) {
		c.registers[ac.Rflags] |= uint64(of)
	} else {
		c.registers[ac.Rflags] &= ^uint64(of)
	}
}

// Set overflow flag if subtraction of two int64 elements overflows.
func (c *cpu) set_of_sub(a, b int64) {
	s := a - b

	if (a > 0 && b < 0 && s < a) || (a < 0 && b > 0 && s > a) {
		c.registers[ac.Rflags] |= uint64(of)
	} else if (a > 0 && b > 0 && s > a) || (a < 0 && b < 0 && s < a) {
		c.registers[ac.Rflags] |= uint64(of)
	} else {
		c.registers[ac.Rflags] &= ^uint64(of)
	}
}

// Set overflow flag if multiplication of two int64 elements verflows.
func (c *cpu) set_of_mul(a, b int64) {
	s := a * b

	if a != 0 && s/a != b {
		c.registers[ac.Rflags] |= uint64(of)
	} else {
		c.registers[ac.Rflags] &= ^uint64(of)
	}
}

// Set overflow flag if division of two int64 elements overflows.
func (c *cpu) set_of_div(a, b int64) {
	if b == -1 && a == math.MinInt64 {
		c.registers[ac.Rflags] |= uint64(of)
	} else {
		c.registers[ac.Rflags] &= ^uint64(of)
	}
}

// Test overflow flag
func (c *cpu) test_of() bool {
	return c.registers[ac.Rflags]&uint64(of) != 0
}

// Clear overflow flag.
func (c *cpu) unset_of() {
	c.registers[ac.Rflags] &= ^uint64(of)
}

// Set a value in a general purpose register and panic if the register is not a general purpose register.
func (c *cpu) set_gp(r ac.Register, v any) {
	switch {
	case r.IsGeneralPurpose64():
		c.registers[r] = v.(uint64)

	case r.IsGeneralPurpose32():
		c.registers[to_64(r)] = uint64(v.(uint32))

	case r.IsGeneralPurpose16():
		c.registers[to_64(r)] = c.registers[to_64(r)]&0xffffffffffff0000 | uint64(v.(uint16))

	case r.IsGeneralPurposeLow8():
		c.registers[to_64(r)] = c.registers[to_64(r)]&0xffffffffffffff00 | uint64(v.(uint8))

	case r.IsGeneralPurposeHigh8():
		c.registers[to_64(r)] = c.registers[to_64(r)]&0xffffffffffff00ff | uint64(v.(uint8))<<8

	default:
		panic(cor.NewGeneralError(cor.Emulator, failureMap, cor.Fatal, unknownGeneralPurposeRegister, r, nil))
	}
}

// Get a value from a general purpose register and panic if the register is not a general purpose register.
func (c *cpu) get_gp(r ac.Register) any {
	switch {
	case r.IsGeneralPurpose64():
		return c.registers[r]

	case r.IsGeneralPurpose32():
		return uint32(c.registers[to_64(r)])

	case r.IsGeneralPurpose16():
		return uint16(c.registers[to_64(r)])

	case r.IsGeneralPurposeLow8():
		return uint8(c.registers[to_64(r)])

	case r.IsGeneralPurposeHigh8():
		return uint8(c.registers[to_64(r)] >> 8)

	default:
		panic(cor.NewGeneralError(cor.Emulator, failureMap, cor.Fatal, unknownGeneralPurposeRegister, r, nil))
	}
}

// Set a value in the flags register.
func (c *cpu) set_flg(v uint64) {
	c.registers[ac.Rflags] = v
}

// Get a value from the flags register.
func (c *cpu) get_flg() uint64 {
	return c.registers[ac.Rflags]
}

// Set an address in a pointer register and panic if the register is not a pointer register.
func (c *cpu) set_ptr(r ac.Register, v uint64) {
	if !r.IsInstructionPointer() && !r.IsStackPointer() && !r.IsBasePointer() {
		panic(cor.NewGeneralError(cor.Emulator, failureMap, cor.Fatal, unknownPointerRegister, r, nil))
	}

	c.registers[r] = v
}

// Get an address from a pointer register and panic if the register is not a pointer register.
func (c *cpu) get_ptr(r ac.Register) uint64 {
	if !r.IsInstructionPointer() && !r.IsStackPointer() && !r.IsBasePointer() {
		panic(cor.NewGeneralError(cor.Emulator, failureMap, cor.Fatal, unknownPointerRegister, r, nil))
	}

	return c.registers[r]
}

// Map any register to its corresponding 64-bit register.
func to_64(r ac.Register) ac.Register {
	if r.IsGeneralPurpose32() {
		return generalPurpose32to64[r]
	}
	if r.IsGeneralPurpose16() {
		return generalPurpose16to64[r]
	}
	if r.IsGeneralPurpose8() {
		return generalPurpose8to64[r]
	}

	return r
}

// Set a value in the memory space by type bit size.
func set_mem_by_tp(m *machine, a uint64, v any) {
	switch v := v.(type) {
	case uint64:
		set_mem(m, a, v)

	case uint32:
		set_mem(m, a, v)

	case uint16:
		set_mem(m, a, v)

	case uint8:
		set_mem(m, a, v)

	default:
		panic(cor.NewGeneralError(cor.Emulator, failureMap, cor.Fatal, invalidMemoryValue, v, nil))
	}
}

// Get a value from the memory space by operand bit size.
func get_mem_by_op(m *machine, a uint64, o ac.OperandSize) any {
	switch o {
	case ac.Bits64:
		return get_mem[uint64](m, a)

	case ac.Bits32:
		return get_mem[uint32](m, a)

	case ac.Bits16:
		return get_mem[uint16](m, a)

	case ac.Bits8:
		return get_mem[uint8](m, a)

	default:
		panic(cor.NewGeneralError(cor.Emulator, failureMap, cor.Fatal, invalidMemoryValue, o, nil))
	}
}

// Set a value in the memory space.
func set_mem[T raw](m *machine, a uint64, v T) {
	*(*T)(unsafe.Pointer(&m.memory[a])) = v
}

// Get a value from the memory space.
func get_mem[T raw](m *machine, a uint64) T {
	return *(*T)(unsafe.Pointer(&m.memory[a]))
}

// Calculate the memory size of any value in bytes.
func get_mem_sz(v any) uint64 {
	return uint64(unsafe.Sizeof(v))
}
