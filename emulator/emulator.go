// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emulator

import (
	"fmt"
	"io"
	"math"
	"unsafe"

	cor "github.com/petersen65/PL0/v2/core"
	emi "github.com/petersen65/PL0/v2/emitter"
)

const (
	memorySize         = uint64(65536) // memory entries are 8-bit unsigned bytes (64KB)
	stackSize          = uint64(16384) // control stack entries are 8-bit unsigned bytes (16KB)
	pointerSize        = uint64(8)     // a pointer contains a memory address (8 bytes)
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
		assemblyCode emi.AssemblyCodeUnit // assembly code of the process
	}

	// CPU with its registers.
	cpu struct {
		registers map[emi.Register]uint64 // Registers of the CPU
	}

	// Emulation machine that can run processes.
	machine struct {
		cpu     cpu     // CPU of the machine
		memory  []byte  // memory of the machine
		process process // process running on the machine
	}
)

// Create a new emulation machine with CPU, registers, memory, and stack.
func newMachine() Machine {
	return &machine{
		cpu: cpu{
			registers: make(map[emi.Register]uint64),
		},
		memory: make([]byte, memorySize),
		process: process{
			assemblyCode: emi.NewAssemblyCodeUnit(),
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
	m.cpu.set_gp(emi.Rax, zero) // accumulator register
	m.cpu.set_gp(emi.Rbx, zero) // base register
	m.cpu.set_gp(emi.Rcx, zero) // counter register
	m.cpu.set_gp(emi.Rdx, zero) // data register
	m.cpu.set_gp(emi.Rsi, zero) // source index register
	m.cpu.set_gp(emi.Rdi, zero) // destination index register
	m.cpu.set_gp(emi.R8, zero)  // general purpose register
	m.cpu.set_gp(emi.R9, zero)  // general purpose register
	m.cpu.set_gp(emi.R10, zero) // general purpose register
	m.cpu.set_gp(emi.R11, zero) // general purpose register
	m.cpu.set_gp(emi.R12, zero) // general purpose register
	m.cpu.set_gp(emi.R13, zero) // general purpose register
	m.cpu.set_gp(emi.R14, zero) // general purpose register
	m.cpu.set_gp(emi.R15, zero) // general purpose register

	// initialize 64-bit flags and instruction pointer registers of the CPU
	m.cpu.set_flg(zero)          // flags is a bit field that contains the status of the CPU and the result of arithmetic operations
	m.cpu.set_ptr(emi.Rip, zero) // instruction pointer points to the next instruction to be executed

	// initialize activation record descriptor of the main block
	set_mem(m, memorySize-pointerSize, zero)   // static link (access link for compile-time block nesting hierarchy)
	set_mem(m, memorySize-2*pointerSize, zero) // return address (to caller)

	// initialize stack pointer and base pointer registers of the CPU
	m.cpu.set_ptr(emi.Rsp, memorySize-2*pointerSize) // points to return address before main block's prelude runs
	m.cpu.set_ptr(emi.Rbp, zero)                     // intentionnaly, there is no valid base pointer yet (from a caller)

	// the stack pointer and the base pointer point to 64-bit memory addresses (byte memory of the machine)
	// the instruction pointer points to 64-bit code addresses (assembly code of the process)
	for {
		// stack address space is byte memory from 'memorySize-1' down to 'memorySize-stackSize' excluding a forbidden zone
		if m.cpu.get_ptr(emi.Rsp) > memorySize-1 || m.cpu.get_ptr(emi.Rsp) < memorySize-stackSize+stackForbiddenZone {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, stackOverflow, m.cpu.get_ptr(emi.Rsp), nil)
		}

		// instruction address space is text section memory from '0' up to 'textSectionSize-1'
		if m.cpu.get_ptr(emi.Rip) >= uint64(m.process.assemblyCode.Length()) {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, addressOutOfRange, m.cpu.get_ptr(emi.Rip), nil)
		}

		// fetch non-nil instruction from text section memory using the instruction pointer
		instr := m.process.assemblyCode.GetInstruction(int(m.cpu.get_ptr(emi.Rip)))

		// increment the instruction pointer to point to the next instruction
		m.cpu.set_ptr(emi.Rip, m.cpu.get_ptr(emi.Rip)+1)

		// execute instructions until main block returns to external code
		switch instr.Operation {
		case emi.Push: // push operand on top of stack
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Push, nil)
			}

			if err := m.push(instr.Operands[0]); err != nil {
				return err
			}

		case emi.Pop: // pop element from top of stack into operand
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Pop, nil)
			}

			if err := m.pop(instr.Operands[0]); err != nil {
				return err
			}

		case emi.Mov: // copy second operand to first operand
			if len(instr.Operands) != 2 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Mov, nil)
			}

			if err := m.mov(instr.Operands[0], instr.Operands[1]); err != nil {
				return err
			}

		case emi.Jmp: // unconditionally jump to uint64 address
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Jmp, nil)
			}

			if err := m.cpu.jmp(instr.Operands[0]); err != nil {
				return err
			}

		case emi.Je: // jump to uint64 address if last comparison was equal
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Je, nil)
			}

			if err := m.cpu.je(instr.Operands[0]); err != nil {
				return err
			}

		case emi.Jne: // jump to uint64 address if last comparison was not equal
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Jne, nil)
			}

			if err := m.cpu.jne(instr.Operands[0]); err != nil {
				return err
			}

		case emi.Jl: // jump to uint64 address if last comparison was less than
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Jl, nil)
			}

			if err := m.cpu.jl(instr.Operands[0]); err != nil {
				return err
			}

		case emi.Jle: // jump to uint64 address if last comparison was less than or equal to
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Jle, nil)
			}

			if err := m.cpu.jle(instr.Operands[0]); err != nil {
				return err
			}

		case emi.Jg: // jump to uint64 address if last comparison was greater than
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Jg, nil)
			}

			if err := m.cpu.jg(instr.Operands[0]); err != nil {
				return err
			}

		case emi.Jge: // jump to uint64 address if last comparison was greater than or equal to
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Jge, nil)
			}

			if err := m.cpu.jge(instr.Operands[0]); err != nil {
				return err
			}

		case emi.Neg: // negate int64 element
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Neg, nil)
			}

			if err := m.cpu.neg(instr.Operands[0]); err != nil {
				return err
			}

		case emi.And: // test if element is odd and set zero flag if it is
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.And, nil)
			}

			if err := m.cpu.and(instr.Operands[0], 1); err != nil {
				return err
			}

		case emi.Add: // add two int64 elements and store the result
			if len(instr.Operands) != 2 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Add, nil)
			}

			if err := m.cpu.add(instr.Operands[0], instr.Operands[1]); err != nil {
				return err
			}

		case emi.Sub: // subtract two int64 elements and store the result
			if len(instr.Operands) != 2 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Sub, nil)
			}

			if err := m.cpu.sub(instr.Operands[0], instr.Operands[1]); err != nil {
				return err
			}

		case emi.Imul: // multiply two int64 elements and store the result
			if len(instr.Operands) != 2 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Imul, nil)
			}

			if err := m.cpu.imul(instr.Operands[0], instr.Operands[1]); err != nil {
				return err
			}

		case emi.Idiv: // divide two int64 elements and store the result
			if len(instr.Operands) != 2 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Idiv, nil)
			}

			if err := m.cpu.idiv(instr.Operands[0], instr.Operands[1]); err != nil {
				return err
			}

		case emi.Cmp: // compare two int64 elements and set flags register based on result
			if len(instr.Operands) != 2 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Cmp, nil)
			}

			if err := m.cpu.cmp(instr.Operands[0], instr.Operands[1]); err != nil {
				return err
			}

		case emi.Call: // caller function calls callee function
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Call, nil)
			}

			// call function at uint64 address
			if err := m.call(instr.Operands[0]); err != nil {
				return err
			}

		case emi.Ret: // callee function returns to caller function
			if err := m.ret(); err != nil {
				return err
			}

			// returning from the entrypoint of the program exits the program
			if m.cpu.get_ptr(emi.Rip) == 0 {
				return nil
			}

		case emi.StdCall: // call to programming language standard library based on standard call code
			if len(instr.Operands) != 1 || instr.Operands[0].Kind != emi.ImmediateOperand {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.StdCall, nil)
			}

			// call standard library function via call code
			if err := m.standard(emi.StandardCall(instr.Operands[0].Immediate.(int64))); err != nil {
				return err
			}

		default:
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unknownOperation, m.cpu.get_ptr(emi.Rip)-1, nil)
		}
	}
}

// Call a programming language standard library function.
func (m *machine) standard(code emi.StandardCall) error {
	switch code {
	case emi.Readln:
		// read integer from stdin
		var input int64

		for {
			fmt.Print("> ")
			_, err := fmt.Scanln(&input)

			if err == nil {
				m.push(emi.NewOperand(emi.ImmediateOperand, input))
				break
			}
		}

	case emi.Writeln:
		// write integer to stdout
		m.pop(emi.NewOperand(emi.RegisterOperand, emi.Rax))
		fmt.Printf("%v\n", m.cpu.get_gp(emi.Rax).(int64))

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unknownStandardCallCode, code, nil)
	}

	return nil
}

// Push operand on top of stack, top of stack points to new operand.
func (m *machine) push(op *emi.Operand) error {
	var value any

	switch op.Kind {
	case emi.RegisterOperand:
		// push the value of a register to the top of the stack
		if op.Register.IsPointer() {
			value = m.cpu.get_ptr(op.Register)
		} else if op.Register.IsGeneralPurpose() {
			value = m.cpu.get_gp(op.Register)
		} else {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Push, nil)
		}

	case emi.ImmediateOperand:
		// push an immediate value to the top of the stack
		value = op.Immediate

	case emi.JumpOperand:
		// push a jump address to the top of the stack
		value = op.Jump

	case emi.MemoryOperand:
		// push the content of a memory address to the top of the stack
		// read the memory address from the register referenced by the memory operand and add a displacement
		address := uint64(int64(get_reg[uint64](&m.cpu, op.Memory)) + op.Displacement)

		// get the value from the memory address
		value = get_mem[uint64](m, address)

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Push, nil)
	}

	// decrement the stack pointer (stack grows downwards)
	m.cpu.set_ptr(emi.Rsp, m.cpu.get_ptr(emi.Rsp)-uint64(unsafe.Sizeof(value)))

	// store the content at the new top of the stack
	switch v := value.(type) {
	case uint64:
		set_mem(m, m.cpu.get_ptr(emi.Rsp), v)

	case uint32:
		set_mem(m, m.cpu.get_ptr(emi.Rsp), v)

	case uint16:
		set_mem(m, m.cpu.get_ptr(emi.Rsp), v)

	case uint8:
		set_mem(m, m.cpu.get_ptr(emi.Rsp), v)

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, invalidMemoryValue, v, nil)
	}

	return nil
}

// Pop element from top of stack into operand, top of stack points to previous element.
func (m *machine) pop(op *emi.Operand) error {
	switch op.Kind {
	case emi.RegisterOperand:
		// pop the content of the top of the stack into a register
		arg := *(*uint64)(unsafe.Pointer(&m.memory[m.cpu.registers[emi.Rsp]]))
		m.cpu.registers[op.Register] = arg

		// increment the stack pointer (stack shrinks upwards)
		m.cpu.registers[emi.Rsp] += uint64(unsafe.Sizeof(arg))

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Pop, nil)
	}

	return nil
}

// Caller function calls callee function.
func (m *machine) call(op *emi.Operand) error {
	// push return address (instruction pointer of caller + 1)
	m.push(emi.NewOperand(emi.JumpOperand, m.cpu.registers[emi.Rip]))

	// jump to function at uint64 address
	return m.cpu.jmp(op)
}

// Callee function returns to caller function.
func (m *machine) ret() error {
	// restore callers instruction pointer
	return m.pop(emi.NewOperand(emi.RegisterOperand, emi.Rip))
}

// Copy second operand to first operand.
func (m *machine) mov(a, b *emi.Operand) error {
	switch {
	case a.Kind == emi.RegisterOperand && b.Kind == emi.RegisterOperand:
		m.cpu.registers[a.Register] = m.cpu.registers[b.Register]

	case a.Kind == emi.RegisterOperand && b.Kind == emi.ImmediateOperand:
		m.cpu.registers[a.Register] = uint64(b.Immediate.(int64))

	case a.Kind == emi.RegisterOperand && b.Kind == emi.MemoryOperand:
		address := uint64(int64(m.cpu.registers[b.Memory]) + b.Displacement)
		arg := *(*uint64)(unsafe.Pointer(&m.memory[address]))
		m.cpu.registers[a.Register] = arg

	case a.Kind == emi.MemoryOperand && b.Kind == emi.RegisterOperand:
		address := uint64(int64(m.cpu.registers[a.Memory]) + a.Displacement)
		*(*uint64)(unsafe.Pointer(&m.memory[address])) = m.cpu.registers[b.Register]

	case a.Kind == emi.MemoryOperand && b.Kind == emi.ImmediateOperand:
		address := uint64(int64(m.cpu.registers[a.Memory]) + a.Displacement)
		*(*uint64)(unsafe.Pointer(&m.memory[address])) = uint64(b.Immediate.(int64))

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Mov, nil)
	}

	return nil
}

// Negate int64 element.
func (c *cpu) neg(op *emi.Operand) error {
	switch op.Kind {
	case emi.RegisterOperand:
		c.set_of_neg(int64(c.registers[op.Register]))

		if c.test_of() {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, arithmeticOverflowNegation, c.registers[emi.Rip]-1, nil)
		}

		c.registers[op.Register] = uint64(-int64(c.registers[op.Register]))
		c.set_zf(int64(c.registers[op.Register]))
		c.set_sf(int64(c.registers[op.Register]))

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Neg, nil)
	}

	return nil
}

// Perform bitwise 'and' operation with uint64 element and uint64 argument.
func (c *cpu) and(op *emi.Operand, arg uint64) error {
	switch op.Kind {
	case emi.RegisterOperand:
		c.registers[op.Register] = c.registers[op.Register] & arg

		c.set_zf(int64(c.registers[op.Register]))
		c.set_sf(int64(c.registers[op.Register]))
		c.unset_of()

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.And, nil)
	}

	return nil
}

// Add two int64 elements and store the result.
func (c *cpu) add(a, b *emi.Operand) error {
	var arg_b int64

	switch {
	case a.Kind == emi.RegisterOperand && b.Kind == emi.RegisterOperand:
		arg_b = int64(c.registers[b.Register])

	case a.Kind == emi.RegisterOperand && b.Kind == emi.ImmediateOperand:
		arg_b = b.Immediate.(int64)

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Add, nil)
	}

	c.set_of_add(int64(c.registers[a.Register]), arg_b)

	if c.test_of() {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, arithmeticOverflowAddition, c.registers[emi.Rip]-1, nil)
	}

	c.registers[a.Register] = uint64(int64(c.registers[a.Register]) + arg_b)
	c.set_zf(int64(c.registers[a.Register]))
	c.set_sf(int64(c.registers[a.Register]))
	return nil
}

// Subtract two int64 elements and store the result.
func (c *cpu) sub(a, b *emi.Operand) error {
	var arg_b int64

	switch {
	case a.Kind == emi.RegisterOperand && b.Kind == emi.RegisterOperand:
		arg_b = int64(c.registers[b.Register])

	case a.Kind == emi.RegisterOperand && b.Kind == emi.ImmediateOperand:
		arg_b = b.Immediate.(int64)

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Sub, nil)
	}

	c.set_of_sub(int64(c.registers[a.Register]), arg_b)

	if c.test_of() {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, arithmeticOverflowSubtraction, c.registers[emi.Rip]-1, nil)

	}

	c.registers[a.Register] = uint64(int64(c.registers[a.Register]) - arg_b)
	c.set_zf(int64(c.registers[a.Register]))
	c.set_sf(int64(c.registers[a.Register]))
	return nil
}

// Multiply two int64 elements and store the result.
func (c *cpu) imul(a, b *emi.Operand) error {
	var arg_b int64

	switch {
	case a.Kind == emi.RegisterOperand && b.Kind == emi.RegisterOperand:
		arg_b = int64(c.registers[b.Register])

	case a.Kind == emi.RegisterOperand && b.Kind == emi.ImmediateOperand:
		arg_b = b.Immediate.(int64)

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Imul, nil)
	}

	c.set_of_mul(int64(c.registers[a.Register]), arg_b)

	if c.test_of() {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, arithmeticOverflowMultiplication, c.registers[emi.Rip]-1, nil)
	}

	c.registers[a.Register] = uint64(int64(c.registers[a.Register]) * arg_b)
	c.set_zf(int64(c.registers[a.Register]))
	c.set_sf(int64(c.registers[a.Register]))
	return nil
}

// Divide two int64 elements and store the result.
func (c *cpu) idiv(a, b *emi.Operand) error {
	var arg_b int64

	switch {
	case a.Kind == emi.RegisterOperand && b.Kind == emi.RegisterOperand:
		arg_b = int64(c.registers[b.Register])

	case a.Kind == emi.RegisterOperand && b.Kind == emi.ImmediateOperand:
		arg_b = b.Immediate.(int64)

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Idiv, nil)
	}

	c.set_of_div(int64(c.registers[a.Register]), arg_b)

	if arg_b == 0 {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, divisionByZero, c.registers[emi.Rip]-1, nil)
	}

	if c.test_of() {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, arithmeticOverflowDivision, c.registers[emi.Rip]-1, nil)
	}

	c.registers[a.Register] = uint64(int64(c.registers[a.Register]) / arg_b)
	c.set_zf(int64(c.registers[a.Register]))
	c.set_sf(int64(c.registers[a.Register]))
	return nil
}

// Compare two int64 elements from top of stack and set flags register based on result (zero zf, sign sf, overflow of).
func (c *cpu) cmp(a, b *emi.Operand) error {
	var arg_b int64

	switch {
	case a.Kind == emi.RegisterOperand && b.Kind == emi.RegisterOperand:
		arg_b = int64(c.registers[b.Register])

	case a.Kind == emi.RegisterOperand && b.Kind == emi.ImmediateOperand:
		arg_b = b.Immediate.(int64)

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Cmp, nil)
	}

	c.set_of_sub(int64(c.registers[a.Register]), arg_b)

	c.registers[a.Register] = uint64(int64(c.registers[a.Register]) - arg_b)
	c.set_zf(int64(c.registers[a.Register]))
	c.set_sf(int64(c.registers[a.Register]))
	return nil
}

// Unconditionally jump to uint64 address.
func (c *cpu) jmp(op *emi.Operand) error {
	switch op.Kind {
	case emi.RegisterOperand:
		c.registers[emi.Rip] = c.registers[op.Register]

	case emi.JumpOperand:
		c.registers[emi.Rip] = op.Jump

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Jmp, nil)
	}

	return nil
}

// Jump to uint64 address if zero flag is set, nz (not zero).
func (c *cpu) je(op *emi.Operand) error {
	if c.registers[emi.Rflags]&uint64(zf) != 0 {
		return c.jmp(op)
	}

	return nil
}

// Jump to uint64 address if zero flag is not set, zr (zero).
func (c *cpu) jne(op *emi.Operand) error {
	if c.registers[emi.Rflags]&uint64(zf) == 0 {
		return c.jmp(op)
	}

	return nil
}

// Jump to uint64 address if sign flag is not equal to overflow flag (sf != of).
func (c *cpu) jl(op *emi.Operand) error {
	if c.registers[emi.Rflags]&uint64(sf) != c.registers[emi.Rflags]&uint64(of) {
		return c.jmp(op)
	}

	return nil
}

// Jump to uint64 address if zero flag is set and sign flag is not equal to overflow flag (zf != 0, sf != of).
func (c *cpu) jle(op *emi.Operand) error {
	if c.registers[emi.Rflags]&uint64(zf) != 0 || c.registers[emi.Rflags]&uint64(sf) != c.registers[emi.Rflags]&uint64(of) {
		return c.jmp(op)
	}

	return nil
}

// Jump to uint64 address if zero flag is not set and sign flag is equal to overflow flag (zf == 0, sf == of).
func (c *cpu) jg(op *emi.Operand) error {
	if c.registers[emi.Rflags]&uint64(zf) == 0 && c.registers[emi.Rflags]&uint64(sf) == c.registers[emi.Rflags]&uint64(of) {
		return c.jmp(op)
	}

	return nil
}

// Jump to uint64 address if sign flag is equal to overflow flag (sf == of).
func (c *cpu) jge(op *emi.Operand) error {
	if c.registers[emi.Rflags]&uint64(sf) == c.registers[emi.Rflags]&uint64(of) {
		return c.jmp(op)
	}

	return nil
}

// Set zero flag if int64 element is zero.
func (c *cpu) set_zf(a int64) {
	if a == 0 {
		c.registers[emi.Rflags] |= uint64(zf)
	} else {
		c.registers[emi.Rflags] &= ^uint64(zf)
	}
}

// Set sign flag if int64 element is negative.
func (c *cpu) set_sf(a int64) {
	if a < 0 {
		c.registers[emi.Rflags] |= uint64(sf)
	} else {
		c.registers[emi.Rflags] &= ^uint64(sf)
	}
}

// Set overflow flag if negation of the int64 element overflows.
func (c *cpu) set_of_neg(a int64) {
	if a == math.MinInt64 {
		c.registers[emi.Rflags] |= uint64(of)
	} else {
		c.registers[emi.Rflags] &= ^uint64(of)
	}
}

// Set overflow flag if addition of two int64 elements overflows.
func (c *cpu) set_of_add(a, b int64) {
	s := a + b

	if (a > 0 && b > 0 && s < a) || (a < 0 && b < 0 && s > a) {
		c.registers[emi.Rflags] |= uint64(of)
	} else if (a > 0 && b < 0 && s > a) || (a < 0 && b > 0 && s < a) {
		c.registers[emi.Rflags] |= uint64(of)
	} else {
		c.registers[emi.Rflags] &= ^uint64(of)
	}
}

// Set overflow flag if subtraction of two int64 elements overflows.
func (c *cpu) set_of_sub(a, b int64) {
	s := a - b

	if (a > 0 && b < 0 && s < a) || (a < 0 && b > 0 && s > a) {
		c.registers[emi.Rflags] |= uint64(of)
	} else if (a > 0 && b > 0 && s > a) || (a < 0 && b < 0 && s < a) {
		c.registers[emi.Rflags] |= uint64(of)
	} else {
		c.registers[emi.Rflags] &= ^uint64(of)
	}
}

// Set overflow flag if multiplication of two int64 elements verflows.
func (c *cpu) set_of_mul(a, b int64) {
	s := a * b

	if a != 0 && s/a != b {
		c.registers[emi.Rflags] |= uint64(of)
	} else {
		c.registers[emi.Rflags] &= ^uint64(of)
	}
}

// Set overflow flag if division of two int64 elements overflows.
func (c *cpu) set_of_div(a, b int64) {
	if b == -1 && a == math.MinInt64 {
		c.registers[emi.Rflags] |= uint64(of)
	} else {
		c.registers[emi.Rflags] &= ^uint64(of)
	}
}

// Test overflow flag
func (c *cpu) test_of() bool {
	return c.registers[emi.Rflags]&uint64(of) != 0
}

// Clear overflow flag.
func (c *cpu) unset_of() {
	c.registers[emi.Rflags] &= ^uint64(of)
}

// Set a value in a general purpose register and panic if the register is not a general purpose register.
func (c *cpu) set_gp(r emi.Register, v any) {
	switch {
	case r.IsGeneralPurpose64():
		c.registers[r] = v.(uint64)

	case r.IsGeneralPurpose32():
		c.registers[r.To64()] = uint64(v.(uint32))

	case r.IsGeneralPurpose16():
		c.registers[r.To64()] = c.registers[r.To64()]&0xffffffffffff0000 | uint64(v.(uint16))

	case r.IsGeneralPurposeLow8():
		c.registers[r.To64()] = c.registers[r.To64()]&0xffffffffffffff00 | uint64(v.(uint8))

	case r.IsGeneralPurposeHigh8():
		c.registers[r.To64()] = c.registers[r.To64()]&0xffffffffffff00ff | uint64(v.(uint8))<<8

	default:
		panic(cor.NewGeneralError(cor.Emulator, failureMap, cor.Fatal, unknownGeneralPurposeRegister, r, nil))
	}
}

// Get a value from a general purpose register and panic if the register is not a general purpose register.
func (c *cpu) get_gp(r emi.Register) any {
	switch {
	case r.IsGeneralPurpose64():
		return c.registers[r]

	case r.IsGeneralPurpose32():
		return uint32(c.registers[r.To64()])

	case r.IsGeneralPurpose16():
		return uint16(c.registers[r.To64()])

	case r.IsGeneralPurposeLow8():
		return uint8(c.registers[r.To64()])

	case r.IsGeneralPurposeHigh8():
		return uint8(c.registers[r.To64()] >> 8)

	default:
		panic(cor.NewGeneralError(cor.Emulator, failureMap, cor.Fatal, unknownGeneralPurposeRegister, r, nil))
	}
}

// Set a value in the flags register.
func (c *cpu) set_flg(v uint64) {
	c.registers[emi.Rflags] = v
}

// Get a value from the flags register.
func (c *cpu) get_flg() uint64 {
	return c.registers[emi.Rflags]
}

// Set an address in a pointer register and panic if the register is not a pointer register.
func (c *cpu) set_ptr(r emi.Register, v uint64) {
	if !r.IsPointer() {
		panic(cor.NewGeneralError(cor.Emulator, failureMap, cor.Fatal, unknownPointerRegister, r, nil))
	}

	c.registers[r] = v
}

// Get an address from a pointer register and panic if the register is not a pointer register.
func (c *cpu) get_ptr(r emi.Register) uint64 {
	if !r.IsPointer() {
		panic(cor.NewGeneralError(cor.Emulator, failureMap, cor.Fatal, unknownPointerRegister, r, nil))
	}

	return c.registers[r]
}

// Set a value in the memory space.
func set_mem[T raw](m *machine, a uint64, v T) {
	*(*T)(unsafe.Pointer(&m.memory[a])) = v
}

// Get a value from the memory space.
func get_mem[T raw](m *machine, a uint64) T {
	return *(*T)(unsafe.Pointer(&m.memory[a]))
}
