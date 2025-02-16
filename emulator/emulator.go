// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emulator

import (
	"fmt"
	"io"
	"math"

	cor "github.com/petersen65/PL0/v2/core"
	emi "github.com/petersen65/PL0/v2/emitter"
)

const (
	memorySize         = 65536 // memory entries are 64-bit unsigned integers
	stackSize          = 16384 // control stack entries are 64-bit unsigned integers
	stackForbiddenZone = 1024  // control stack entries below this address are forbidden to be used
)

const (
	_  = flag(iota)
	zf = 0x0000000000000040 // zero flag is set if the result of an arithmetic operation is zero
	sf = 0x0000000000000080 // sign flag is set if the result of an arithmetic operation is negative
	of = 0x0000000000000800 // overflow flag is set if the result of an arithmetic operation is too large to fit in the register
)

type (
	// Flags that reflect the state of the CPU and the result of arithmetic operations.
	flag uint64

	// Virtual process that holds instructions of a binary target.
	process struct {
		assemblyCode emi.AssemblyCodeUnit // assembly code instructions of the process
		stackPointer uint64               // memory address of the downward growing stack
	}

	// Virtual CPU with its registers.
	cpu struct {
		registers map[emi.Register]uint64 // registers of the CPU
	}

	// Virtual machine that can run processes.
	machine struct {
		cpu     cpu      // CPU of the virtual machine
		memory  []uint64 // memory of the virtual machine
		process process  // process running on the virtual machine
	}
)

// Create a new emulation machine with CPU, registers, memory, and stack.
func newMachine() Machine {
	return &machine{
		cpu: cpu{
			registers: make(map[emi.Register]uint64),
		},
		memory: make([]uint64, memorySize),
		process: process{
			assemblyCode: emi.NewAssemblyCodeUnit(),
			stackPointer: memorySize - 1,
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
	m.cpu.registers[emi.Rax] = 0   // accumulator register
	m.cpu.registers[emi.Rbx] = 0   // base register
	m.cpu.registers[emi.Rcx] = 0   // counter register
	m.cpu.registers[emi.Rdx] = 0   // data register
	m.cpu.registers[emi.Rsi] = 0   // source index register
	m.cpu.registers[emi.Rdi] = 0   // destination index register
	m.cpu.registers[emi.R8] = 0    // general purpose register
	m.cpu.registers[emi.R9] = 0    // general purpose register
	m.cpu.registers[emi.R10] = 0   // general purpose register
	m.cpu.registers[emi.R11] = 0   // general purpose register
	m.cpu.registers[emi.R12] = 0   // general purpose register
	m.cpu.registers[emi.R13] = 0   // general purpose register
	m.cpu.registers[emi.R14] = 0   // general purpose register
	m.cpu.registers[emi.R15] = 0   // general purpose register
	m.cpu.registers[emi.Flags] = 0 // flags register
	m.cpu.registers[emi.Rip] = 0   // instruction pointer

	// initialize activation record descriptor of main block
	m.memory[m.process.stackPointer] = 0                  // static link (access link for compile-time block nesting hierarchy)
	m.memory[m.process.stackPointer-1] = 0                // return address (to caller)
	m.cpu.registers[emi.Rsp] = m.process.stackPointer - 1 // stack pointer points to return address before main block's prelude runs
	m.cpu.registers[emi.Rbp] = 0                          // intentionnaly, there is no valid base pointer yet (from a caller)

	// execute instructions until main block return to external code
	for {
		if m.cpu.registers[emi.Rip] >= uint64(m.process.assemblyCode.Length()) {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, addressOutOfRange, m.cpu.registers[emi.Rip], nil)
		}

		// stack address space is from 'stackPointer' down to 'stackPointer - stackSize + 1' excluding a forbidden zone
		if m.cpu.registers[emi.Rsp] <= m.process.stackPointer-stackSize+stackForbiddenZone ||
			m.cpu.registers[emi.Rsp] > m.process.stackPointer {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, stackOverflow, m.cpu.registers[emi.Rsp], nil)
		}

		instr := m.process.assemblyCode.GetInstruction(int(m.cpu.registers[emi.Rip]))
		m.cpu.registers[emi.Rip]++

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
			if m.cpu.registers[emi.Rip] == 0 {
				return nil
			}

		case emi.StdCall: // call to programming language standard library based on standard call code
			if len(instr.Operands) != 1 || instr.Operands[0].OperandKind != emi.ImmediateOperand {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.StdCall, nil)
			}

			// call standard library function via call code
			if err := m.standard(emi.StandardCall(instr.Operands[0].ArgInt)); err != nil {
				return err
			}

		default:
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unknownOperation, m.cpu.registers[emi.Rip]-1, nil)
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
		fmt.Printf("%v\n", int64(m.cpu.registers[emi.Rax]))

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unknownStandardCallCode, code, nil)
	}

	return nil
}

// Push operand on top of stack, top of stack points to new operand.
func (m *machine) push(op *emi.Operand) error {
	var arg uint64

	switch op.OperandKind {
	case emi.RegisterOperand:
		arg = m.cpu.registers[op.Register]

	case emi.ImmediateOperand:
		arg = uint64(op.ArgInt)

	case emi.JumpOperand:
		arg = op.Jump

	case emi.MemoryOperand:
		arg = m.memory[int64(m.cpu.registers[op.Memory])+op.Displacement]

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Push, nil)
	}

	m.cpu.registers[emi.Rsp]--
	m.memory[m.cpu.registers[emi.Rsp]] = arg
	return nil
}

// Pop element from top of stack into operand, top of stack points to previous element.
func (m *machine) pop(op *emi.Operand) error {
	switch op.OperandKind {
	case emi.RegisterOperand:
		m.cpu.registers[op.Register] = m.memory[m.cpu.registers[emi.Rsp]]

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Pop, nil)
	}

	m.cpu.registers[emi.Rsp]++
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
	case a.OperandKind == emi.RegisterOperand && b.OperandKind == emi.RegisterOperand:
		m.cpu.registers[a.Register] = m.cpu.registers[b.Register]

	case a.OperandKind == emi.RegisterOperand && b.OperandKind == emi.ImmediateOperand:
		m.cpu.registers[a.Register] = uint64(b.ArgInt)

	case a.OperandKind == emi.RegisterOperand && b.OperandKind == emi.MemoryOperand:
		m.cpu.registers[a.Register] = m.memory[int64(m.cpu.registers[b.Memory])+b.Displacement]

	case a.OperandKind == emi.MemoryOperand && b.OperandKind == emi.RegisterOperand:
		m.memory[int64(m.cpu.registers[a.Memory])+a.Displacement] = m.cpu.registers[b.Register]

	case a.OperandKind == emi.MemoryOperand && b.OperandKind == emi.ImmediateOperand:
		m.memory[int64(m.cpu.registers[a.Memory])+a.Displacement] = uint64(b.ArgInt)

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, emi.Mov, nil)
	}

	return nil
}

// Negate int64 element.
func (c *cpu) neg(op *emi.Operand) error {
	switch op.OperandKind {
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
	switch op.OperandKind {
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
	case a.OperandKind == emi.RegisterOperand && b.OperandKind == emi.RegisterOperand:
		arg_b = int64(c.registers[b.Register])

	case a.OperandKind == emi.RegisterOperand && b.OperandKind == emi.ImmediateOperand:
		arg_b = b.ArgInt

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
	case a.OperandKind == emi.RegisterOperand && b.OperandKind == emi.RegisterOperand:
		arg_b = int64(c.registers[b.Register])

	case a.OperandKind == emi.RegisterOperand && b.OperandKind == emi.ImmediateOperand:
		arg_b = b.ArgInt

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
	case a.OperandKind == emi.RegisterOperand && b.OperandKind == emi.RegisterOperand:
		arg_b = int64(c.registers[b.Register])

	case a.OperandKind == emi.RegisterOperand && b.OperandKind == emi.ImmediateOperand:
		arg_b = b.ArgInt

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
	case a.OperandKind == emi.RegisterOperand && b.OperandKind == emi.RegisterOperand:
		arg_b = int64(c.registers[b.Register])

	case a.OperandKind == emi.RegisterOperand && b.OperandKind == emi.ImmediateOperand:
		arg_b = b.ArgInt

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
	case a.OperandKind == emi.RegisterOperand && b.OperandKind == emi.RegisterOperand:
		arg_b = int64(c.registers[b.Register])

	case a.OperandKind == emi.RegisterOperand && b.OperandKind == emi.ImmediateOperand:
		arg_b = b.ArgInt

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
	switch op.OperandKind {
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
	if c.registers[emi.Flags]&uint64(zf) != 0 {
		return c.jmp(op)
	}

	return nil
}

// Jump to uint64 address if zero flag is not set, zr (zero).
func (c *cpu) jne(op *emi.Operand) error {
	if c.registers[emi.Flags]&uint64(zf) == 0 {
		return c.jmp(op)
	}

	return nil
}

// Jump to uint64 address if sign flag is not equal to overflow flag (sf != of).
func (c *cpu) jl(op *emi.Operand) error {
	if c.registers[emi.Flags]&uint64(sf) != c.registers[emi.Flags]&uint64(of) {
		return c.jmp(op)
	}

	return nil
}

// Jump to uint64 address if zero flag is set and sign flag is not equal to overflow flag (zf != 0, sf != of).
func (c *cpu) jle(op *emi.Operand) error {
	if c.registers[emi.Flags]&uint64(zf) != 0 || c.registers[emi.Flags]&uint64(sf) != c.registers[emi.Flags]&uint64(of) {
		return c.jmp(op)
	}

	return nil
}

// Jump to uint64 address if zero flag is not set and sign flag is equal to overflow flag (zf == 0, sf == of).
func (c *cpu) jg(op *emi.Operand) error {
	if c.registers[emi.Flags]&uint64(zf) == 0 && c.registers[emi.Flags]&uint64(sf) == c.registers[emi.Flags]&uint64(of) {
		return c.jmp(op)
	}

	return nil
}

// Jump to uint64 address if sign flag is equal to overflow flag (sf == of).
func (c *cpu) jge(op *emi.Operand) error {
	if c.registers[emi.Flags]&uint64(sf) == c.registers[emi.Flags]&uint64(of) {
		return c.jmp(op)
	}

	return nil
}

// Set zero flag if int64 element is zero.
func (c *cpu) set_zf(a int64) {
	if a == 0 {
		c.registers[emi.Flags] |= uint64(zf)
	} else {
		c.registers[emi.Flags] &= ^uint64(zf)
	}
}

// Set sign flag if int64 element is negative.
func (c *cpu) set_sf(a int64) {
	if a < 0 {
		c.registers[emi.Flags] |= uint64(sf)
	} else {
		c.registers[emi.Flags] &= ^uint64(sf)
	}
}

// Set overflow flag if negation of the int64 element overflows.
func (c *cpu) set_of_neg(a int64) {
	if a == math.MinInt64 {
		c.registers[emi.Flags] |= uint64(of)
	} else {
		c.registers[emi.Flags] &= ^uint64(of)
	}
}

// Set overflow flag if addition of two int64 elements overflows.
func (c *cpu) set_of_add(a, b int64) {
	s := a + b

	if (a > 0 && b > 0 && s < a) || (a < 0 && b < 0 && s > a) {
		c.registers[emi.Flags] |= uint64(of)
	} else if (a > 0 && b < 0 && s > a) || (a < 0 && b > 0 && s < a) {
		c.registers[emi.Flags] |= uint64(of)
	} else {
		c.registers[emi.Flags] &= ^uint64(of)
	}
}

// Set overflow flag if subtraction of two int64 elements overflows.
func (c *cpu) set_of_sub(a, b int64) {
	s := a - b

	if (a > 0 && b < 0 && s < a) || (a < 0 && b > 0 && s > a) {
		c.registers[emi.Flags] |= uint64(of)
	} else if (a > 0 && b > 0 && s > a) || (a < 0 && b < 0 && s < a) {
		c.registers[emi.Flags] |= uint64(of)
	} else {
		c.registers[emi.Flags] &= ^uint64(of)
	}
}

// Set overflow flag if multiplication of two int64 elements verflows.
func (c *cpu) set_of_mul(a, b int64) {
	s := a * b

	if a != 0 && s/a != b {
		c.registers[emi.Flags] |= uint64(of)
	} else {
		c.registers[emi.Flags] &= ^uint64(of)
	}
}

// Set overflow flag if division of two int64 elements overflows.
func (c *cpu) set_of_div(a, b int64) {
	if b == -1 && a == math.MinInt64 {
		c.registers[emi.Flags] |= uint64(of)
	} else {
		c.registers[emi.Flags] &= ^uint64(of)
	}
}

// Test overflow flag
func (c *cpu) test_of() bool {
	return c.registers[emi.Flags]&uint64(of) != 0
}

// Clear overflow flag.
func (c *cpu) unset_of() {
	c.registers[emi.Flags] &= ^uint64(of)
}
