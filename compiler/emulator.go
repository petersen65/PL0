// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

// Package compiler provides functions that compile PL/0 source code into IL/0 intermediate language code. The package also enables the emulation of IL/0 programs.
package compiler

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"io"
	"math"

	emt "github.com/petersen65/PL0/emitter"
)

const (
	stackSize           = 16384 // stack entries are 64-bit unsigned integers
	stackForbiddenZone  = 1024  // stack entries above this address are forbidden to be used
	stackDescriptorSize = 3     // size of a stack frame descriptor
)

const (
	_  = flag(iota)
	zf = 0x0000000000000040 // zero flag is set if the result of an arithmetic operation is zero
	sf = 0x0000000000000080 // sign flag is set if the result of an arithmetic operation is negative
	of = 0x0000000000000800 // overflow flag is set if the result of an arithmetic operation is too large to fit in the register
)

// Emulation core types for process, cpu, and machine.
type (
	flag uint64

	process struct {
		text emt.TextSection
	}

	cpu struct {
		registers map[emt.Destination]uint64
		stack     []uint64
	}

	machine struct {
		cpu cpu
	}
)

// Create a new emulation machine with cpu, registers and stack that can run IL/0 processes.
func newMachine() *machine {
	return &machine{
		cpu: cpu{
			registers: make(map[emt.Destination]uint64),
			stack:     make([]uint64, stackSize),
		},
	}
}

// Load a program and print it to a writer.
func printProgram(sections []byte, print io.Writer) error {
	return (&process{}).dump(sections, print)
}

// Run a program and return an error if the program fails.
func (m *machine) runProgram(sections []byte) error {
	var process process

	// load program into memory (text section)
	if err := process.load(sections); err != nil {
		return err
	}

	// state of active callee, which is a running procedure: bp, sp, and ip
	// state of caller is in descriptor of callee (first 3 entries of stack frame)
	// if callee calls another procedure, the callee's state is saved in the stack frame of the new callee (its descriptor)

	m.cpu.registers[emt.Ax] = 0    // accumulator register
	m.cpu.registers[emt.Bx] = 0    // base register
	m.cpu.registers[emt.Cx] = 0    // counter register
	m.cpu.registers[emt.Dx] = 0    // data register
	m.cpu.registers[emt.Flags] = 0 // flags register
	m.cpu.registers[emt.Sp] = 0    // stack pointer to top of stack
	m.cpu.registers[emt.Bp] = 0    // base pointer to bottom of stack frame
	m.cpu.registers[emt.Ip] = 0    // instruction pointer

	// preserve state of first caller and create descriptor of first callee
	// first callee is the entrypoint of the program
	m.cpu.push(m.cpu.registers[emt.Ip])               // return address (instruction pointer of caller + 1)
	m.cpu.push(m.cpu.registers[emt.Bp])               // dynamic link chains base pointers so that each callee knows the base pointer of its caller
	m.cpu.stack[0] = m.cpu.link(0)                    // static link
	m.cpu.registers[emt.Bp] = m.cpu.registers[emt.Sp] // base pointer of callee is pointing to the end of its descriptor

	// execute instructions until the the first callee returns to the first caller (entrypoint returns to external code)
	for {
		if m.cpu.registers[emt.Ip] >= uint64(len(process.text)) {
			return fmt.Errorf("halt - address '%v' out of range", m.cpu.registers[emt.Ip])
		}

		if m.cpu.registers[emt.Sp] >= stackSize-stackForbiddenZone || m.cpu.registers[emt.Sp] < (stackDescriptorSize-1) {
			return fmt.Errorf("halt - stack overflow at address '%v'", m.cpu.registers[emt.Ip])
		}

		instr := process.text[m.cpu.registers[emt.Ip]]
		m.cpu.registers[emt.Ip]++

		switch instr.Operation {
		case emt.Movr: // copy int64 constant into a register
			m.cpu.movr(instr.Destination, uint64(instr.Arg1))

		case emt.Movs: // copy int64 constant into the stack
			m.cpu.movs(instr.Destination, uint64(instr.Arg1))

		case emt.Jmp: // unconditionally jump to uint64 address
			m.cpu.jmp(uint64(instr.Address))

		case emt.Je: // jump to uint64 address if last comparison was equal
			m.cpu.je(uint64(instr.Address))

		case emt.Jne: // jump to uint64 address if last comparison was not equal
			m.cpu.jne(uint64(instr.Address))

		case emt.Jl: // jump to uint64 address if last comparison was less than
			m.cpu.jl(uint64(instr.Address))

		case emt.Jle: // jump to uint64 address if last comparison was less than or equal to
			m.cpu.jle(uint64(instr.Address))

		case emt.Jg: // jump to uint64 address if last comparison was greater than
			m.cpu.jg(uint64(instr.Address))

		case emt.Jge: // jump to uint64 address if last comparison was greater than or equal to
			m.cpu.jge(uint64(instr.Address))

		case emt.Alc: // allocate stack space for variables and memory locations required by expressions
			varOffset := uint64(instr.Address)
			mlocOffset := instr.Arg1

			if mlocOffset > emt.FreeCpuRegisters {
				mlocOffset -= emt.FreeCpuRegisters
			} else {
				mlocOffset = 0
			}

			m.cpu.registers[emt.Sp] += varOffset + uint64(mlocOffset)

		case emt.Neg: // negate int64 element within stack or register
			m.cpu.neg(m.cpu.mloc(instr.MemoryLocation))

		case emt.Add: // add two int64 elements and store the result in the first stack or register
			reg1, ptr1 := m.cpu.mloc(instr.MemoryLocation)
			reg2, ptr2 := m.cpu.mloc(instr.MemoryLocation + 1)
			m.cpu.add(reg1, ptr1, reg2, ptr2)

		case emt.Sub: // subtract two int64 elements and store the result in the first stack or register
			reg1, ptr1 := m.cpu.mloc(instr.MemoryLocation)
			reg2, ptr2 := m.cpu.mloc(instr.MemoryLocation + 1)
			m.cpu.sub(reg1, ptr1, reg2, ptr2)

		case emt.Mul: // multiply two int64 elements and store the result in the first stack or register
			reg1, ptr1 := m.cpu.mloc(instr.MemoryLocation)
			reg2, ptr2 := m.cpu.mloc(instr.MemoryLocation + 1)
			m.cpu.mul(reg1, ptr1, reg2, ptr2)

		case emt.Div: // divide two int64 elements and store the result in the first stack or register
			var b int64
			reg1, ptr1 := m.cpu.mloc(instr.MemoryLocation)
			reg2, ptr2 := m.cpu.mloc(instr.MemoryLocation + 1)

			if reg2 == emt.Sp {
				b = int64(m.cpu.stack[ptr2])
			} else {
				b = int64(m.cpu.registers[reg2])
			}

			if b == 0 {
				return fmt.Errorf("halt - division by zero at address '%v'", m.cpu.registers[emt.Ip]-1)
			}

			m.cpu.div(reg1, ptr1, reg2, ptr2)

		case emt.And: // test if stack's or register's uint64 element is odd and set zero flag if it is
			reg, ptr := m.cpu.mloc(instr.MemoryLocation)
			m.cpu.and(reg, ptr, 1)

		case emt.Eq: // test if two int64 elements are equal and set zero flag if they are
			fallthrough

		case emt.Neq: // test if two int64 elements are not equal and clear zero flag if they are
			fallthrough

		case emt.Lss: // test if second int64 element is less than first int64 element and store result in flags register
			fallthrough

		case emt.Leq: // test if second int64 element is less than or equal to first int64 element and store result in flags register
			fallthrough

		case emt.Gtr: // test if second int64 element is greater than first int64 element and store result in flags register
			fallthrough

		case emt.Geq: // test if second int64 element is greater than or equal to first int64 element and store result in flags register
			reg1, ptr1 := m.cpu.mloc(instr.MemoryLocation)
			reg2, ptr2 := m.cpu.mloc(instr.MemoryLocation + 1)
			m.cpu.cmp(reg1, ptr1, reg2, ptr2)

		case emt.Cal: // caller procedure calls callee procedure
			// create descriptor of procedure being called and preserve state of caller in it
			m.cpu.push(m.cpu.registers[emt.Ip])                      // return address
			m.cpu.push(m.cpu.registers[emt.Bp])                      // dynamic link
			m.cpu.push(m.cpu.link(instr.DeclarationDepthDifference)) // static link

			// base pointer of procedure being called is pointing to the end of its descriptor
			m.cpu.registers[emt.Bp] = m.cpu.registers[emt.Sp]

			// jump to procedure at uint64 address
			m.cpu.jmp(uint64(instr.Address))

		case emt.Ret: // callee procedure returns to caller procedure
			// restore state of caller procdure from descriptor of callee procedure
			m.cpu.registers[emt.Sp] = m.cpu.registers[emt.Bp] - 1 // discard stack space and static link of callee procedure
			m.cpu.pop(emt.Bp)                                     // restore callers base pointer
			m.cpu.pop(emt.Ip)                                     // restore callers instruction pointer

			// returning from the entrypoint of the program exits the program
			if m.cpu.registers[emt.Ip] == 0 {
				return nil
			}

		case emt.Mlv: // copy int64 variable loaded from its base plus offset to stack or register
			reg, ptr := m.cpu.mloc(instr.MemoryLocation)
			variablesBase := m.cpu.link(instr.DeclarationDepthDifference) + 1     // base pointer + 1
			m.cpu.mov(reg, ptr, m.cpu.stack[variablesBase+uint64(instr.Address)]) // variables base + variable offset

		case emt.Msv: // copy int64 element from stack or register to a variable stored within its base plus offset
			var a uint64

			if reg, ptr := m.cpu.mloc(instr.MemoryLocation); reg == emt.Sp {
				a = m.cpu.stack[ptr]
			} else {
				a = m.cpu.registers[reg]
			}

			variablesBase := m.cpu.link(instr.DeclarationDepthDifference) + 1 // base pointer + 1
			m.cpu.mov(emt.Sp, variablesBase+uint64(instr.Address), a)             // variables base + variable offset

		case emt.Sys: // system call to operating system based on system call code
			reg, ptr := m.cpu.mloc(instr.MemoryLocation)
			m.cpu.sys(emt.SystemCall(instr.Address), reg, ptr)

		default:
			return fmt.Errorf("halt - unknown operation '%v' at address '%v'", instr.Operation, m.cpu.registers[emt.Ip]-1)
		}
	}
}

// Load sections from a byte slice into the text section of a process.
func (p *process) load(sections []byte) error {
	p.text = make(emt.TextSection, len(sections)/binary.Size(emt.Instruction{}))

	var buffer bytes.Buffer
	buffer.Write(sections)

	if err := binary.Read(&buffer, binary.LittleEndian, p.text); err != nil {
		return err
	}

	return nil
}

// Dump sections loaded by a process to a writer.
func (p *process) dump(sections []byte, print io.Writer) error {
	if err := p.load(sections); err != nil {
		return err
	}

	print.Write([]byte(fmt.Sprintf("%-5v %-5v %-5v %-5v %-5v %-5v\n", "text", "op", "mloc", "dep", "addr", "arg1")))

	for text, instr := range p.text {
		print.Write([]byte(fmt.Sprintf("%-5v %-5v %-5v %-5v %-5v %-5v\n",
			text,
			emt.OperationNames[instr.Operation],
			instr.MemoryLocation,
			instr.DeclarationDepthDifference,
			instr.Address,
			instr.Arg1)))
	}

	return nil
}

// Follow static link to declaration depth of target variable.
func (c *cpu) link(depth int32) uint64 {
	basePointer := c.registers[emt.Bp]

	for ; depth > 0; depth-- {
		basePointer = c.stack[basePointer]
	}

	return basePointer
}

// Map a memory location to a register or to a location on the stack.
func (c *cpu) mloc(memloc int32) (emt.Destination, uint64) {
	switch memloc {
	case 0:
		return emt.Ax, 0

	case 1:
		return emt.Bx, 0

	case 2:
		return emt.Cx, 0

	case 3:
		return emt.Dx, 0

	default:
		// memory locations are allocated downwards from the top of the stack including top of stack
		return emt.Sp, c.registers[emt.Sp] - uint64(memloc-emt.FreeCpuRegisters)
	}
}

// Set zero flag if int64 element is zero.
func (c *cpu) set_zf(a int64) {
	if a == 0 {
		c.registers[emt.Flags] |= uint64(zf)
	} else {
		c.registers[emt.Flags] &= ^uint64(zf)
	}
}

// Set sign flag if int64 element is negative.
func (c *cpu) set_sf(a int64) {
	if a < 0 {
		c.registers[emt.Flags] |= uint64(sf)
	} else {
		c.registers[emt.Flags] &= ^uint64(sf)
	}
}

// Set overflow flag if negation of the int64 element overflows.
func (c *cpu) set_of_neg(a int64) {
	if a == math.MinInt64 {
		c.registers[emt.Flags] |= uint64(of)
	} else {
		c.registers[emt.Flags] &= ^uint64(of)
	}
}

// Set overflow flag if addition of two int64 elements overflows.
func (c *cpu) set_of_add(a, b int64) {
	if (a + b) < a {
		c.registers[emt.Flags] |= uint64(of)
	} else {
		c.registers[emt.Flags] &= ^uint64(of)
	}
}

// Set overflow flag if subtraction of two int64 elements overflows.
func (c *cpu) set_of_sub(a, b int64) {
	if b > 0 && a < math.MinInt64+b || b < 0 && a > math.MaxInt64+b {
		c.registers[emt.Flags] |= uint64(of)
	} else {
		c.registers[emt.Flags] &= ^uint64(of)
	}
}

// Set overflow flag if multiplication of two int64 elements overflows.
func (c *cpu) set_of_mul(a, b int64) {
	if a != 0 && (a*b)/a != b {
		c.registers[emt.Flags] |= uint64(of)
	} else {
		c.registers[emt.Flags] &= ^uint64(of)
	}
}

// Set overflow flag if division of two int64 elements overflows.
func (c *cpu) set_of_div(a, b int64) {
	if b == -1 && a == math.MinInt64 {
		c.registers[emt.Flags] |= uint64(of)
	} else {
		c.registers[emt.Flags] &= ^uint64(of)
	}
}

// Clear overflow flag.
func (c *cpu) unset_of() {
	c.registers[emt.Flags] &= ^uint64(of)
}

// System call to operating system based on system call code.
func (c *cpu) sys(code emt.SystemCall, reg emt.Destination, ptr uint64) {
	switch code {
	case emt.Read:
		// read integer from stdin
		var input int64

		for {
			fmt.Print("> ")
			_, err := fmt.Scanln(&input)

			if err == nil {
				if reg == emt.Sp {
					c.stack[ptr] = uint64(input)
				} else {
					c.registers[reg] = uint64(input)
				}
				break
			}
		}

	case emt.Write:
		// write integer to stdout
		if reg == emt.Sp {
			fmt.Printf("%v\n", int64(c.stack[ptr]))
		} else {
			fmt.Printf("%v\n", int64(c.registers[reg]))
		}
	}
}

// Push argument on top of stack, top of stack points to new argument.
func (c *cpu) push(arg uint64) {
	c.registers[emt.Sp]++
	c.stack[c.registers[emt.Sp]] = arg
}

// Pop argument from top of stack, top of stack points to previous argument.
func (c *cpu) pop(reg emt.Destination) {
	c.registers[reg] = c.stack[c.registers[emt.Sp]]

	if c.registers[emt.Sp] > 0 {
		c.registers[emt.Sp]--
	}
}

// Unconditionally jump to uint64 address.
func (c *cpu) jmp(addr uint64) {
	c.registers[emt.Ip] = addr
}

// Copy uint64 argument to stack at ptr address or to register reg.
func (c *cpu) mov(reg emt.Destination, ptr, arg uint64) {
	if reg == emt.Sp {
		c.stack[ptr] = arg
	} else {
		c.registers[reg] = arg
	}
}

// Copy uint64 argument to register reg.
func (c *cpu) movr(reg emt.Destination, arg uint64) {
	c.registers[reg] = uint64(arg)
}

// Copy uint64 argument to top of stack minus offset.
func (c *cpu) movs(ofs emt.Destination, arg uint64) {
	c.stack[c.registers[emt.Sp] - uint64(ofs)] = arg
}

// Negate stack or register int64 element.
func (c *cpu) neg(reg emt.Destination, ptr uint64) {
	var a int64

	if reg == emt.Sp {
		a = -int64(c.stack[ptr])
	} else {
		a = -int64(c.registers[reg])
	}

	c.set_zf(a)
	c.set_sf(a)
	c.set_of_neg(a)

	if reg == emt.Sp {
		c.stack[ptr] = uint64(a)
	} else {
		c.registers[reg] = uint64(a)
	}
}

// Add two int64 elements and store the result in first stack or register int64 element.
func (c *cpu) add(reg1 emt.Destination, ptr1 uint64, reg2 emt.Destination, ptr2 uint64) {
	var a, b int64

	if reg1 == emt.Sp {
		a = int64(c.stack[ptr1])
	} else {
		a = int64(c.registers[reg1])
	}

	if reg2 == emt.Sp {
		b = int64(c.stack[ptr2])
	} else {
		b = int64(c.registers[reg2])
	}

	r := a + b
	c.set_zf(r)
	c.set_sf(r)
	c.set_of_add(a, b)

	if reg1 == emt.Sp {
		c.stack[ptr1] = uint64(r)
	} else {
		c.registers[reg1] = uint64(r)
	}
}

// Subtract two int64 elements and store the result in first stack or register int64 element.
func (c *cpu) sub(reg1 emt.Destination, ptr1 uint64, reg2 emt.Destination, ptr2 uint64) {
	var a, b int64

	if reg1 == emt.Sp {
		a = int64(c.stack[ptr1])
	} else {
		a = int64(c.registers[reg1])
	}

	if reg2 == emt.Sp {
		b = int64(c.stack[ptr2])
	} else {
		b = int64(c.registers[reg2])
	}

	r := a - b
	c.set_zf(r)
	c.set_sf(r)
	c.set_of_sub(a, b)

	if reg1 == emt.Sp {
		c.stack[ptr1] = uint64(r)
	} else {
		c.registers[reg1] = uint64(r)
	}
}

// Multiply two int64 elements and store the result in first stack or register int64 element.
func (c *cpu) mul(reg1 emt.Destination, ptr1 uint64, reg2 emt.Destination, ptr2 uint64) {
	var a, b int64

	if reg1 == emt.Sp {
		a = int64(c.stack[ptr1])
	} else {
		a = int64(c.registers[reg1])
	}

	if reg2 == emt.Sp {
		b = int64(c.stack[ptr2])
	} else {
		b = int64(c.registers[reg2])
	}

	r := a * b
	c.set_zf(r)
	c.set_sf(r)
	c.set_of_mul(a, b)

	if reg1 == emt.Sp {
		c.stack[ptr1] = uint64(r)
	} else {
		c.registers[reg1] = uint64(r)
	}
}

// Divide two int64 elements and store the result in first stack or register int64 element.
func (c *cpu) div(reg1 emt.Destination, ptr1 uint64, reg2 emt.Destination, ptr2 uint64) {
	var a, b int64

	if reg1 == emt.Sp {
		a = int64(c.stack[ptr1])
	} else {
		a = int64(c.registers[reg1])
	}

	if reg2 == emt.Sp {
		b = int64(c.stack[ptr2])
	} else {
		b = int64(c.registers[reg2])
	}

	r := a / b
	c.set_zf(r)
	c.set_sf(r)
	c.set_of_div(a, b)

	if reg1 == emt.Sp {
		c.stack[ptr1] = uint64(r)
	} else {
		c.registers[reg1] = uint64(r)
	}
}

// Perform bitwise 'and' operation with uint64 argument and store the result in stack or register.
func (c *cpu) and(reg emt.Destination, ptr, arg uint64) {
	var a uint64

	if reg == emt.Sp {
		a = c.stack[ptr] & arg
	} else {
		a = c.registers[reg] & arg
	}

	c.set_zf(int64(a))
	c.set_sf(int64(a))
	c.unset_of()

	if reg == emt.Sp {
		c.stack[ptr] = a
	} else {
		c.registers[reg] = a
	}
}

// Compare two int64 elements and set flags register based on result (zero zf, sign sf, overflow of).
func (c *cpu) cmp(reg1 emt.Destination, ptr1 uint64, reg2 emt.Destination, ptr2 uint64) {
	var a, b int64

	if reg1 == emt.Sp {
		a = int64(c.stack[ptr1])
	} else {
		a = int64(c.registers[reg1])
	}

	if reg2 == emt.Sp {
		b = int64(c.stack[ptr2])
	} else {
		b = int64(c.registers[reg2])
	}

	c.set_zf(a - b)
	c.set_sf(a - b)
	c.set_of_sub(a, b)
}

// Jump to uint64 address if zero flag is set, nz (not zero).
func (c *cpu) je(addr uint64) {
	if c.registers[emt.Flags]&uint64(zf) != 0 {
		c.registers[emt.Ip] = addr
	}
}

// Jump to uint64 address if zero flag is not set, zr (zero).
func (c *cpu) jne(addr uint64) {
	if c.registers[emt.Flags]&uint64(zf) == 0 {
		c.registers[emt.Ip] = addr
	}
}

// Jump to uint64 address if sign flag is not equal to overflow flag (sf != of).
func (c *cpu) jl(addr uint64) {
	if c.registers[emt.Flags]&uint64(sf) != c.registers[emt.Flags]&uint64(of) {
		c.registers[emt.Ip] = addr
	}
}

// Jump to uint64 address if zero flag is set and sign flag is not equal to overflow flag (zf != 0, sf != of).
func (c *cpu) jle(addr uint64) {
	if c.registers[emt.Flags]&uint64(zf) != 0 || c.registers[emt.Flags]&uint64(sf) != c.registers[emt.Flags]&uint64(of) {
		c.registers[emt.Ip] = addr
	}
}

// Jump to uint64 address if zero flag is not set and sign flag is equal to overflow flag (zf == 0, sf == of).
func (c *cpu) jg(addr uint64) {
	if c.registers[emt.Flags]&uint64(zf) == 0 && c.registers[emt.Flags]&uint64(sf) == c.registers[emt.Flags]&uint64(of) {
		c.registers[emt.Ip] = addr
	}
}

// Jump to uint64 address if sign flag is equal to overflow flag (sf == of).
func (c *cpu) jge(addr uint64) {
	if c.registers[emt.Flags]&uint64(sf) == c.registers[emt.Flags]&uint64(of) {
		c.registers[emt.Ip] = addr
	}
}
