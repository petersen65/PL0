// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

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
	stackSize          = 16384 // stack entries are 64-bit unsigned integers
	stackForbiddenZone = 1024  // stack entries above this address are forbidden to be used
)

const (
	ip    = register(iota) // instruction pointer is pointing to the next instruction to be executed
	sp                     // stack pointer is pointing to the top of the stack
	bp                     // base pointer is pointing to the base of the current stack frame (descriptor)
	ax                     // accumulator is used for intermediate results of arithmetic operations
	bx                     // base register is used for addressing variables in the current stack frame (descriptor)
	cx                     // counter register is used for counting iterations of loops
	dx                     // data register is used for addressing variables in the previous stack frame (descriptor)
	flags                  // flags register contains the current state of the cpu and reflects the result of arithmetic operations
)

const (
	_  = flag(iota)
	zf = 0x0000000000000040 // zero flag is set if the result of an arithmetic operation is zero
	sf = 0x0000000000000080 // sign flag is set if the result of an arithmetic operation is negative
	of = 0x0000000000000800 // overflow flag is set if the result of an arithmetic operation is too large to fit in the register
)

type (
	register int
	flag     uint64

	process struct {
		text emt.TextSection
	}

	cpu struct {
		registers map[register]uint64
		stack     []uint64
	}

	machine struct {
		cpu cpu
	}
)

func newMachine() *machine {
	return &machine{
		cpu: cpu{
			registers: make(map[register]uint64),
			stack:     make([]uint64, stackSize),
		},
	}
}

func getRegister(memloc emt.MemoryLocation) register {
	switch memloc {
	case emt.M1:
		return ax

	case emt.M2:
		return bx

	case emt.M3:
		return cx

	case emt.M4:
		return dx

	default:
		panic(fmt.Sprintf("panic - invalid memory location '%v'", memloc))
	}
}

// load a program and print it to a writer
func printProgram(sections []byte, print io.Writer) error {
	return (&process{}).dump(sections, print)
}

func (m *machine) runProgram(sections []byte) error {
	var process process

	// load program into memory (text section)
	if err := process.load(sections); err != nil {
		return err
	}

	// state of active callee, which is a running procedure: bp, sp, and ip
	// state of caller is in descriptor of callee (first 3 entries of stack frame)
	// if callee calls another procedure, the callee's state is saved in the stack frame of the new callee (its descriptor)

	// define first caller state: return to first caller exits program
	// first caller is external code that calls the entrypoint of the program (first callee)
	m.cpu.registers[sp] = 0 // stack pointer to top of stack of first caller
	m.cpu.registers[bp] = 0 // base pointer to bottom of stack of first caller
	m.cpu.registers[ip] = 0 // instruction pointer of first caller

	// preserve state of first caller and create descriptor of first callee
	// first callee is the entrypoint of the program
	m.cpu.stack[0] = m.cpu.base(0)  // dynamic link to first callers new callee
	m.cpu.push(m.cpu.registers[bp]) // save first callers base pointer
	m.cpu.push(m.cpu.registers[ip]) // save first callers instruction pointer

	m.cpu.registers[ax] = 0    // accumulator register
	m.cpu.registers[bx] = 0    // base register
	m.cpu.registers[cx] = 0    // counter register
	m.cpu.registers[dx] = 0    // data register
	m.cpu.registers[flags] = 0 // flags register

	// execute instructions until the the first callee returns to the first caller (entrypoint returns to external code)
	for {
		if m.cpu.registers[ip] >= uint64(len(process.text)) {
			return fmt.Errorf("halt - address '%v' out of range", m.cpu.registers[ip])
		}

		if m.cpu.registers[sp] >= stackSize-stackForbiddenZone || m.cpu.registers[sp] < 2 {
			return fmt.Errorf("halt - stack overflow at address '%v'", m.cpu.registers[ip])
		}

		instr := process.text[m.cpu.registers[ip]]
		m.cpu.registers[ip]++

		switch instr.Operation {
		case emt.Lit: // copy int64 constant into a register
			m.cpu.mov(getRegister(instr.MemoryLocation), uint64(instr.Arg1))

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

		case emt.Inc: // allocate space on stack for variables of a procedure
			m.cpu.registers[sp] += uint64(instr.Address)

		case emt.Neg: // negate int64 element within a register
			m.cpu.neg(getRegister(instr.MemoryLocation))

		case emt.Add: // add two register's int64 elements and store the result in the first register
			m.cpu.add(getRegister(instr.MemoryLocation), getRegister(instr.MemoryLocation+1))

		case emt.Sub: // subtract two register's int64 elements and store the result in the first register
			m.cpu.sub(getRegister(instr.MemoryLocation), getRegister(instr.MemoryLocation+1))

		case emt.Mul: // multiply two register's int64 elements and store the result in the first register
			m.cpu.mul(getRegister(instr.MemoryLocation), getRegister(instr.MemoryLocation+1))

		case emt.Div: // divide two register's int64 elements and store the result in the first register
			if int64(m.cpu.registers[getRegister(instr.MemoryLocation+1)]) == 0 {
				return fmt.Errorf("halt - division by zero at address '%v'", m.cpu.registers[ip]-1)
			}

			m.cpu.div(getRegister(instr.MemoryLocation), getRegister(instr.MemoryLocation+1))

		case emt.Odd: // test if register's uint64 element is odd and set zero flag if it is
			m.cpu.and(getRegister(instr.MemoryLocation), 1)

		case emt.Eq: // test if register ax and bx int64 elements are equal and set zero flag if they are
			fallthrough

		case emt.Neq: // test if register ax and bx int64 elements are not equal and clear zero flag if they are
			fallthrough

		case emt.Lss: // test if register bx int64 element is less than register ax int64 element
			fallthrough

		case emt.Leq: // test if register bx int64 element is less than or equal to register ax int64 element
			fallthrough

		case emt.Gtr: // test if register bx int64 element is greater than register ax int64 element
			fallthrough

		case emt.Geq: // test if register bx int64 element is greater than or equal to register ax int64 element
			m.cpu.cmp(ax, bx)

		case emt.Cal: // caller procedure calls callee procedure
			// preserve state of caller and create descriptor of procedure being called
			m.cpu.push(m.cpu.base(instr.Depth)) // dynamic link to procedure being called
			m.cpu.push(m.cpu.registers[bp])     // save callers base pointer
			m.cpu.push(m.cpu.registers[ip])     // save callers instruction pointer

			// base pointer of procedure being called is pointing to its descriptor
			m.cpu.registers[bp] = m.cpu.registers[sp] - 2

			// jump to procedure at uint64 address
			m.cpu.jmp(uint64(instr.Address))

		case emt.Ret: // callee procedure returns to caller procedure
			// returning from the entrypoint of the program exits the program
			if m.cpu.registers[bp] == 0 {
				return nil
			}

			// restore state of caller procdure from descriptor of callee procedure
			m.cpu.registers[sp] = m.cpu.registers[bp] + 2 // discard local variables of callee procedure
			m.cpu.pop(ip)                                 // restore callers instruction pointer
			m.cpu.pop(bp)                                 // restore callers base pointer
			m.cpu.registers[sp] -= 1                      // discard dynamic link and restore callers top of stack

		case emt.Lod: // copy int64 variable into a register loaded from its base plus offset
			m.cpu.mov(getRegister(instr.MemoryLocation), m.cpu.stack[m.cpu.base(instr.Depth)+uint64(instr.Address)+3])

		case emt.Sto: // copy int64 variable from ax register to its base plus offset
			m.cpu.stack[m.cpu.base(instr.Depth)+uint64(instr.Address)+3] = m.cpu.registers[ax]

		case emt.Sys: // system call to operating system based on system call code
			m.cpu.sys(emt.SystemCall(instr.Address))
		}
	}
}

// load text section from a byte slice
func (p *process) load(sections []byte) error {
	p.text = make(emt.TextSection, len(sections)/binary.Size(emt.Instruction{}))

	var buffer bytes.Buffer
	buffer.Write(sections)

	if err := binary.Read(&buffer, binary.LittleEndian, p.text); err != nil {
		return err
	}

	return nil
}

// dump text section to a writer
func (p *process) dump(sections []byte, print io.Writer) error {
	if err := p.load(sections); err != nil {
		return err
	}

	print.Write([]byte(fmt.Sprintf("%-5v %-5v %-5v %-5v %-5v %-5v\n", "text", "op", "mem", "dep", "addr", "arg1")))

	for text, instr := range p.text {
		print.Write([]byte(fmt.Sprintf("%-5v %-5v %-5v %-5v %-5v %-5v\n",
			text,
			emt.OperationNames[instr.Operation],
			instr.MemoryLocation,
			instr.Depth,
			instr.Address,
			instr.Arg1)))
	}

	return nil
}

func (c *cpu) base(depth int32) uint64 {
	b := c.registers[bp]

	for depth > 0 {
		b = c.stack[b]
		depth--
	}

	return b
}

// set zero flag if int64 element is zero
func (c *cpu) set_zf(a int64) {
	if a == 0 {
		c.registers[flags] |= uint64(zf)
	} else {
		c.registers[flags] &= ^uint64(zf)
	}
}

// set sign flag if int64 element is negative
func (c *cpu) set_sf(a int64) {
	if a < 0 {
		c.registers[flags] |= uint64(sf)
	} else {
		c.registers[flags] &= ^uint64(sf)
	}
}

// set overflow flag if negation of the int64 element overflows
func (c *cpu) set_of_neg(a int64) {
	if a == math.MinInt64 {
		c.registers[flags] |= uint64(of)
	} else {
		c.registers[flags] &= ^uint64(of)
	}
}

// set overflow flag if addition of two int64 elements overflows
func (c *cpu) set_of_add(a, b int64) {
	if (a + b) < a {
		c.registers[flags] |= uint64(of)
	} else {
		c.registers[flags] &= ^uint64(of)
	}
}

// set overflow flag if subtraction of two int64 elements overflows
func (c *cpu) set_of_sub(a, b int64) {
	if b > 0 && a < math.MinInt64+b || b < 0 && a > math.MaxInt64+b {
		c.registers[flags] |= uint64(of)
	} else {
		c.registers[flags] &= ^uint64(of)
	}
}

// set overflow flag if multiplication of two int64 elements overflows
func (c *cpu) set_of_mul(a, b int64) {
	if a != 0 && (a*b)/a != b {
		c.registers[flags] |= uint64(of)
	} else {
		c.registers[flags] &= ^uint64(of)
	}
}

// system call to operating system based on system call code
func (c *cpu) sys(code emt.SystemCall) {
	switch code {
	case emt.Read:
		// read integer from stdin
		var input int64

		for {
			fmt.Print("> ")
			_, err := fmt.Scanln(&input)

			if err == nil {
				c.registers[ax] = uint64(input)
				break
			}
		}

	case emt.Write:
		// write integer to stdout
		fmt.Printf("%v\n", int64(c.registers[ax]))
	}
}

// push argument on top of stack, top of stack points to new argument
func (c *cpu) push(arg uint64) {
	c.registers[sp]++
	c.stack[c.registers[sp]] = arg
}

// pop argument from top of stack, top of stack points to previous argument
func (c *cpu) pop(reg register) {
	c.registers[reg] = c.stack[c.registers[sp]]
	c.registers[sp]--
}

// unconditionally jump to uint64 address
func (c *cpu) jmp(addr uint64) {
	c.registers[ip] = addr
}

// copy uint64 argument to register reg
func (c *cpu) mov(reg register, arg uint64) {
	c.registers[reg] = arg
}

// negate reg int64 element
func (c *cpu) neg(reg register) {
	a := -int64(c.registers[reg])
	c.set_zf(a)
	c.set_sf(a)
	c.set_of_neg(a)
	c.registers[reg] = uint64(a)
}

// add two register reg1 and reg2 int64 elements and store the result in register reg1
func (c *cpu) add(reg1 register, reg2 register) {
	a, b := int64(c.registers[reg1]), int64(c.registers[reg2])
	r := a + b
	c.set_zf(r)
	c.set_sf(r)
	c.set_of_add(a, b)
	c.registers[reg1] = uint64(r)
}

// subtract register reg2 from reg1 int64 elements and store the result in register reg1
func (c *cpu) sub(reg1 register, reg2 register) {
	a, b := int64(c.registers[reg1]), int64(c.registers[reg2])
	r := a - b
	c.set_zf(r)
	c.set_sf(r)
	c.set_of_sub(a, b)
	c.registers[reg1] = uint64(r)
}

// multiply register reg1 and reg2 int64 elements and store the result in register reg1
func (c *cpu) mul(reg1 register, reg2 register) {
	a, b := int64(c.registers[reg1]), int64(c.registers[reg2])
	r := a * b
	c.set_zf(r)
	c.set_sf(r)
	c.set_of_mul(a, b)
	c.registers[reg1] = uint64(r)
}

// divide register reg1 by reg2 int64 elements and store the result in reg1
func (c *cpu) div(reg1 register, reg2 register) {
	a, b := int64(c.registers[reg1]), int64(c.registers[reg2])
	r := a / b
	c.set_zf(r)
	c.set_sf(r)
	c.registers[reg1] = uint64(r)
}

// perform bitwise and operation on register reg and uint64 argument and store the result in register reg
func (c *cpu) and(reg register, arg uint64) {
	a := c.registers[reg] & arg
	c.set_zf(int64(a))
	c.set_sf(int64(a))
	c.registers[reg] = a
}

// compare register reg1 and reg2 int64 elements and set flags register based on result (zero zf, sign sf, overflow of)
func (c *cpu) cmp(reg1 register, reg2 register) {
	a, b := int64(c.registers[reg1]), int64(c.registers[reg2])
	c.set_zf(a - b)
	c.set_sf(a - b)
	c.set_of_sub(a, b)
}

// jump to uint64 address if zero flag is set, nz (not zero)
func (c *cpu) je(addr uint64) {
	if c.registers[flags]&uint64(zf) != 0 {
		c.registers[ip] = addr
	}
}

// jump to uint64 address if zero flag is not set, zr (zero)
func (c *cpu) jne(addr uint64) {
	if c.registers[flags]&uint64(zf) == 0 {
		c.registers[ip] = addr
	}
}

// jump to uint64 address if sign flag is not equal to overflow flag (sf != of)
func (c *cpu) jl(addr uint64) {
	if c.registers[flags]&uint64(sf) != c.registers[flags]&uint64(of) {
		c.registers[ip] = addr
	}
}

// jump to uint64 address if zero flag is set and sign flag is not equal to overflow flag (zf != 0, sf != of)
func (c *cpu) jle(addr uint64) {
	if c.registers[flags]&uint64(zf) != 0 && c.registers[flags]&uint64(sf) != c.registers[flags]&uint64(of) {
		c.registers[ip] = addr
	}
}

// jump to uint64 address if zero flag is not set and sign flag is equal to overflow flag (zf == 0, sf == of)
func (c *cpu) jg(addr uint64) {
	if c.registers[flags]&uint64(zf) == 0 && c.registers[flags]&uint64(sf) == c.registers[flags]&uint64(of) {
		c.registers[ip] = addr
	}
}

// jump to uint64 address if sign flag is equal to overflow flag (sf == of)
func (c *cpu) jge(addr uint64) {
	if c.registers[flags]&uint64(sf) == c.registers[flags]&uint64(of) {
		c.registers[ip] = addr
	}
}
