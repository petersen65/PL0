// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emulator

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
	ax    = register(iota) // accumulator is used for intermediate results of arithmetic operations
	bx                     // base register can be used for addressing variables
	cx                     // counter register can be used for counting iterations of loops
	dx                     // data register can be used for addressing variables
	flags                  // flags register contains the current state of the cpu and reflects the result of arithmetic operations
	ip                     // instruction pointer is pointing to the next instruction to be executed
	sp                     // stack pointer is pointing to the top of the stack
	bp                     // base pointer is pointing to the base of the current stack frame
)

const (
	_  = flag(iota)
	zf = 0x0000000000000040 // zero flag is set if the result of an arithmetic operation is zero
	sf = 0x0000000000000080 // sign flag is set if the result of an arithmetic operation is negative
	of = 0x0000000000000800 // overflow flag is set if the result of an arithmetic operation is too large to fit in the register
)

// Emulation core types for process, cpu, and machine.
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

// Create a new emulation machine with cpu, registers and stack that can run IL/0 processes.
func newMachine() *machine {
	return &machine{
		cpu: cpu{
			registers: make(map[register]uint64),
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

	m.cpu.registers[ax] = 0                       // accumulator register
	m.cpu.registers[bx] = 0                       // base register
	m.cpu.registers[cx] = 0                       // counter register
	m.cpu.registers[dx] = 0                       // data register
	m.cpu.registers[flags] = 0                    // flags register
	m.cpu.registers[ip] = 0                       // instruction pointer
	m.cpu.registers[sp] = stackDescriptorSize - 1 // stack pointer to top of stack (end of stack frame descriptor)
	m.cpu.registers[bp] = 0                       // base pointer to bottom of stack frame

	// preserve state of first caller and create descriptor of first callee
	// first callee is the entrypoint of the program
	m.cpu.stack[0] = m.cpu.registers[ip]      // return address (instruction pointer of caller + 1)
	m.cpu.stack[1] = m.cpu.registers[bp]      // dynamic link chains base pointers so that each callee knows the base pointer of its caller
	m.cpu.stack[2] = m.cpu.parent(0)          // static link points to direct parent block in declaration nesting hierarchy
	m.cpu.registers[bp] = m.cpu.registers[sp] // base pointer of callee is pointing to the end of its descriptor

	// execute instructions until the the first callee returns to the first caller (entrypoint returns to external code)
	for {
		if m.cpu.registers[ip] >= uint64(len(process.text)) {
			return fmt.Errorf("halt - address '%v' out of range", m.cpu.registers[ip])
		}

		if m.cpu.registers[sp] >= stackSize-stackForbiddenZone || m.cpu.registers[sp] < (stackDescriptorSize-1) {
			return fmt.Errorf("halt - stack overflow at address '%v'", m.cpu.registers[ip])
		}

		instr := process.text[m.cpu.registers[ip]]
		m.cpu.registers[ip]++

		switch instr.Operation {
		case emt.Ldc: // copy int64 constant onto the stack
			m.cpu.push(uint64(instr.ArgInt))

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

		case emt.Alc: // allocate stack space for variables
			m.cpu.registers[sp] += uint64(instr.Address)

		case emt.Neg: // negate int64 element on top of stack
			if err := m.cpu.neg(); err != nil {
				return err
			}

		case emt.And: // test if top of stack's uint64 element is odd and set zero flag if it is
			m.cpu.and(1)

		case emt.Add: // add two int64 elements from top of stack and store the result onto the stack
			if err := m.cpu.add(); err != nil {
				return err
			}

		case emt.Sub: // subtract two int64 elements from top of stack and store the result onto the stack
			if err := m.cpu.sub(); err != nil {
				return err
			}

		case emt.Mul: // multiply two int64 elements from top of stack and store the result onto the stack
			if err := m.cpu.mul(); err != nil {
				return err
			}

		case emt.Div: // divide two int64 elements from top of stack and store the result onto the stack
			if err := m.cpu.div(); err != nil {
				return err
			}

		case emt.Eq: // test if two int64 elements from top of stack are equal and set zero flag if they are
			fallthrough

		case emt.Neq: // test if two int64 elements from top of stack are not equal and clear zero flag if they are
			fallthrough

		case emt.Lss: // test if the int64 element from top of stack is less than the int64 element from top of stack minus one and store result in flags register
			fallthrough

		case emt.Leq: // test if the int64 element from top of stack is less than or equal to the int64 element from top of stack minus one and store result in flags register
			fallthrough

		case emt.Gtr: // test if the int64 element from top of stack is greater than the int64 element from top of stack minus one and store result in flags register
			fallthrough

		case emt.Geq: // test if the int64 element from top of stack is greater than or equal to the int64 element from top of stack minus one and store result in flags register
			m.cpu.cmp()

		case emt.Cal: // caller procedure calls callee procedure
			// create descriptor of procedure being called and preserve state of caller in it
			m.cpu.push(m.cpu.registers[ip])                            // return address
			m.cpu.push(m.cpu.registers[bp])                            // dynamic link
			m.cpu.push(m.cpu.parent(instr.DeclarationDepthDifference)) // static link

			// base pointer of procedure being called is pointing to the end of its descriptor
			m.cpu.registers[bp] = m.cpu.registers[sp]

			// jump to procedure at uint64 address
			m.cpu.jmp(uint64(instr.Address))

		case emt.Ret: // callee procedure returns to caller procedure
			// restore state of caller procdure from descriptor of callee procedure
			m.cpu.registers[sp] = m.cpu.registers[bp] - 1 // discard stack space and static link
			m.cpu.pop(bp)                                 // restore callers base pointer
			m.cpu.pop(ip)                                 // restore callers instruction pointer

			// returning from the entrypoint of the program exits the program
			if m.cpu.registers[ip] == 0 {
				return nil
			}

		case emt.Ldv: // copy int64 variable loaded from its base plus offset onto the stack
			variablesBase := m.cpu.parent(instr.DeclarationDepthDifference) + 1 // base pointer + 1
			m.cpu.push(m.cpu.stack[variablesBase+uint64(instr.Address)])        // variables base + variable offset

		case emt.Stv: // copy int64 element from top of stack to a variable stored within its base plus offset
			variablesBase := m.cpu.parent(instr.DeclarationDepthDifference) + 1    // base pointer + 1
			m.cpu.pop(ax)                                                          // int64 element to be stored in variable
			m.cpu.stack[variablesBase+uint64(instr.Address)] = m.cpu.registers[ax] // variables base + variable offset

		case emt.Sys: // system call to operating system based on system call code
			m.cpu.sys(emt.SystemCall(instr.Address))

		default:
			return fmt.Errorf("halt - unknown operation '%v' at address '%v'", instr.Operation, m.cpu.registers[ip]-1)
		}
	}
}

// Follow static link to parent blocks in compile-time declaration nesting hierarchy.
func (c *cpu) parent(difference int32) uint64 {
	basePointer := c.registers[bp]

	for i := int32(0); i < difference; i++ {
		basePointer = c.stack[basePointer]
	}

	return basePointer
}

// Push argument on top of stack, top of stack points to new argument.
func (c *cpu) push(arg uint64) {
	c.registers[sp]++
	c.stack[c.registers[sp]] = arg
}

// Pop argument from top of stack, top of stack points to previous argument.
func (c *cpu) pop(reg register) {
	c.registers[reg] = c.stack[c.registers[sp]]

	if c.registers[sp] > 0 {
		c.registers[sp]--
	}
}

// Negate int64 element from top of stack.
func (c *cpu) neg() error {
	c.pop(ax)
	c.set_of_neg(ax)

	if c.test_of() {
		return fmt.Errorf("halt - arithmetic overflow for negation at address '%v'", c.registers[ip]-1)
	}

	c.registers[ax] = uint64(-int64(c.registers[ax]))
	c.set_zf(ax)
	c.set_sf(ax)

	c.push(c.registers[ax])
	return nil
}

// Perform bitwise 'and' operation with uint64 element from top of stack and uint64 argument.
func (c *cpu) and(arg uint64) {
	c.pop(ax)
	c.registers[ax] = c.registers[ax] & arg

	c.set_zf(ax)
	c.set_sf(ax)
	c.unset_of()

	c.push(c.registers[ax])
}

// Add two int64 elements from top of stack and store the result onto the stack.
func (c *cpu) add() error {
	c.pop(bx)
	c.pop(ax)
	c.set_of_add(ax, bx)

	if c.test_of() {
		return fmt.Errorf("halt - arithmetic overflow for addition at address '%v'", c.registers[ip]-1)
	}

	c.registers[ax] = uint64(int64(c.registers[ax]) + int64(c.registers[bx]))
	c.set_zf(ax)
	c.set_sf(ax)

	c.push(c.registers[ax])
	return nil
}

// Subtract two int64 elements from top of stack and store the result onto the stack.
func (c *cpu) sub() error {
	c.pop(bx)
	c.pop(ax)
	c.set_of_sub(ax, bx)

	if c.test_of() {
		return fmt.Errorf("halt - arithmetic overflow for subtraction at address '%v'", c.registers[ip]-1)
	}

	c.registers[ax] = uint64(int64(c.registers[ax]) - int64(c.registers[bx]))
	c.set_zf(ax)
	c.set_sf(ax)

	c.push(c.registers[ax])
	return nil
}

// Multiply two int64 elements from top of stack and store the result onto the stack.
func (c *cpu) mul() error {
	c.pop(bx)
	c.pop(ax)
	c.set_of_mul(ax, bx)

	if c.test_of() {
		return fmt.Errorf("halt - arithmetic overflow for multiplication at address '%v'", c.registers[ip]-1)
	}

	c.registers[ax] = uint64(int64(c.registers[ax]) * int64(c.registers[bx]))
	c.set_zf(ax)
	c.set_sf(ax)

	c.push(c.registers[ax])
	return nil
}

// Divide two int64 elements from top of stack and store the result onto the stack.
func (c *cpu) div() error {
	c.pop(bx)
	c.pop(ax)
	c.set_of_div(ax, bx)

	if c.registers[bx] == 0 {
		return fmt.Errorf("halt - division by zero at address '%v'", c.registers[ip]-1)
	}

	if c.test_of() {
		return fmt.Errorf("halt - arithmetic overflow for division at address '%v'", c.registers[ip]-1)
	}

	c.registers[ax] = uint64(int64(c.registers[ax]) / int64(c.registers[bx]))
	c.set_zf(ax)
	c.set_sf(ax)

	c.push(c.registers[ax])
	return nil
}

// Compare two int64 elements from top of stack and set flags register based on result (zero zf, sign sf, overflow of).
func (c *cpu) cmp() {
	c.pop(bx)
	c.pop(ax)
	c.set_of_sub(ax, bx)

	c.registers[ax] = uint64(int64(c.registers[ax]) - int64(c.registers[bx]))
	c.set_zf(ax)
	c.set_sf(ax)
}

// Unconditionally jump to uint64 address.
func (c *cpu) jmp(addr uint64) {
	c.registers[ip] = addr
}

// Jump to uint64 address if zero flag is set, nz (not zero).
func (c *cpu) je(addr uint64) {
	if c.registers[flags]&uint64(zf) != 0 {
		c.registers[ip] = addr
	}
}

// Jump to uint64 address if zero flag is not set, zr (zero).
func (c *cpu) jne(addr uint64) {
	if c.registers[flags]&uint64(zf) == 0 {
		c.registers[ip] = addr
	}
}

// Jump to uint64 address if sign flag is not equal to overflow flag (sf != of).
func (c *cpu) jl(addr uint64) {
	if c.registers[flags]&uint64(sf) != c.registers[flags]&uint64(of) {
		c.registers[ip] = addr
	}
}

// Jump to uint64 address if zero flag is set and sign flag is not equal to overflow flag (zf != 0, sf != of).
func (c *cpu) jle(addr uint64) {
	if c.registers[flags]&uint64(zf) != 0 || c.registers[flags]&uint64(sf) != c.registers[flags]&uint64(of) {
		c.registers[ip] = addr
	}
}

// Jump to uint64 address if zero flag is not set and sign flag is equal to overflow flag (zf == 0, sf == of).
func (c *cpu) jg(addr uint64) {
	if c.registers[flags]&uint64(zf) == 0 && c.registers[flags]&uint64(sf) == c.registers[flags]&uint64(of) {
		c.registers[ip] = addr
	}
}

// Jump to uint64 address if sign flag is equal to overflow flag (sf == of).
func (c *cpu) jge(addr uint64) {
	if c.registers[flags]&uint64(sf) == c.registers[flags]&uint64(of) {
		c.registers[ip] = addr
	}
}

// Set zero flag if int64 element in register reg is zero.
func (c *cpu) set_zf(reg register) {
	if int64(c.registers[reg]) == 0 {
		c.registers[flags] |= uint64(zf)
	} else {
		c.registers[flags] &= ^uint64(zf)
	}
}

// Set sign flag if int64 element in register reg is negative.
func (c *cpu) set_sf(reg register) {
	if int64(c.registers[reg]) < 0 {
		c.registers[flags] |= uint64(sf)
	} else {
		c.registers[flags] &= ^uint64(sf)
	}
}

// Set overflow flag if negation of the int64 element in register reg overflows.
func (c *cpu) set_of_neg(reg register) {
	if int64(c.registers[reg]) == math.MinInt64 {
		c.registers[flags] |= uint64(of)
	} else {
		c.registers[flags] &= ^uint64(of)
	}
}

// Set overflow flag if addition of two int64 elements in registers reg1/reg2 overflows.
func (c *cpu) set_of_add(reg1, reg2 register) {
	a := int64(c.registers[reg1])
	b := int64(c.registers[reg2])
	s := a + b

	if (a > 0 && b > 0 && s < a) || (a < 0 && b < 0 && s > a) {
		c.registers[flags] |= uint64(of)
	} else if (a > 0 && b < 0 && s > a) || (a < 0 && b > 0 && s < a) {
		c.registers[flags] |= uint64(of)
	} else {
		c.registers[flags] &= ^uint64(of)
	}
}

// Set overflow flag if subtraction of two int64 elements in registers reg1/reg2 overflows.
func (c *cpu) set_of_sub(reg1, reg2 register) {
	a := int64(c.registers[reg1])
	b := int64(c.registers[reg2])
	s := a - b

	if (a > 0 && b < 0 && s < a) || (a < 0 && b > 0 && s > a) {
		c.registers[flags] |= uint64(of)
	} else if (a > 0 && b > 0 && s > a) || (a < 0 && b < 0 && s < a) {
		c.registers[flags] |= uint64(of)
	} else {
		c.registers[flags] &= ^uint64(of)
	}
}

// Set overflow flag if multiplication of two int64 elements in registers reg1/reg2 overflows.
func (c *cpu) set_of_mul(reg1, reg2 register) {
	a := int64(c.registers[reg1])
	b := int64(c.registers[reg2])
	s := a * b

	if a != 0 && s/a != b {
		c.registers[flags] |= uint64(of)
	} else {
		c.registers[flags] &= ^uint64(of)
	}
}

// Set overflow flag if division of two int64 elements in registers reg1/reg2 overflows.
func (c *cpu) set_of_div(reg1, reg2 register) {
	a := int64(c.registers[reg1])
	b := int64(c.registers[reg2])

	if b == -1 && a == math.MinInt64 {
		c.registers[flags] |= uint64(of)
	} else {
		c.registers[flags] &= ^uint64(of)
	}
}

// Test overflow flag
func (c *cpu) test_of() bool {
	return c.registers[flags]&uint64(of) != 0
}

// Clear overflow flag.
func (c *cpu) unset_of() {
	c.registers[flags] &= ^uint64(of)
}

// System call to operating system based on system call code.
func (c *cpu) sys(code emt.SystemCall) {
	switch code {
	case emt.Read:
		// read integer from stdin
		var input int64

		for {
			fmt.Print("> ")
			_, err := fmt.Scanln(&input)

			if err == nil {
				c.push(uint64(input))
				break
			}
		}

	case emt.Write:
		// write integer to stdout
		c.pop(ax)
		fmt.Printf("%v\n", int64(c.registers[ax]))
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

	print.Write([]byte(fmt.Sprintf("%-5v %-5v %-5v %-5v %-5v\n", "text", "op", "dep", "adr", "ai")))

	for text, instr := range p.text {
		print.Write([]byte(fmt.Sprintf("%-5v %-5v %-5v %-5v %-5v\n",
			text,
			emt.OperationNames[instr.Operation],
			instr.DeclarationDepthDifference,
			instr.Address,
			instr.ArgInt)))
	}

	return nil
}
