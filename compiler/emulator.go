// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package compiler

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"io"

	emt "github.com/petersen65/PL0/emitter"
)

const (
	stackSize          = 16384 // stack entries are 64-bit unsigned integers
	stackForbiddenZone = 1024  // stack entries above this address are forbidden to be used
)

const (
	ip = register(iota) // instruction pointer is pointing to the next instruction to be executed
	sp                  // stack pointer is pointing to the top of the stack
	bp                  // base pointer is pointing to the base of the current stack frame (descriptor)
)

type (
	register int

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

	// execute instructions until the the frist callee returns to the first caller (entrypoint returns to external code)
	for {
		if m.cpu.registers[ip] >= uint64(len(process.text)) {
			return fmt.Errorf("address '%v' out of range", m.cpu.registers[ip])
		}

		if m.cpu.registers[sp] >= stackSize-stackForbiddenZone || m.cpu.registers[sp] < 2 {
			return fmt.Errorf("stack overflow at address '%v'", m.cpu.registers[ip])
		}

		instr := process.text[m.cpu.registers[ip]]
		m.cpu.registers[ip]++

		switch instr.Operation {
		case emt.Lit: // load int64 constant on top of stack
			m.cpu.push(uint64(instr.Arg1))

		case emt.Jmp: // unconditionally jump to uint64 address
			m.cpu.jmp(uint64(instr.Address))

		case emt.Jpc: // jump to uint64 address if top of stack is 0
			m.cpu.jpc(uint64(instr.Address))

		case emt.Inc: // allocate space on stack for variables of a procedure
			m.cpu.registers[sp] += uint64(instr.Address)

		case emt.Neg: // negate int64 element on top of stack
			m.cpu.stack[m.cpu.registers[sp]] = uint64(-int64(m.cpu.stack[m.cpu.registers[sp]]))

		case emt.Add: // add top two stack int64 elements
			m.cpu.registers[sp]--
			m.cpu.stack[m.cpu.registers[sp]] = uint64(int64(m.cpu.stack[m.cpu.registers[sp]]) + int64(m.cpu.stack[m.cpu.registers[sp]+1]))

		case emt.Sub: // subtract top two stack int64 elements
			m.cpu.registers[sp]--
			m.cpu.stack[m.cpu.registers[sp]] = uint64(int64(m.cpu.stack[m.cpu.registers[sp]]) - int64(m.cpu.stack[m.cpu.registers[sp]+1]))

		case emt.Mul: // multiply top two stack int64 elements
			m.cpu.registers[sp]--
			m.cpu.stack[m.cpu.registers[sp]] = uint64(int64(m.cpu.stack[m.cpu.registers[sp]]) * int64(m.cpu.stack[m.cpu.registers[sp]+1]))

		case emt.Div: // divide top two stack int64 elements
			m.cpu.registers[sp]--
			m.cpu.stack[m.cpu.registers[sp]] = uint64(int64(m.cpu.stack[m.cpu.registers[sp]]) / int64(m.cpu.stack[m.cpu.registers[sp]+1]))

		case emt.Odd: // test if top of stack int64 element is odd
			m.cpu.stack[m.cpu.registers[sp]] = uint64(int64(m.cpu.stack[m.cpu.registers[sp]]) % 2)

		case emt.Eq: // test if top two stack int64 elements are equal
			m.cpu.registers[sp]--

			if int64(m.cpu.stack[m.cpu.registers[sp]]) == int64(m.cpu.stack[m.cpu.registers[sp]+1]) {
				m.cpu.stack[m.cpu.registers[sp]] = 1
			} else {
				m.cpu.stack[m.cpu.registers[sp]] = 0
			}

		case emt.Neq: // test if top two stack int64 elements are not equal
			m.cpu.registers[sp]--

			if int64(m.cpu.stack[m.cpu.registers[sp]]) != int64(m.cpu.stack[m.cpu.registers[sp]+1]) {
				m.cpu.stack[m.cpu.registers[sp]] = 1
			} else {
				m.cpu.stack[m.cpu.registers[sp]] = 0
			}

		case emt.Lss: // test if second stack int64 element is less than top stack int64 element
			m.cpu.registers[sp]--

			if int64(m.cpu.stack[m.cpu.registers[sp]]) < int64(m.cpu.stack[m.cpu.registers[sp]+1]) {
				m.cpu.stack[m.cpu.registers[sp]] = 1
			} else {
				m.cpu.stack[m.cpu.registers[sp]] = 0
			}

		case emt.Leq: // test if second stack int64 element is less than or equal to top stack int64 element
			m.cpu.registers[sp]--

			if int64(m.cpu.stack[m.cpu.registers[sp]]) <= int64(m.cpu.stack[m.cpu.registers[sp]+1]) {
				m.cpu.stack[m.cpu.registers[sp]] = 1
			} else {
				m.cpu.stack[m.cpu.registers[sp]] = 0
			}

		case emt.Gtr: // test if second stack int64 element is greater than top stack int64 element
			m.cpu.registers[sp]--

			if int64(m.cpu.stack[m.cpu.registers[sp]]) > int64(m.cpu.stack[m.cpu.registers[sp]+1]) {
				m.cpu.stack[m.cpu.registers[sp]] = 1
			} else {
				m.cpu.stack[m.cpu.registers[sp]] = 0
			}

		case emt.Geq: // test if second stack int64 element is greater than or equal to top stack int64 element
			m.cpu.registers[sp]--

			if int64(m.cpu.stack[m.cpu.registers[sp]]) >= int64(m.cpu.stack[m.cpu.registers[sp]+1]) {
				m.cpu.stack[m.cpu.registers[sp]] = 1
			} else {
				m.cpu.stack[m.cpu.registers[sp]] = 0
			}

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

		case emt.Lod: // push int64 variable on top of stack loaded from its base plus offset
			m.cpu.registers[sp]++
			m.cpu.stack[m.cpu.registers[sp]] = m.cpu.stack[m.cpu.base(instr.Depth)+uint64(instr.Address)+3]

		case emt.Sto: // pop int64 variable from top of stack to its base plus offset
			m.cpu.stack[m.cpu.base(instr.Depth)+uint64(instr.Address)+3] = m.cpu.stack[m.cpu.registers[sp]]
			m.cpu.registers[sp]--

		case emt.Sys: // system call to operating system based on system call code
			m.cpu.sys(emt.SystemCall(instr.Address))
		}
	}
}

func (m *machine) printProgram(sections []byte, print io.Writer) error {
	return (&process{}).dump(sections, print)
}

func (p *process) load(sections []byte) error {
	p.text = make(emt.TextSection, len(sections)/binary.Size(emt.Instruction{}))

	var buffer bytes.Buffer
	buffer.Write(sections)

	if err := binary.Read(&buffer, binary.LittleEndian, p.text); err != nil {
		return err
	}

	return nil
}

func (p *process) dump(sections []byte, print io.Writer) error {
	if err := p.load(sections); err != nil {
		return err
	}

	print.Write([]byte(fmt.Sprintf("%-5v %-5v %-5v %-5v %-5v\n", "text", "op", "dep", "addr", "arg1")))

	for text, instr := range p.text {
		print.Write([]byte(fmt.Sprintf("%-5v %-5v %-5v %-5v %-5v\n",
			text,
			emt.OperationNames[instr.Operation],
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
		fmt.Printf("%v\n", int64(c.stack[c.registers[sp]]))
		c.registers[sp]--
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

// unconditionally jump to address
func (c *cpu) jmp(addr uint64) {
	c.registers[ip] = addr
}

// if top of stack is 0, jump to address
func (c *cpu) jpc(addr uint64) {
	if c.stack[c.registers[sp]] == 0 {
		c.registers[ip] = addr
	}

	c.registers[sp]--
}
