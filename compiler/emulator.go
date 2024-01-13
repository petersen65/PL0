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

const stackSize = 500 // stack entries

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
		registers map[register]emt.Address
		stack     [stackSize]emt.Address
	}

	machine struct {
		cpu cpu
	}
)

func newMachine() *machine {
	return &machine{
		cpu: cpu{
			registers: make(map[register]emt.Address),
			stack:     [stackSize]emt.Address{},
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
		instr, err := process.getInstruction(m.cpu.registers[ip])

		if err != nil {
			return err
		}

		m.cpu.registers[ip]++

		switch instr.Operation {
		case emt.Lit: // load constant on top of stack
			m.cpu.push(instr.Argument)

		case emt.Jmp: // unconditionally jump to address
			m.cpu.jmp(instr.Argument)

		case emt.Jpc: // jump to address if top of stack is zero
			m.cpu.jpc(instr.Argument)

		case emt.Inc: // allocate space on stack for variables of a procedure
			m.cpu.registers[sp] += instr.Argument

		case emt.Neg: // negate top of stack
			m.cpu.stack[m.cpu.registers[sp]] = -m.cpu.stack[m.cpu.registers[sp]]

		case emt.Add: // add top two stack elements
			m.cpu.registers[sp]--
			m.cpu.stack[m.cpu.registers[sp]] += m.cpu.stack[m.cpu.registers[sp]+1]

		case emt.Sub: // subtract top two stack elements
			m.cpu.registers[sp]--
			m.cpu.stack[m.cpu.registers[sp]] -= m.cpu.stack[m.cpu.registers[sp]+1]

		case emt.Mul: // multiply top two stack elements
			m.cpu.registers[sp]--
			m.cpu.stack[m.cpu.registers[sp]] *= m.cpu.stack[m.cpu.registers[sp]+1]

		case emt.Div: // divide top two stack elements
			m.cpu.registers[sp]--
			m.cpu.stack[m.cpu.registers[sp]] /= m.cpu.stack[m.cpu.registers[sp]+1]

		case emt.Odd: // test if top of stack is odd
			m.cpu.stack[m.cpu.registers[sp]] = m.cpu.stack[m.cpu.registers[sp]] % 2

		case emt.Eq: // test if top two stack elements are equal
			m.cpu.registers[sp]--

			if m.cpu.stack[m.cpu.registers[sp]] == m.cpu.stack[m.cpu.registers[sp]+1] {
				m.cpu.stack[m.cpu.registers[sp]] = 1
			} else {
				m.cpu.stack[m.cpu.registers[sp]] = 0
			}

		case emt.Neq: // test if top two stack elements are not equal
			m.cpu.registers[sp]--

			if m.cpu.stack[m.cpu.registers[sp]] != m.cpu.stack[m.cpu.registers[sp]+1] {
				m.cpu.stack[m.cpu.registers[sp]] = 1
			} else {
				m.cpu.stack[m.cpu.registers[sp]] = 0
			}

		case emt.Lss: // test if second stack element is less than top stack element
			m.cpu.registers[sp]--

			if m.cpu.stack[m.cpu.registers[sp]] < m.cpu.stack[m.cpu.registers[sp]+1] {
				m.cpu.stack[m.cpu.registers[sp]] = 1
			} else {
				m.cpu.stack[m.cpu.registers[sp]] = 0
			}

		case emt.Leq: // test if second stack element is less than or equal to top stack element
			m.cpu.registers[sp]--

			if m.cpu.stack[m.cpu.registers[sp]] <= m.cpu.stack[m.cpu.registers[sp]+1] {
				m.cpu.stack[m.cpu.registers[sp]] = 1
			} else {
				m.cpu.stack[m.cpu.registers[sp]] = 0
			}

		case emt.Gtr: // test if second stack element is greater than top stack element
			m.cpu.registers[sp]--

			if m.cpu.stack[m.cpu.registers[sp]] > m.cpu.stack[m.cpu.registers[sp]+1] {
				m.cpu.stack[m.cpu.registers[sp]] = 1
			} else {
				m.cpu.stack[m.cpu.registers[sp]] = 0
			}

		case emt.Geq: // test if second stack element is greater than or equal to top stack element
			m.cpu.registers[sp]--

			if m.cpu.stack[m.cpu.registers[sp]] >= m.cpu.stack[m.cpu.registers[sp]+1] {
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

			// jump to procedure
			m.cpu.jmp(instr.Argument)

		case emt.Ret: // callee procedure returns to caller procedure
			// returning from the entrypoint of the program exits the program
			if m.cpu.registers[bp] == 0 {
				return nil
			}

			// restore state of caller procdure from descriptor of callee procedure
			m.cpu.pop(ip)            // restore callers instruction pointer
			m.cpu.pop(bp)            // restore callers base pointer
			m.cpu.registers[sp] -= 1 // discard dynamic link and restore callers top of stack

		case emt.Lod: // push variable on top of stack
			m.cpu.registers[sp]++
			m.cpu.stack[m.cpu.registers[sp]] = m.cpu.stack[m.cpu.base(instr.Depth)+instr.Argument]

		case emt.Sto: // pop variable from top of stack
			m.cpu.stack[m.cpu.base(instr.Depth)+instr.Argument] = m.cpu.stack[m.cpu.registers[sp]]
			m.cpu.registers[sp]--
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

func (p *process) getInstruction(address emt.Address) (emt.Instruction, error) {
	if address >= emt.Address(len(p.text)) {
		return emt.Instruction{}, fmt.Errorf("address '%v' out of range", address)
	}

	return p.text[address], nil
}

func (p *process) dump(sections []byte, print io.Writer) error {
	if err := p.load(sections); err != nil {
		return err
	}

	print.Write([]byte(fmt.Sprintf("%-5v %-5v %-5v %-5v\n", "addr", "op", "dep", "arg")))

	for addr, instr := range p.text {
		print.Write([]byte(fmt.Sprintf("%-5v %-5v %-5v %-5v\n",
			addr,
			emt.OperationNames[instr.Operation],
			instr.Depth,
			instr.Argument)))
	}

	return nil
}

func (c *cpu) base(depth int32) emt.Address {
	b := c.registers[bp]

	for depth > 0 {
		b = c.stack[b]
		depth--
	}

	return b
}

// push argument on top of stack, top of stack points to new argument
func (c *cpu) push(arg emt.Address) {
	c.registers[sp]++
	c.stack[c.registers[sp]] = arg
}

func (c *cpu) pop(reg register) {
	c.registers[reg] = c.stack[c.registers[sp]]
	c.registers[sp]--
}

// unconditionally jump to address
func (c *cpu) jmp(addr emt.Address) {
	c.registers[ip] = addr
}

// if top of stack is 0, jump to address
func (c *cpu) jpc(addr emt.Address) {
	if c.registers[sp] == 0 {
		c.registers[ip] = addr
	}

	c.registers[sp]--
}
