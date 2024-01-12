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
	stackSize = 500 // stack entries
)

const (
	ip = register(iota)
	sp
	bp
)

type (
	register int

	process struct {
		code emt.TextSection
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

	if err := process.load(sections); err != nil {
		return err
	}

	m.cpu.registers[ip], m.cpu.registers[sp], m.cpu.registers[bp] = 0, 0, 0

	for {
		instr, err := process.getInstruction(m.cpu.registers[ip])

		if err != nil {
			return err
		}

		m.cpu.registers[ip]++

		switch instr.Operation {
		case emt.Lit: // load constant
			m.cpu.push(instr.Argument)

		case emt.Jmp: // jump to address
			m.cpu.jmp(instr.Argument)

		case emt.Jpc: // jump to address if top of stack is zero
			m.cpu.jpc(instr.Argument)

		case emt.Inc: // allocate space on stack
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

		case emt.Cal: // call procedure
			m.cpu.push(m.cpu.base(instr.Depth))
			m.cpu.push(m.cpu.registers[bp])
			m.cpu.push(m.cpu.registers[ip])
			m.cpu.registers[bp] = m.cpu.registers[sp] + 1
			m.cpu.registers[ip] = instr.Argument

		case emt.Ret: // return from procedure
			m.cpu.registers[sp] = m.cpu.registers[bp] - 1
			m.cpu.registers[ip] = m.cpu.stack[m.cpu.registers[sp]+2]
			m.cpu.registers[bp] = m.cpu.stack[m.cpu.registers[sp]+1]

		case emt.Lod: // push variable on top of stack
			m.cpu.registers[sp]++
			m.cpu.stack[m.cpu.registers[sp]] = m.cpu.stack[m.cpu.base(instr.Depth)+instr.Argument]

		case emt.Sto: // pop variable from top of stack
			m.cpu.stack[m.cpu.base(instr.Depth)+instr.Argument] = m.cpu.stack[m.cpu.registers[sp]]
			m.cpu.registers[sp]--
		}

		if m.cpu.registers[ip] == 0 {
			break
		}
	}

	return nil
}

func (m *machine) printProgram(sections []byte, print io.Writer) error {
	return (&process{}).dump(sections, print)
}

func (c *cpu) base(Depth int32) emt.Address {
	b := c.registers[bp]

	for Depth > 0 {
		b = c.stack[b]
		Depth--
	}

	return b
}

func (c *cpu) push(val emt.Address) {
	c.registers[sp]++
	c.stack[c.registers[sp]] = val
}

func (c *cpu) jmp(addr emt.Address) {
	c.registers[ip] = addr
}

func (c *cpu) jpc(addr emt.Address) {
	if c.registers[sp] == 0 {
		c.registers[ip] = addr
	}

	c.registers[sp]--
}

func (p *process) load(sections []byte) error {
	p.code = make(emt.TextSection, len(sections)/binary.Size(emt.Instruction{}))

	var buffer bytes.Buffer
	buffer.Write(sections)

	if err := binary.Read(&buffer, binary.LittleEndian, p.code); err != nil {
		return err
	}

	return nil
}

func (p *process) getInstruction(address emt.Address) (emt.Instruction, error) {
	if address >= emt.Address(len(p.code)) {
		return emt.Instruction{}, fmt.Errorf("address '%v' out of range", address)
	}

	return p.code[address], nil
}

func (p *process) dump(sections []byte, print io.Writer) error {
	if err := p.load(sections); err != nil {
		return err
	}

	print.Write([]byte(fmt.Sprintf("%-5v %-5v %-5v %-5v\n", "addr", "op", "dep", "arg")))

	for addr, instr := range p.code {
		print.Write([]byte(fmt.Sprintf("%-5v %-5v %-5v %-5v\n",
			addr,
			emt.OperationNames[instr.Operation],
			instr.Depth,
			instr.Argument)))
	}

	return nil
}

/*
	var x, squ;

	procedure square;
	begin
   		squ:= x * x
	end;

	begin
   		x := 1;

		while x <= 10 do
   		begin
    		call square;
      		x := x + 1
   		end
	end.
*/

/*
	PL/0 Code Segment and Symbol Table

	| 					|						| proc n='square',d=0,a=1	| 3
	| f=inc,d=1,a=3		| 2	square				| var n='squ',d=0,o=4		| 2
	| f=jmp,d=1,a=2		| 1	square				| var n='x',d=0,o=3			| 1
	| f=jmp d=0,a=0		| 0	main			   	| proc n='main',d=0,a=0 	| 0
	+-------------------+ code					+---------------------------+ symtab
*/

/*
	PL/0 Stack Segment


	| ip = 0			| 2
	| bp = 0			| 1
	| base = 0			| 0
	+-------------------+ stack
*/
