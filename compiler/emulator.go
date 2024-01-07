package compiler

const (
	ramSize   = 2048 // ram bytes
	stackSize = 500  // stack entries
)

const (
	lit = operation(iota)
	opr
	lod
	sto
	cal
	ret
	inc
	jmp
	jpc
)

const (
	neg = operator(iota)
	add
	sub
	mul
	div
	odd
	eq
	neq
	lss
	leq
	gtr
	geq
)

const (
	rax = register(iota)
	rbx
	rcx
	rdx
	rip
	rsp
	rbp
)

type (
	operation   int
	operator    int
	register    int
	textSegment []byte
	codeSegment []instruction
	ram         []byte

	instruction struct {
		depth     int
		operation operation
		argument  uintptr
	}

	process struct {
		text textSegment
		code codeSegment
	}

	cpu struct {
		registers map[register]uintptr
		stack     *[stackSize]uintptr
	}

	machine struct {
		cpu cpu
		ram ram
	}
)

func newMachine() *machine {
	return &machine{
		cpu: cpu{
			registers: make(map[register]uintptr),
			stack:     new([stackSize]uintptr),
		},
		ram: make(ram, ramSize),
	}
}

func (m *machine) startProcess(text textSegment, code codeSegment) {
	process := process{
		text: text,
		code: code,
	}

	m.cpu.registers[rip] = 0
	m.cpu.registers[rsp] = 0
	m.cpu.registers[rbp] = 0

	for {
		instr := process.code[m.cpu.registers[rip]]
		m.cpu.registers[rip]++

		switch instr.operation {
		case lit: // load constant
			m.cpu.push(instr.argument)

		case jmp: // jump to address
			m.cpu.jmp(instr.argument)

		case jpc: // jump to address if top of stack is zero
			m.cpu.jpc(instr.argument)

		case inc: // allocate space on stack
			m.cpu.registers[rsp] += instr.argument

		case opr: // execute any operator from the operator set
			switch operator(instr.argument) {
			case neg:
				m.cpu.stack[m.cpu.registers[rsp]] = -m.cpu.stack[m.cpu.registers[rsp]]

			case add:
				m.cpu.registers[rsp]--
				m.cpu.stack[m.cpu.registers[rsp]] += m.cpu.stack[m.cpu.registers[rsp]+1]

			case sub:
				m.cpu.registers[rsp]--
				m.cpu.stack[m.cpu.registers[rsp]] -= m.cpu.stack[m.cpu.registers[rsp]+1]

			case mul:
				m.cpu.registers[rsp]--
				m.cpu.stack[m.cpu.registers[rsp]] *= m.cpu.stack[m.cpu.registers[rsp]+1]

			case div:
				m.cpu.registers[rsp]--
				m.cpu.stack[m.cpu.registers[rsp]] /= m.cpu.stack[m.cpu.registers[rsp]+1]

			case odd:
				m.cpu.stack[m.cpu.registers[rsp]] = m.cpu.stack[m.cpu.registers[rsp]] % 2

			case eq:
				m.cpu.registers[rsp]--

				if m.cpu.stack[m.cpu.registers[rsp]] == m.cpu.stack[m.cpu.registers[rsp]+1] {
					m.cpu.stack[m.cpu.registers[rsp]] = 1
				} else {
					m.cpu.stack[m.cpu.registers[rsp]] = 0
				}

			case neq:
				m.cpu.registers[rsp]--

				if m.cpu.stack[m.cpu.registers[rsp]] != m.cpu.stack[m.cpu.registers[rsp]+1] {
					m.cpu.stack[m.cpu.registers[rsp]] = 1
				} else {
					m.cpu.stack[m.cpu.registers[rsp]] = 0
				}

			case lss:
				m.cpu.registers[rsp]--

				if m.cpu.stack[m.cpu.registers[rsp]] < m.cpu.stack[m.cpu.registers[rsp]+1] {
					m.cpu.stack[m.cpu.registers[rsp]] = 1
				} else {
					m.cpu.stack[m.cpu.registers[rsp]] = 0
				}

			case leq:
				m.cpu.registers[rsp]--

				if m.cpu.stack[m.cpu.registers[rsp]] <= m.cpu.stack[m.cpu.registers[rsp]+1] {
					m.cpu.stack[m.cpu.registers[rsp]] = 1
				} else {
					m.cpu.stack[m.cpu.registers[rsp]] = 0
				}

			case gtr:
				m.cpu.registers[rsp]--

				if m.cpu.stack[m.cpu.registers[rsp]] > m.cpu.stack[m.cpu.registers[rsp]+1] {
					m.cpu.stack[m.cpu.registers[rsp]] = 1
				} else {
					m.cpu.stack[m.cpu.registers[rsp]] = 0
				}

			case geq:
				m.cpu.registers[rsp]--

				if m.cpu.stack[m.cpu.registers[rsp]] >= m.cpu.stack[m.cpu.registers[rsp]+1] {
					m.cpu.stack[m.cpu.registers[rsp]] = 1
				} else {
					m.cpu.stack[m.cpu.registers[rsp]] = 0
				}
			}

		case cal: // call procedure
			m.cpu.push(m.cpu.base(instr.depth))
			m.cpu.push(m.cpu.registers[rbp])
			m.cpu.push(m.cpu.registers[rip])
			m.cpu.registers[rbp] = m.cpu.registers[rsp] + 1
			m.cpu.registers[rip] = instr.argument

		case ret: // return from procedure
			m.cpu.registers[rsp] = m.cpu.registers[rbp] - 1
			m.cpu.registers[rip] = m.cpu.stack[m.cpu.registers[rsp]+2]
			m.cpu.registers[rbp] = m.cpu.stack[m.cpu.registers[rsp]+1]

		case lod: // push variable on top of stack
			m.cpu.registers[rsp]++
			m.cpu.stack[m.cpu.registers[rsp]] = m.cpu.stack[m.cpu.base(instr.depth)+instr.argument]

		case sto: // pop variable from top of stack
			m.cpu.stack[m.cpu.base(instr.depth)+instr.argument] = m.cpu.stack[m.cpu.registers[rsp]]
			m.cpu.registers[rsp]--
		}
	}
}

func (c *cpu) base(depth int) uintptr {
	b := c.registers[rbp]

	for depth > 0 {
		b = c.stack[b]
		depth--
	}

	return b
}

func (c *cpu) push(val uintptr) {
	c.registers[rsp]++
	c.stack[c.registers[rsp]] = val
}

func (c *cpu) pop(reg register) {
	c.registers[reg] = c.stack[c.registers[rsp]]
	c.registers[rsp]--
}

func (c *cpu) mov(reg register, val uintptr) {
	c.registers[reg] = val
}

func (c *cpu) call(addr uintptr) {
	c.push(c.registers[rip])
	c.registers[rip] = addr
}

func (c *cpu) ret() {
	c.pop(rip)
}

func (c *cpu) jmp(addr uintptr) {
	c.registers[rip] = addr
}

func (c *cpu) jpc(addr uintptr) {
	if c.registers[rsp] == 0 {
		c.registers[rip] = addr
	}

	c.registers[rsp]--
}

func (c *cpu) add(reg register, val uintptr) {
	c.registers[reg] += val
}

func (c *cpu) sub(reg register, val uintptr) {
	c.registers[reg] -= val
}

func (c *cpu) mul(reg register, val uintptr) {
	c.registers[reg] *= val
}

func (c *cpu) div(reg register, val uintptr) {
	c.registers[reg] /= val
}

func (c *cpu) neg(reg register) {
	c.registers[reg] = -c.registers[reg]
}

func (c *cpu) odd(reg register) {
	c.registers[reg] = c.registers[reg] % 2
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
	|					| 2						| var n='squ',d=0,o=4		| 2
	| f=jmp,d=0,a=0		| 1	square				| var n='x',d=0,o=3			| 1
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
