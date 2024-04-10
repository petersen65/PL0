// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emulator

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"io"
	"math"

	cod "github.com/petersen65/PL0/v2/code"
	cor "github.com/petersen65/PL0/v2/core"
)

// Operation codes for assembler instructions.
const (
	_ = operationCode(iota)

	// stack assembly instructions for stack operations
	push
	pop

	// comparison assembly instruction for all relational operators and conditional jumps
	cmp

	// unconditional and conditional jump assembly instructions
	jmp
	je
	jne
	jl
	jle
	jg
	jge

	// arithmetic assembly instructions with one operand
	neg
	and

	// arithmetic assembly instructions with two operands
	add
	sub
	imul
	idiv

	// subroutine assembly instructions for procedure calls
	call
	ret

	// pseudo-assembly instructions that require multiple assembly instructions
	loadvar
	storevar
	alloc
	rcall
)

// Operand types for assembler instructions.
const (
	_                = operandType(iota)
	registerOperand  // 64-bit registers: rax, rbx, rcx, rdx, rsi, rdi, rsp, rbp, r8 to r15
	immediateOperand // int64 constant values like 'mov eax, 1'
	addressOperand   // uint64 destinations for jump instructions or other instructions that require an address
	labelOperand     // labels are used to specify jump targets and must be replaced by absolute addresses before execution
)

// Call codes for the programming language runtime library.
const (
	readln = runtimeCall(iota)
	writeln
)

const (
	stackSize           = 16384 // stack entries are 64-bit unsigned integers
	stackForbiddenZone  = 1024  // stack entries above this address are forbidden to be used
	stackDescriptorSize = 3     // size of a stack frame descriptor
)

const (
	rax   = register(iota) // accumulator is used for intermediate results of arithmetic operations
	rbx                    // base register can be used for addressing variables
	rcx                    // counter register can be used for counting iterations of loops
	rdx                    // data register can be used for addressing variables
	rsi                    // source index register used in string and array operations as a pointer to source data
	rdi                    // destination index register is used used in string and array operations as a pointer to destination data
	r8                     // 64-bit general purpose register
	r9                     // 64-bit general purpose register
	r10                    // 64-bit general purpose register
	r11                    // 64-bit general purpose register
	r12                    // 64-bit general purpose register
	r13                    // 64-bit general purpose register
	r14                    // 64-bit general purpose register
	r15                    // 64-bit general purpose register
	flags                  // flags register contains the current state of the CPU and reflects the result of arithmetic operations
	rip                    // instruction pointer is pointing to the next instruction to be executed
	rsp                    // stack pointer is pointing to the top of the stack
	rbp                    // base pointer is pointing to the base of the current stack frame
)

const (
	_  = flag(iota)
	zf = 0x0000000000000040 // zero flag is set if the result of an arithmetic operation is zero
	sf = 0x0000000000000080 // sign flag is set if the result of an arithmetic operation is negative
	of = 0x0000000000000800 // overflow flag is set if the result of an arithmetic operation is too large to fit in the register
)

type (
	// Type for operationCode codes.
	operationCode int32

	// Type of operands.
	operandType int32

	// Type for runtime call codes.
	runtimeCall int64

	// Text section of the binary target.
	textSection []*instruction

	// Operand of an operation with a register, address, int64 value, or label.
	operand struct {
		operand  operandType // type of the operand
		register register    // register operand for the operation
		address  uint64      // jump address or offset of a variable
		argInt   int64       // int64 value argument
		label    string      // labels for jump instructions will be replaced by an address
	}

	// Instruction is the representation of a single operation with all its operands and a depth difference.
	instruction struct {
		operation       operationCode // operation code of the instruction
		operands        []*operand    // operands for the operation
		depthDifference int32         // block nesting depth difference between variable use and variable declaration
		label           string        // label to whom a jump instruction will jump
	}

	// Enumeration of registers of the CPU.
	register int32

	// Flags that reflect the state of the CPU and the result of arithmetic operations.
	flag uint64

	// Virtual process that holds instructions of a binary target.
	process struct {
		text textSection // text section with binary instructions
	}

	// Virtual CPU with registers and the stack.
	cpu struct {
		registers map[register]uint64 // registers of the CPU
		stack     []uint64            // stack of the CPU
	}

	// Virtual machine that can run processes and modules.
	machine struct {
		cpu     cpu
		process process
	}
)

// Map operation codes to their string representation.
var operationMap = map[operationCode]string{
	push:     "push",
	pop:      "pop",
	cmp:      "cmp",
	jmp:      "jmp",
	je:       "je",
	jne:      "jne",
	jl:       "jl",
	jle:      "jle",
	jg:       "jg",
	jge:      "jge",
	neg:      "neg",
	and:      "and",
	add:      "add",
	sub:      "sub",
	imul:     "imul",
	idiv:     "idiv",
	call:     "call",
	ret:      "ret",
	loadvar:  "loadvar",
	storevar: "storevar",
	alloc:    "alloc",
	rcall:    "rcall",
}

// Create a new emulation machine with CPU, registers and stack that can run binary processes.
func newMachine() Machine {
	return &machine{
		cpu: cpu{
			registers: make(map[register]uint64),
			stack:     make([]uint64, stackSize),
		},
		process: process{
			text: make(textSection, 0),
		},
	}
}

// Create a new instruction with an operation code, a depth difference, and operands.
func newInstruction(op operationCode, depthDifference int32, label string, operands ...*operand) *instruction {
	return &instruction{
		operation:       op,
		operands:        operands,
		depthDifference: depthDifference,
		label:           label,
	}
}

// Create a new operand with a register, address, int64 value, or label.
func newOperand(opType operandType, op any) *operand {
	switch opType {
	case registerOperand:
		return &operand{operand: registerOperand, register: op.(register)}

	case immediateOperand:
		return &operand{operand: immediateOperand, argInt: op.(int64)}

	case addressOperand:
		return &operand{operand: addressOperand, address: op.(uint64)}

	case labelOperand:
		return &operand{operand: labelOperand, label: op.(string)}

	default:
		return &operand{} // empty operand will generate an error when used
	}
}

// Import a raw byte slice into the text section of a process.
func (p *process) importRaw(raw []byte) error {
	p.text = make(textSection, len(raw)/binary.Size(instruction{}))

	var buffer bytes.Buffer
	buffer.Write(raw)

	if err := binary.Read(&buffer, binary.LittleEndian, p.text); err != nil {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, textSectionImportFailed, nil, err)
	}

	return nil
}

// Load a binary target and return an error if the target import fails.
func (m *machine) Load(raw []byte) error {
	// import a raw target into a process
	if err := m.process.importRaw(raw); err != nil {
		return err
	}

	return nil
}

// Load a module and return an error if the module fails to JIT compile.
func (m *machine) LoadModule(module cod.Module) error {
	// JIT compile the module into a process
	if err := m.process.jitCompile(module); err != nil {
		return err
	}

	return nil
}

// Print an assembly target to the specified writer.
func (m *machine) Print(print io.Writer, args ...any) error {
	return nil
}

// Export the assembly or binary target that is managed by the virtual machine.
func (m *machine) Export(format cor.ExportFormat, print io.Writer) error {
	switch format {
	case cor.Text:
		// print is a convenience function to export the assembly target as a string to the print writer
		return m.Print(print)

	case cor.Binary:
		return nil

	default:
		panic(cor.NewGeneralError(cor.Emulator, failureMap, cor.Fatal, unknownExportFormat, format, nil))
	}
}

// Run a process and return an error if the process fails to execute.
func (m *machine) RunProcess() error {
	// state of active callee, which is a running procedure: bp, sp, and ip
	// state of caller is in descriptor of callee (first 3 entries of stack frame)
	// if callee calls another procedure, the callee's state is saved in the stack frame of the new callee (its descriptor)

	m.cpu.registers[rax] = 0                       // accumulator register
	m.cpu.registers[rbx] = 0                       // base register
	m.cpu.registers[rcx] = 0                       // counter register
	m.cpu.registers[rdx] = 0                       // data register
	m.cpu.registers[rsi] = 0                       // source index register
	m.cpu.registers[rdi] = 0                       // destination index register
	m.cpu.registers[r8] = 0                        // general purpose register
	m.cpu.registers[r9] = 0                        // general purpose register
	m.cpu.registers[r10] = 0                       // general purpose register
	m.cpu.registers[r11] = 0                       // general purpose register
	m.cpu.registers[r12] = 0                       // general purpose register
	m.cpu.registers[r13] = 0                       // general purpose register
	m.cpu.registers[r14] = 0                       // general purpose register
	m.cpu.registers[r15] = 0                       // general purpose register
	m.cpu.registers[flags] = 0                     // flags register
	m.cpu.registers[rip] = 0                       // instruction pointer
	m.cpu.registers[rsp] = stackDescriptorSize - 1 // stack pointer to top of stack (end of stack frame descriptor)
	m.cpu.registers[rbp] = 0                       // base pointer to bottom of stack frame

	// preserve state of first caller and create descriptor of first callee
	// first callee is the entrypoint of the program
	m.cpu.stack[0] = m.cpu.registers[rip]       // return address (instruction pointer of caller + 1)
	m.cpu.stack[1] = m.cpu.registers[rbp]       // dynamic link chains base pointers so that each callee knows the base pointer of its caller
	m.cpu.stack[2] = m.cpu.parent(0)            // static link points to direct parent block in block nesting hierarchy
	m.cpu.registers[rbp] = m.cpu.registers[rsp] // base pointer of callee is pointing to the end of its descriptor

	// execute instructions until the the first callee returns to the first caller (entrypoint returns to external code)
	for {
		if m.cpu.registers[rip] >= uint64(len(m.process.text)) {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, addressOutOfRange, m.cpu.registers[rip], nil)
		}

		if m.cpu.registers[rsp] >= stackSize-stackForbiddenZone || m.cpu.registers[rsp] < (stackDescriptorSize-1) {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, stackOverflow, m.cpu.registers[rsp], nil)
		}

		instr := m.process.text[m.cpu.registers[rip]]
		m.cpu.registers[rip]++

		switch instr.operation {
		case push: // push operand onto the stack
			if len(instr.operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, operationMap[push], nil)
			}

			if err := m.cpu.push(instr.operands[0]); err != nil {
				return err
			}

		case jmp: // unconditionally jump to uint64 address
			if len(instr.operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, operationMap[jmp], nil)
			}

			if err := m.cpu.jmp(instr.operands[0]); err != nil {
				return err
			}

		case je: // jump to uint64 address if last comparison was equal
			if len(instr.operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, operationMap[je], nil)
			}

			if err := m.cpu.je(instr.operands[0]); err != nil {
				return err
			}

		case jne: // jump to uint64 address if last comparison was not equal
			if len(instr.operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, operationMap[jne], nil)
			}

			if err := m.cpu.jne(instr.operands[0]); err != nil {
				return err
			}

		case jl: // jump to uint64 address if last comparison was less than
			if len(instr.operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, operationMap[jl], nil)
			}

			if err := m.cpu.jl(instr.operands[0]); err != nil {
				return err
			}

		case jle: // jump to uint64 address if last comparison was less than or equal to
			if len(instr.operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, operationMap[jle], nil)
			}

			if err := m.cpu.jle(instr.operands[0]); err != nil {
				return err
			}

		case jg: // jump to uint64 address if last comparison was greater than
			if len(instr.operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, operationMap[jg], nil)
			}

			if err := m.cpu.jg(instr.operands[0]); err != nil {
				return err
			}

		case jge: // jump to uint64 address if last comparison was greater than or equal to
			if len(instr.operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, operationMap[jge], nil)
			}

			if err := m.cpu.jge(instr.operands[0]); err != nil {
				return err
			}

		case alloc: // allocate stack space for variables
			if len(instr.operands) != 1 || instr.operands[0].operand != addressOperand {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, operationMap[alloc], nil)
			}

			m.cpu.registers[rsp] += instr.operands[0].address

		case neg: // negate int64 element on top of stack
			if err := m.cpu.neg(); err != nil {
				return err
			}

		case and: // test if top of stack's uint64 element is odd and set zero flag if it is
			m.cpu.and(1)

		case add: // add two int64 elements from top of stack and store the result onto the stack
			if err := m.cpu.add(); err != nil {
				return err
			}

		case sub: // subtract two int64 elements from top of stack and store the result onto the stack
			if err := m.cpu.sub(); err != nil {
				return err
			}

		case imul: // multiply two int64 elements from top of stack and store the result onto the stack
			if err := m.cpu.mul(); err != nil {
				return err
			}

		case idiv: // divide two int64 elements from top of stack and store the result onto the stack
			if err := m.cpu.div(); err != nil {
				return err
			}

		case cmp: // compare two int64 elements from top of stack and set flags register based on result
			m.cpu.cmp()

		case call: // caller procedure calls callee procedure
			if len(instr.operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, operationMap[call], nil)
			}

			// create descriptor of procedure being called and preserve state of caller in it
			m.cpu.push(newOperand(registerOperand, m.cpu.registers[rip]))               // return address
			m.cpu.push(newOperand(registerOperand, m.cpu.registers[rbp]))               // dynamic link
			m.cpu.push(newOperand(addressOperand, m.cpu.parent(instr.depthDifference))) // static link

			// base pointer of procedure being called is pointing to the end of its descriptor
			m.cpu.registers[rbp] = m.cpu.registers[rsp]

			// jump to procedure at uint64 address
			if err := m.cpu.jmp(instr.operands[0]); err != nil {
				return err
			}

		case ret: // callee procedure returns to caller procedure
			// restore state of caller procdure from descriptor of callee procedure
			m.cpu.registers[rsp] = m.cpu.registers[rbp] - 1 // discard stack space and static link
			m.cpu.pop(newOperand(registerOperand, rbp))     // restore callers base pointer
			m.cpu.pop(newOperand(registerOperand, rip))     // restore callers instruction pointer

			// returning from the entrypoint of the program exits the program
			if m.cpu.registers[rip] == 0 {
				return nil
			}

		case loadvar: // copy int64 variable loaded from its base plus offset onto the stack
			if len(instr.operands) != 1 || instr.operands[0].operand != addressOperand {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, operationMap[loadvar], nil)
			}

			variablesBase := m.cpu.parent(instr.depthDifference) + 1                          // base pointer + 1
			variableOffset := instr.operands[0].address                                       // variable offset
			m.cpu.push(newOperand(addressOperand, m.cpu.stack[variablesBase+variableOffset])) // variables base + variable offset

		case storevar: // copy int64 element from top of stack to a variable stored within its base plus offset
			if len(instr.operands) != 1 || instr.operands[0].operand != addressOperand {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, operationMap[storevar], nil)
			}

			variablesBase := m.cpu.parent(instr.depthDifference) + 1         // base pointer + 1
			variableOffset := instr.operands[0].address                      // variable offset
			m.cpu.pop(newOperand(registerOperand, rax))                      // int64 element to be stored in variable
			m.cpu.stack[variablesBase+variableOffset] = m.cpu.registers[rax] // variables base + variable offset

		case rcall: // call to programming language runtime library based on runtime call code
			if len(instr.operands) != 1 || instr.operands[0].operand != immediateOperand {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, operationMap[rcall], nil)
			}

			m.cpu.runtime(runtimeCall(instr.operands[0].argInt))

		default:
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unknownOperation, m.cpu.registers[rip]-1, nil)
		}
	}
}

// Follow static link to parent blocks in compile-time block nesting hierarchy.
func (c *cpu) parent(difference int32) uint64 {
	basePointer := c.registers[rbp]

	for i := int32(0); i < difference; i++ {
		basePointer = c.stack[basePointer]
	}

	return basePointer
}

// Push operand on top of stack, top of stack points to new operand.
func (c *cpu) push(op *operand) error {
	var arg uint64

	switch op.operand {
	case registerOperand:
		arg = c.registers[op.register]

	case immediateOperand:
		arg = uint64(op.argInt)

	case addressOperand:
		arg = op.address

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, operationMap[push], nil)
	}

	c.registers[rsp]++
	c.stack[c.registers[rsp]] = arg
	return nil
}

// Pop element from top of stack into operand, top of stack points to previous element.
func (c *cpu) pop(op *operand) error {
	switch op.operand {
	case registerOperand:
		c.registers[op.register] = c.stack[c.registers[rsp]]

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, operationMap[pop], nil)
	}

	if c.registers[rsp] > 0 {
		c.registers[rsp]--
	}

	return nil
}

// Negate int64 element from top of stack.
func (c *cpu) neg() error {
	c.pop(newOperand(registerOperand, rax))
	c.set_of_neg(rax)

	if c.test_of() {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, arithmeticOverflowNegation, c.registers[rip]-1, nil)
	}

	c.registers[rax] = uint64(-int64(c.registers[rax]))
	c.set_zf(rax)
	c.set_sf(rax)

	c.push(newOperand(registerOperand, rax))
	return nil
}

// Perform bitwise 'and' operation with uint64 element from top of stack and uint64 argument.
func (c *cpu) and(arg uint64) {
	c.pop(newOperand(registerOperand, rax))
	c.registers[rax] = c.registers[rax] & arg

	c.set_zf(rax)
	c.set_sf(rax)
	c.unset_of()

	c.push(newOperand(registerOperand, rax))
}

// Add two int64 elements from top of stack and store the result onto the stack.
func (c *cpu) add() error {
	c.pop(newOperand(registerOperand, rbx))
	c.pop(newOperand(registerOperand, rax))
	c.set_of_add(rax, rbx)

	if c.test_of() {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, arithmeticOverflowAddition, c.registers[rip]-1, nil)
	}

	c.registers[rax] = uint64(int64(c.registers[rax]) + int64(c.registers[rbx]))
	c.set_zf(rax)
	c.set_sf(rax)

	c.push(newOperand(registerOperand, rax))
	return nil
}

// Subtract two int64 elements from top of stack and store the result onto the stack.
func (c *cpu) sub() error {
	c.pop(newOperand(registerOperand, rbx))
	c.pop(newOperand(registerOperand, rax))
	c.set_of_sub(rax, rbx)

	if c.test_of() {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, arithmeticOverflowSubtraction, c.registers[rip]-1, nil)

	}

	c.registers[rax] = uint64(int64(c.registers[rax]) - int64(c.registers[rbx]))
	c.set_zf(rax)
	c.set_sf(rax)

	c.push(newOperand(registerOperand, rax))
	return nil
}

// Multiply two int64 elements from top of stack and store the result onto the stack.
func (c *cpu) mul() error {
	c.pop(newOperand(registerOperand, rbx))
	c.pop(newOperand(registerOperand, rax))
	c.set_of_mul(rax, rbx)

	if c.test_of() {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, arithmeticOverflowMultiplication, c.registers[rip]-1, nil)
	}

	c.registers[rax] = uint64(int64(c.registers[rax]) * int64(c.registers[rbx]))
	c.set_zf(rax)
	c.set_sf(rax)

	c.push(newOperand(registerOperand, rax))
	return nil
}

// Divide two int64 elements from top of stack and store the result onto the stack.
func (c *cpu) div() error {
	c.pop(newOperand(registerOperand, rbx))
	c.pop(newOperand(registerOperand, rax))
	c.set_of_div(rax, rbx)

	if c.registers[rbx] == 0 {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, divisionByZero, c.registers[rip]-1, nil)
	}

	if c.test_of() {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, arithmeticOverflowDivision, c.registers[rip]-1, nil)
	}

	c.registers[rax] = uint64(int64(c.registers[rax]) / int64(c.registers[rbx]))
	c.set_zf(rax)
	c.set_sf(rax)

	c.push(newOperand(registerOperand, rax))
	return nil
}

// Compare two int64 elements from top of stack and set flags register based on result (zero zf, sign sf, overflow of).
func (c *cpu) cmp() {
	c.pop(newOperand(registerOperand, rbx))
	c.pop(newOperand(registerOperand, rax))
	c.set_of_sub(rax, rbx)

	c.registers[rax] = uint64(int64(c.registers[rax]) - int64(c.registers[rbx]))
	c.set_zf(rax)
	c.set_sf(rax)
}

// Unconditionally jump to uint64 address.
func (c *cpu) jmp(op *operand) error {
	switch op.operand {
	case addressOperand:
		c.registers[rip] = op.address

	case registerOperand:
		c.registers[rip] = c.registers[op.register]

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, operationMap[jmp], nil)
	}

	return nil
}

// Jump to uint64 address if zero flag is set, nz (not zero).
func (c *cpu) je(op *operand) error {
	if c.registers[flags]&uint64(zf) != 0 {
		return c.jmp(op)
	}

	return nil
}

// Jump to uint64 address if zero flag is not set, zr (zero).
func (c *cpu) jne(op *operand) error {
	if c.registers[flags]&uint64(zf) == 0 {
		return c.jmp(op)
	}

	return nil
}

// Jump to uint64 address if sign flag is not equal to overflow flag (sf != of).
func (c *cpu) jl(op *operand) error {
	if c.registers[flags]&uint64(sf) != c.registers[flags]&uint64(of) {
		return c.jmp(op)
	}

	return nil
}

// Jump to uint64 address if zero flag is set and sign flag is not equal to overflow flag (zf != 0, sf != of).
func (c *cpu) jle(op *operand) error {
	if c.registers[flags]&uint64(zf) != 0 || c.registers[flags]&uint64(sf) != c.registers[flags]&uint64(of) {
		return c.jmp(op)
	}

	return nil
}

// Jump to uint64 address if zero flag is not set and sign flag is equal to overflow flag (zf == 0, sf == of).
func (c *cpu) jg(op *operand) error {
	if c.registers[flags]&uint64(zf) == 0 && c.registers[flags]&uint64(sf) == c.registers[flags]&uint64(of) {
		return c.jmp(op)
	}

	return nil
}

// Jump to uint64 address if sign flag is equal to overflow flag (sf == of).
func (c *cpu) jge(op *operand) error {
	if c.registers[flags]&uint64(sf) == c.registers[flags]&uint64(of) {
		return c.jmp(op)
	}

	return nil
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

// Call to programming language runtime library.
func (c *cpu) runtime(code runtimeCall) {
	switch code {
	case readln:
		// read integer from stdin
		var input int64

		for {
			fmt.Print("> ")
			_, err := fmt.Scanln(&input)

			if err == nil {
				c.push(newOperand(immediateOperand, input))
				break
			}
		}

	case writeln:
		// write integer to stdout
		c.pop(newOperand(registerOperand, rax))
		fmt.Printf("%v\n", int64(c.registers[rax]))
	}
}
