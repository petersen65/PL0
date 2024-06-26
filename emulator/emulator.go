// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emulator

import (
	"bytes"
	"encoding/gob"
	"encoding/json"
	"fmt"
	"io"
	"math"
	"strings"

	cod "github.com/petersen65/PL0/v2/code"
	cor "github.com/petersen65/PL0/v2/core"
)

const (
	memorySize            = 65536                   // memory entries are 64-bit unsigned integers
	stackSize             = 16384                   // stack entries are 64-bit unsigned integers
	stackForbiddenZone    = 1024                    // stack entries below this address are forbidden to be used
	stackDescriptorSize   = 3                       // size of a stack frame descriptor
	defaultStartLabel     = "_start"                // label for the entry point of the program if none is provided
	createStaticLinkLabel = "rt.create_static_link" // label for runtime library function "create_static_link"
	followStaticLinkLabel = "rt.follow_static_link" // label for runtime library function "follow_static_link"
)

// Operation codes for assembly instructions.
const (
	_ = operationCode(iota)

	// assembly instructions for data copy operations
	push
	pop
	mov

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
	stdcall
)

// Operand types for instructions.
const (
	_                = operandType(iota)
	registerOperand  // 64-bit registers: rax, rbx, rcx, rdx, rsi, rdi, rsp, rbp, r8 to r15
	immediateOperand // int64 constant values like 'mov eax, 1'
	memoryOperand    // memory addresses are specified indirectly through registers
	labelOperand     // labels are used to specify jump targets and must be replaced by absolute addresses before execution
	jumpOperand      // destinations for jump instructions that are specified as absolute addresses
)

// Call codes for the programming language standard library.
const (
	_ = standardCall(iota)
	readln
	writeln
)

const (
	_     = register(iota)
	rax   // accumulator is used for intermediate results of arithmetic operations
	rbx   // base register can be used for addressing variables
	rcx   // counter register can be used for counting iterations of loops
	rdx   // data register can be used for addressing variables
	rsi   // source index register used in string and array operations as a pointer to source data
	rdi   // destination index register is used used in string and array operations as a pointer to destination data
	r8    // 64-bit general purpose register
	r9    // 64-bit general purpose register
	r10   // 64-bit general purpose register
	r11   // 64-bit general purpose register
	r12   // 64-bit general purpose register
	r13   // 64-bit general purpose register
	r14   // 64-bit general purpose register
	r15   // 64-bit general purpose register
	flags // flags register contains the current state of the CPU and reflects the result of arithmetic operations
	rip   // instruction pointer is pointing to the next instruction to be executed
	rsp   // stack pointer is pointing to the top of the stack
	rbp   // base pointer is pointing to the base of the current stack frame
)

const (
	_  = flag(iota)
	zf = 0x0000000000000040 // zero flag is set if the result of an arithmetic operation is zero
	sf = 0x0000000000000080 // sign flag is set if the result of an arithmetic operation is negative
	of = 0x0000000000000800 // overflow flag is set if the result of an arithmetic operation is too large to fit in the register
)

type (
	// Type for operation codes.
	operationCode int32

	// Type of operands.
	operandType int32

	// Type for standard library call codes.
	standardCall int64

	// Text section of the binary target.
	textSection []*instruction

	// Operand of an operation.
	operand struct {
		Operand      operandType `json:"operand"`      // type of the operand
		Register     register    `json:"register"`     // register operand for the operation
		ArgInt       int64       `json:"arg_int"`      // int64 immediate value argument
		Memory       register    `json:"memory"`       // memory address specified indirectly through register
		Label        string      `json:"label"`        // labels for jump instructions will be replaced by an address
		Jump         uint64      `json:"jump"`         // destinations for jump instructions are specified as absolute addresses
		Displacement int64       `json:"displacement"` // used by the memory operand for "base plus displacement" addressing
	}

	// Instruction is the representation of a single operation with all its operands.
	instruction struct {
		Operation operationCode `json:"operation"` // operation code of the instruction
		Operands  []*operand    `json:"operands"`  // operands for the operation
		Labels    []string      `json:"labels"`    // labels to whom jump instructions will jump
	}

	// Enumeration of registers of the CPU.
	register int32

	// Flags that reflect the state of the CPU and the result of arithmetic operations.
	flag uint64

	// Virtual process that holds instructions of a binary target.
	process struct {
		text         textSection // text section with instructions
		stackPointer uint64      // memory address of the downward growing stack
		resolved     bool        // flag to indicate if all labels have been resolved
	}

	// Virtual CPU with its registers.
	cpu struct {
		registers map[register]uint64 // registers of the CPU
	}

	// Virtual machine that can run processes.
	machine struct {
		cpu     cpu      // CPU of the virtual machine
		memory  []uint64 // memory of the virtual machine
		process process  // process running on the virtual machine
	}
)

var (
	// Map operation codes to their string representation.
	operationNames = map[operationCode]string{
		push:    "push",
		pop:     "pop",
		mov:     "mov",
		cmp:     "cmp",
		jmp:     "jmp",
		je:      "je",
		jne:     "jne",
		jl:      "jl",
		jle:     "jle",
		jg:      "jg",
		jge:     "jge",
		neg:     "neg",
		and:     "and",
		add:     "add",
		sub:     "sub",
		imul:    "imul",
		idiv:    "idiv",
		call:    "call",
		ret:     "ret",
		stdcall: "rcall",
	}

	// Map registers to their string representation.
	registerNames = map[register]string{
		rax: "rax",
		rbx: "rbx",
		rcx: "rcx",
		rdx: "rdx",
		rsi: "rsi",
		rdi: "rdi",
		r8:  "r8",
		r9:  "r9",
		r10: "r10",
		r11: "r11",
		r12: "r12",
		r13: "r13",
		r14: "r14",
		r15: "r15",
		rip: "rip",
		rsp: "rsp",
		rbp: "rbp",
	}
)

// Create a new emulation machine with CPU, registers, memory, and stack.
func newMachine() Machine {
	return &machine{
		cpu: cpu{
			registers: make(map[register]uint64),
		},
		memory: make([]uint64, memorySize),
		process: process{
			text:         make(textSection, 0),
			stackPointer: memorySize - 1,
		},
	}
}

// Create a new instruction with an operation code, some labels, and operands.
func newInstruction(op operationCode, labels []string, operands ...*operand) *instruction {
	return &instruction{
		Operation: op,
		Operands:  operands,
		Labels:    labels,
	}
}

// Create a new operand for an instruction.
func newOperand(opType operandType, op any, disp ...int64) *operand {
	switch opType {
	case registerOperand:
		return &operand{Operand: registerOperand, Register: op.(register)}

	case immediateOperand:
		return &operand{Operand: immediateOperand, ArgInt: op.(int64)}

	case memoryOperand:
		if len(disp) > 0 {
			return &operand{Operand: memoryOperand, Memory: op.(register), Displacement: disp[0]}
		}

		return &operand{Operand: memoryOperand, Memory: op.(register)}

	case labelOperand:
		return &operand{Operand: labelOperand, Label: op.(string)}

	case jumpOperand:
		return &operand{Operand: jumpOperand, Jump: op.(uint64)}

	default:
		return &operand{} // empty operand will generate an error when used
	}
}

// String representation of a register.
func (r register) String() string {
	return registerNames[r]
}

// String representation of an operand.
func (o *operand) String() string {
	switch o.Operand {
	case registerOperand:
		return o.Register.String()

	case immediateOperand:
		return fmt.Sprintf("%v", o.ArgInt)

	case memoryOperand:
		if o.Displacement != 0 {
			return fmt.Sprintf("[%v%+d]", o.Memory, o.Displacement)
		}

		return fmt.Sprintf("[%v]", o.Memory)

	case labelOperand:
		return o.Label

	case jumpOperand:
		return fmt.Sprintf("%v", o.Jump)

	default:
		panic(cor.NewGeneralError(cor.Emulator, failureMap, cor.Fatal, unknownInstructionOperand, o.Operand, nil))
	}
}

// String representation of an operation code.
func (oc operationCode) String() string {
	return operationNames[oc]
}

// String representation of an instruction.
func (i *instruction) String() string {
	var buffer bytes.Buffer

	for _, label := range i.Labels {
		buffer.WriteString(label)
		buffer.WriteString(":\n")
	}

	buffer.WriteString(fmt.Sprintf("  %-12v", i.Operation))

	for _, op := range i.Operands {
		buffer.WriteString(op.String())
		buffer.WriteString(", ")
	}

	return strings.TrimSuffix(buffer.String(), ", ")
}

// Import a raw byte slice into the text section of a process.
func (p *process) importRaw(raw []byte) error {
	var buffer bytes.Buffer

	// transfer raw bytes into a binary buffer
	if _, err := buffer.Write(raw); err != nil {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, textSectionImportFailed, nil, err)
	}

	// decode the binary buffer into the text section
	if err := gob.NewDecoder(&buffer).Decode(&p.text); err != nil {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, textSectionImportFailed, nil, err)
	}

	return nil
}

// Append an instruction to the end of the text section of the process.
func (p *process) appendInstruction(op operationCode, labels []string, operands ...*operand) {
	p.text = append(p.text, newInstruction(op, labels, operands...))
}

// Append a set of instructions to create all runtime library functions.
func (p *process) appendRuntimeLibrary() {
	loopCondition := fmt.Sprintf("%v.1", followStaticLinkLabel)
	behindLoop := fmt.Sprintf("%v.2", followStaticLinkLabel)

	// runtime library function "create_static_link"
	p.appendInstruction(mov, []string{createStaticLinkLabel}, newOperand(registerOperand, rcx), newOperand(memoryOperand, rbp, stackDescriptorSize-1))
	p.appendInstruction(mov, nil, newOperand(registerOperand, rbx), newOperand(memoryOperand, rbp))
	p.appendInstruction(call, nil, newOperand(labelOperand, loopCondition))
	p.appendInstruction(mov, nil, newOperand(memoryOperand, rbp, stackDescriptorSize-1), newOperand(registerOperand, rbx))
	p.appendInstruction(ret, nil)

	// runtime library function "follow_static_link"
	p.appendInstruction(mov, []string{followStaticLinkLabel}, newOperand(registerOperand, rbx), newOperand(registerOperand, rbp))
	p.appendInstruction(cmp, []string{loopCondition}, newOperand(registerOperand, rcx), newOperand(immediateOperand, int64(0)))
	p.appendInstruction(je, nil, newOperand(labelOperand, behindLoop))
	p.appendInstruction(mov, nil, newOperand(registerOperand, rbx), newOperand(memoryOperand, rbx, stackDescriptorSize-1))
	p.appendInstruction(sub, nil, newOperand(registerOperand, rcx), newOperand(immediateOperand, int64(1)))
	p.appendInstruction(jmp, nil, newOperand(labelOperand, loopCondition))
	p.appendInstruction(ret, []string{behindLoop})
}

// The linker resolves jump and call label references to absolut code addresses.
func (p *process) linker() error {
	// return without linking if all labels have already been resolved
	if p.resolved {
		return nil
	}

	labels := make(map[string]uint64)

	// create a map of labels and their absolute addresses
	for i := range p.text {
		for _, label := range p.text[i].Labels {
			labels[label] = uint64(i)
		}
	}

	// resolve jump and call label references to absolute code addresses
	for _, asm := range p.text {
		switch asm.Operation {
		case call, jmp, je, jne, jl, jle, jg, jge:
			if address, ok := labels[asm.Operands[0].Label]; !ok {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unresolvedLabelReference, asm.Operands[0].Label, nil)
			} else {
				asm.Operands[0] = newOperand(jumpOperand, address)
			}
		}
	}

	p.resolved = true
	return nil
}

// Load a binary target and return an error if the target import fails.
func (m *machine) Load(raw []byte) error {
	// import a binary target into a process
	if err := m.process.importRaw(raw); err != nil {
		return err
	}

	// a binary target has resolved all labels and can be executed
	m.process.resolved = true
	return nil
}

// Load a module and return an error if the module fails to JIT compile.
func (m *machine) LoadModule(module cod.Module) error {
	// JIT compile the module into a process
	if err := m.process.jitCompile(module); err != nil {
		return err
	}

	// append all runtime library functions
	m.process.appendRuntimeLibrary()
	return nil
}

// Link all assembly instructions of the JIT compiled modules.
func (m *machine) Link() error {
	if err := m.process.linker(); err != nil {
		return err
	}

	return nil
}

// Print a process to the specified writer.
func (m *machine) Print(print io.Writer, args ...any) error {
	if _, err := fmt.Fprintf(print, "global %v\nsection .text\n%v:\n", defaultStartLabel, defaultStartLabel); err != nil {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, textSectionExportFailed, nil, err)
	}

	for _, instr := range m.process.text {
		if _, err := fmt.Fprintf(print, "%v\n", instr); err != nil {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, textSectionExportFailed, nil, err)
		}
	}

	return nil
}

// Export the process that is managed by the virtual machine.
func (m *machine) Export(format cor.ExportFormat, print io.Writer) error {
	switch format {
	case cor.Json:
		// export the process as a JSON object and wrap it in a struct to provide a field name for the process sections
		if raw, err := json.MarshalIndent(struct {
			Text textSection `json:"text_section"`
		}{Text: m.process.text}, "", "  "); err != nil {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, textSectionExportFailed, nil, err)
		} else {
			_, err = print.Write(raw)

			if err != nil {
				err = cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, textSectionExportFailed, nil, err)
			}

			return err
		}

	case cor.Text:
		// print is a convenience function to export the process as a string to the print writer
		return m.Print(print)

	case cor.Binary:
		var buffer bytes.Buffer

		// cannot export binary target before a linking step has resolved all labels
		if !m.process.resolved {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, linkingStepMissing, nil, nil)
		}

		// encode the text section into a binary buffer
		if err := gob.NewEncoder(&buffer).Encode(m.process.text); err != nil {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, textSectionExportFailed, nil, err)
		}

		// transfer the binary buffer to the print writer
		if _, err := buffer.WriteTo(print); err != nil {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, textSectionExportFailed, nil, err)
		}

		return nil

	default:
		panic(cor.NewGeneralError(cor.Emulator, failureMap, cor.Fatal, unknownExportFormat, format, nil))
	}
}

// Run a process and return an error if the process fails to execute.
func (m *machine) RunProcess() error {
	// cannot run the process before a linking step has resolved all labels
	if !m.process.resolved {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, linkingStepMissing, nil, nil)
	}

	m.cpu.registers[rax] = 0   // accumulator register
	m.cpu.registers[rbx] = 0   // base register
	m.cpu.registers[rcx] = 0   // counter register
	m.cpu.registers[rdx] = 0   // data register
	m.cpu.registers[rsi] = 0   // source index register
	m.cpu.registers[rdi] = 0   // destination index register
	m.cpu.registers[r8] = 0    // general purpose register
	m.cpu.registers[r9] = 0    // general purpose register
	m.cpu.registers[r10] = 0   // general purpose register
	m.cpu.registers[r11] = 0   // general purpose register
	m.cpu.registers[r12] = 0   // general purpose register
	m.cpu.registers[r13] = 0   // general purpose register
	m.cpu.registers[r14] = 0   // general purpose register
	m.cpu.registers[r15] = 0   // general purpose register
	m.cpu.registers[flags] = 0 // flags register
	m.cpu.registers[rip] = 0   // instruction pointer

	// initialize stack frame descriptor of main block
	m.memory[m.process.stackPointer] = 0              // static link (compile-time block nesting hierarchy)
	m.memory[m.process.stackPointer-1] = 0            // return address (to caller)
	m.cpu.registers[rsp] = m.process.stackPointer - 1 // stack pointer points to return address before main block's prelude runs
	m.cpu.registers[rbp] = 0                          // intentionnaly, there is no valid base pointer (from a caller)

	// execute instructions until main block return to external code
	for {
		if m.cpu.registers[rip] >= uint64(len(m.process.text)) {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, addressOutOfRange, m.cpu.registers[rip], nil)
		}

		// stack address space is from 'stackPointer' down to 'stackPointer - stackSize + 1' excluding a forbidden zone
		if m.cpu.registers[rsp] <= m.process.stackPointer-stackSize+stackForbiddenZone ||
			m.cpu.registers[rsp] > m.process.stackPointer {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, stackOverflow, m.cpu.registers[rsp], nil)
		}

		instr := m.process.text[m.cpu.registers[rip]]
		m.cpu.registers[rip]++

		switch instr.Operation {
		case push: // push operand on top of stack
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, push, nil)
			}

			if err := m.push(instr.Operands[0]); err != nil {
				return err
			}

		case pop: // pop element from top of stack into operand
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, pop, nil)
			}

			if err := m.pop(instr.Operands[0]); err != nil {
				return err
			}

		case mov: // copy second operand to first operand
			if len(instr.Operands) != 2 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, mov, nil)
			}

			if err := m.mov(instr.Operands[0], instr.Operands[1]); err != nil {
				return err
			}

		case jmp: // unconditionally jump to uint64 address
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, jmp, nil)
			}

			if err := m.cpu.jmp(instr.Operands[0]); err != nil {
				return err
			}

		case je: // jump to uint64 address if last comparison was equal
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, je, nil)
			}

			if err := m.cpu.je(instr.Operands[0]); err != nil {
				return err
			}

		case jne: // jump to uint64 address if last comparison was not equal
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, jne, nil)
			}

			if err := m.cpu.jne(instr.Operands[0]); err != nil {
				return err
			}

		case jl: // jump to uint64 address if last comparison was less than
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, jl, nil)
			}

			if err := m.cpu.jl(instr.Operands[0]); err != nil {
				return err
			}

		case jle: // jump to uint64 address if last comparison was less than or equal to
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, jle, nil)
			}

			if err := m.cpu.jle(instr.Operands[0]); err != nil {
				return err
			}

		case jg: // jump to uint64 address if last comparison was greater than
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, jg, nil)
			}

			if err := m.cpu.jg(instr.Operands[0]); err != nil {
				return err
			}

		case jge: // jump to uint64 address if last comparison was greater than or equal to
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, jge, nil)
			}

			if err := m.cpu.jge(instr.Operands[0]); err != nil {
				return err
			}

		case neg: // negate int64 element
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, neg, nil)
			}

			if err := m.cpu.neg(instr.Operands[0]); err != nil {
				return err
			}

		case and: // test if element is odd and set zero flag if it is
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, and, nil)
			}

			if err := m.cpu.and(instr.Operands[0], 1); err != nil {
				return err
			}

		case add: // add two int64 elements and store the result
			if len(instr.Operands) != 2 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, add, nil)
			}

			if err := m.cpu.add(instr.Operands[0], instr.Operands[1]); err != nil {
				return err
			}

		case sub: // subtract two int64 elements and store the result
			if len(instr.Operands) != 2 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, sub, nil)
			}

			if err := m.cpu.sub(instr.Operands[0], instr.Operands[1]); err != nil {
				return err
			}

		case imul: // multiply two int64 elements and store the result
			if len(instr.Operands) != 2 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, imul, nil)
			}

			if err := m.cpu.imul(instr.Operands[0], instr.Operands[1]); err != nil {
				return err
			}

		case idiv: // divide two int64 elements and store the result
			if len(instr.Operands) != 2 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, idiv, nil)
			}

			if err := m.cpu.idiv(instr.Operands[0], instr.Operands[1]); err != nil {
				return err
			}

		case cmp: // compare two int64 elements and set flags register based on result
			if len(instr.Operands) != 2 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, cmp, nil)
			}

			if err := m.cpu.cmp(instr.Operands[0], instr.Operands[1]); err != nil {
				return err
			}

		case call: // caller function calls callee function
			if len(instr.Operands) != 1 {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, call, nil)
			}

			// call function at uint64 address
			if err := m.call(instr.Operands[0]); err != nil {
				return err
			}

		case ret: // callee function returns to caller function
			if err := m.ret(); err != nil {
				return err
			}

			// returning from the entrypoint of the program exits the program
			if m.cpu.registers[rip] == 0 {
				return nil
			}

		case stdcall: // call to programming language standard library based on standard call code
			if len(instr.Operands) != 1 || instr.Operands[0].Operand != immediateOperand {
				return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, stdcall, nil)
			}

			// call standard library function via call code
			if err := m.standard(standardCall(instr.Operands[0].ArgInt)); err != nil {
				return err
			}

		default:
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unknownOperation, m.cpu.registers[rip]-1, nil)
		}
	}
}

// Call a programming language standard library function.
func (m *machine) standard(code standardCall) error {
	switch code {
	case readln:
		// read integer from stdin
		var input int64

		for {
			fmt.Print("> ")
			_, err := fmt.Scanln(&input)

			if err == nil {
				m.push(newOperand(immediateOperand, input))
				break
			}
		}

	case writeln:
		// write integer to stdout
		m.pop(newOperand(registerOperand, rax))
		fmt.Printf("%v\n", int64(m.cpu.registers[rax]))

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unknownStandardCallCode, code, nil)
	}

	return nil
}

// Push operand on top of stack, top of stack points to new operand.
func (m *machine) push(op *operand) error {
	var arg uint64

	switch op.Operand {
	case registerOperand:
		arg = m.cpu.registers[op.Register]

	case immediateOperand:
		arg = uint64(op.ArgInt)

	case jumpOperand:
		arg = op.Jump

	case memoryOperand:
		arg = m.memory[int64(m.cpu.registers[op.Memory])+op.Displacement]

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, push, nil)
	}

	m.cpu.registers[rsp]--
	m.memory[m.cpu.registers[rsp]] = arg
	return nil
}

// Pop element from top of stack into operand, top of stack points to previous element.
func (m *machine) pop(op *operand) error {
	switch op.Operand {
	case registerOperand:
		m.cpu.registers[op.Register] = m.memory[m.cpu.registers[rsp]]

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, pop, nil)
	}

	m.cpu.registers[rsp]++
	return nil
}

// Caller function calls callee function.
func (m *machine) call(op *operand) error {
	// push return address (instruction pointer of caller + 1)
	m.push(newOperand(jumpOperand, m.cpu.registers[rip]))

	// jump to function at uint64 address
	return m.cpu.jmp(op)
}

// Callee function returns to caller function.
func (m *machine) ret() error {
	// restore callers instruction pointer
	return m.pop(newOperand(registerOperand, rip))
}

// Copy second operand to first operand.
func (m *machine) mov(a, b *operand) error {
	switch {
	case a.Operand == registerOperand && b.Operand == registerOperand:
		m.cpu.registers[a.Register] = m.cpu.registers[b.Register]

	case a.Operand == registerOperand && b.Operand == immediateOperand:
		m.cpu.registers[a.Register] = uint64(b.ArgInt)

	case a.Operand == registerOperand && b.Operand == memoryOperand:
		m.cpu.registers[a.Register] = m.memory[int64(m.cpu.registers[b.Memory])+b.Displacement]

	case a.Operand == memoryOperand && b.Operand == registerOperand:
		m.memory[int64(m.cpu.registers[a.Memory])+a.Displacement] = m.cpu.registers[b.Register]

	case a.Operand == memoryOperand && b.Operand == immediateOperand:
		m.memory[int64(m.cpu.registers[a.Memory])+a.Displacement] = uint64(b.ArgInt)

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, mov, nil)
	}

	return nil
}

// Negate int64 element.
func (c *cpu) neg(op *operand) error {
	switch op.Operand {
	case registerOperand:
		c.set_of_neg(int64(c.registers[op.Register]))

		if c.test_of() {
			return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, arithmeticOverflowNegation, c.registers[rip]-1, nil)
		}

		c.registers[op.Register] = uint64(-int64(c.registers[op.Register]))
		c.set_zf(int64(c.registers[op.Register]))
		c.set_sf(int64(c.registers[op.Register]))

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, neg, nil)
	}

	return nil
}

// Perform bitwise 'and' operation with uint64 element and uint64 argument.
func (c *cpu) and(op *operand, arg uint64) error {
	switch op.Operand {
	case registerOperand:
		c.registers[op.Register] = c.registers[op.Register] & arg

		c.set_zf(int64(c.registers[op.Register]))
		c.set_sf(int64(c.registers[op.Register]))
		c.unset_of()

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, and, nil)
	}

	return nil
}

// Add two int64 elements and store the result.
func (c *cpu) add(a, b *operand) error {
	var arg_b int64

	switch {
	case a.Operand == registerOperand && b.Operand == registerOperand:
		arg_b = int64(c.registers[b.Register])

	case a.Operand == registerOperand && b.Operand == immediateOperand:
		arg_b = b.ArgInt

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, add, nil)
	}

	c.set_of_add(int64(c.registers[a.Register]), arg_b)

	if c.test_of() {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, arithmeticOverflowAddition, c.registers[rip]-1, nil)
	}

	c.registers[a.Register] = uint64(int64(c.registers[a.Register]) + arg_b)
	c.set_zf(int64(c.registers[a.Register]))
	c.set_sf(int64(c.registers[a.Register]))
	return nil
}

// Subtract two int64 elements and store the result.
func (c *cpu) sub(a, b *operand) error {
	var arg_b int64

	switch {
	case a.Operand == registerOperand && b.Operand == registerOperand:
		arg_b = int64(c.registers[b.Register])

	case a.Operand == registerOperand && b.Operand == immediateOperand:
		arg_b = b.ArgInt

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, sub, nil)
	}

	c.set_of_sub(int64(c.registers[a.Register]), arg_b)

	if c.test_of() {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, arithmeticOverflowSubtraction, c.registers[rip]-1, nil)

	}

	c.registers[a.Register] = uint64(int64(c.registers[a.Register]) - arg_b)
	c.set_zf(int64(c.registers[a.Register]))
	c.set_sf(int64(c.registers[a.Register]))
	return nil
}

// Multiply two int64 elements and store the result.
func (c *cpu) imul(a, b *operand) error {
	var arg_b int64

	switch {
	case a.Operand == registerOperand && b.Operand == registerOperand:
		arg_b = int64(c.registers[b.Register])

	case a.Operand == registerOperand && b.Operand == immediateOperand:
		arg_b = b.ArgInt

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, imul, nil)
	}

	c.set_of_mul(int64(c.registers[a.Register]), arg_b)

	if c.test_of() {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, arithmeticOverflowMultiplication, c.registers[rip]-1, nil)
	}

	c.registers[a.Register] = uint64(int64(c.registers[a.Register]) * arg_b)
	c.set_zf(int64(c.registers[a.Register]))
	c.set_sf(int64(c.registers[a.Register]))
	return nil
}

// Divide two int64 elements and store the result.
func (c *cpu) idiv(a, b *operand) error {
	var arg_b int64

	switch {
	case a.Operand == registerOperand && b.Operand == registerOperand:
		arg_b = int64(c.registers[b.Register])

	case a.Operand == registerOperand && b.Operand == immediateOperand:
		arg_b = b.ArgInt

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, idiv, nil)
	}

	c.set_of_div(int64(c.registers[a.Register]), arg_b)

	if arg_b == 0 {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, divisionByZero, c.registers[rip]-1, nil)
	}

	if c.test_of() {
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, arithmeticOverflowDivision, c.registers[rip]-1, nil)
	}

	c.registers[a.Register] = uint64(int64(c.registers[a.Register]) / arg_b)
	c.set_zf(int64(c.registers[a.Register]))
	c.set_sf(int64(c.registers[a.Register]))
	return nil
}

// Compare two int64 elements from top of stack and set flags register based on result (zero zf, sign sf, overflow of).
func (c *cpu) cmp(a, b *operand) error {
	var arg_b int64

	switch {
	case a.Operand == registerOperand && b.Operand == registerOperand:
		arg_b = int64(c.registers[b.Register])

	case a.Operand == registerOperand && b.Operand == immediateOperand:
		arg_b = b.ArgInt

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, cmp, nil)
	}

	c.set_of_sub(int64(c.registers[a.Register]), arg_b)

	c.registers[a.Register] = uint64(int64(c.registers[a.Register]) - arg_b)
	c.set_zf(int64(c.registers[a.Register]))
	c.set_sf(int64(c.registers[a.Register]))
	return nil
}

// Unconditionally jump to uint64 address.
func (c *cpu) jmp(op *operand) error {
	switch op.Operand {
	case registerOperand:
		c.registers[rip] = c.registers[op.Register]

	case jumpOperand:
		c.registers[rip] = op.Jump

	default:
		return cor.NewGeneralError(cor.Emulator, failureMap, cor.Error, unsupportedOperand, jmp, nil)
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

// Set zero flag if int64 element is zero.
func (c *cpu) set_zf(a int64) {
	if a == 0 {
		c.registers[flags] |= uint64(zf)
	} else {
		c.registers[flags] &= ^uint64(zf)
	}
}

// Set sign flag if int64 element is negative.
func (c *cpu) set_sf(a int64) {
	if a < 0 {
		c.registers[flags] |= uint64(sf)
	} else {
		c.registers[flags] &= ^uint64(sf)
	}
}

// Set overflow flag if negation of the int64 element overflows.
func (c *cpu) set_of_neg(a int64) {
	if a == math.MinInt64 {
		c.registers[flags] |= uint64(of)
	} else {
		c.registers[flags] &= ^uint64(of)
	}
}

// Set overflow flag if addition of two int64 elements overflows.
func (c *cpu) set_of_add(a, b int64) {
	s := a + b

	if (a > 0 && b > 0 && s < a) || (a < 0 && b < 0 && s > a) {
		c.registers[flags] |= uint64(of)
	} else if (a > 0 && b < 0 && s > a) || (a < 0 && b > 0 && s < a) {
		c.registers[flags] |= uint64(of)
	} else {
		c.registers[flags] &= ^uint64(of)
	}
}

// Set overflow flag if subtraction of two int64 elements overflows.
func (c *cpu) set_of_sub(a, b int64) {
	s := a - b

	if (a > 0 && b < 0 && s < a) || (a < 0 && b > 0 && s > a) {
		c.registers[flags] |= uint64(of)
	} else if (a > 0 && b > 0 && s > a) || (a < 0 && b < 0 && s < a) {
		c.registers[flags] |= uint64(of)
	} else {
		c.registers[flags] &= ^uint64(of)
	}
}

// Set overflow flag if multiplication of two int64 elements verflows.
func (c *cpu) set_of_mul(a, b int64) {
	s := a * b

	if a != 0 && s/a != b {
		c.registers[flags] |= uint64(of)
	} else {
		c.registers[flags] &= ^uint64(of)
	}
}

// Set overflow flag if division of two int64 elements overflows.
func (c *cpu) set_of_div(a, b int64) {
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
