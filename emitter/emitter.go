// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emitter

import (
	"bytes"
	"container/list"
	"fmt"
	"strconv"
	"strings"

	cod "github.com/petersen65/PL0/v2/code"
	cor "github.com/petersen65/PL0/v2/core"
)

const (
	integerBitSize        = 64                      // number of bits of a signed integer
	descriptorSize        = 3                       // size of an activation record descriptor
	createStaticLinkLabel = "rt.create_static_link" // label for runtime library function "create_static_link"
	followStaticLinkLabel = "rt.follow_static_link" // label for runtime library function "follow_static_link"
)

// Private implementation of the assembly code emitter.
type emitter struct {
	target   CentralProcessingUnit // target CPU for the emitter
	text     TextSection           // text section with assembly instructions
	resolved bool                  // flag to indicate if all labels have been resolved
}

var (
	// Map CPU targets to their string representation.
	cpuNames = map[CentralProcessingUnit]string{
		Amd64: "amd64",
	}

	// Map operation codes to their string representation.
	operationNames = map[OperationCode]string{
		Push:    "push",
		Pop:     "pop",
		Mov:     "mov",
		Cmp:     "cmp",
		Jmp:     "jmp",
		Je:      "je",
		Jne:     "jne",
		Jl:      "jl",
		Jle:     "jle",
		Jg:      "jg",
		Jge:     "jge",
		Neg:     "neg",
		And:     "and",
		Add:     "add",
		Sub:     "sub",
		Imul:    "imul",
		Idiv:    "idiv",
		Call:    "call",
		Ret:     "ret",
		StdCall: "stdcall",
	}

	// Map registers to their string representation.
	registerNames = map[Register]string{
		Rax: "rax",
		Rbx: "rbx",
		Rcx: "rcx",
		Rdx: "rdx",
		Rsi: "rsi",
		Rdi: "rdi",
		R8:  "r8",
		R9:  "r9",
		R10: "r10",
		R11: "r11",
		R12: "r12",
		R13: "r13",
		R14: "r14",
		R15: "r15",
		Rip: "rip",
		Rsp: "rsp",
		Rbp: "rbp",
	}
)

// Return the public interface of the private emitter implementation.
func newEmitter(cpu CentralProcessingUnit) Emitter {
	if cpu != Amd64 {
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedCpuTarget, cpu, nil))
	}

	return &emitter{
		target: cpu,
		text:   make(TextSection, 0),
	}
}

// Create a new instruction with an operation code, some labels, and operands.
func newInstruction(op OperationCode, labels []string, operands ...*Operand) *Instruction {
	return &Instruction{
		Operation: op,
		Operands:  operands,
		Labels:    labels,
	}
}

// Create a new operand for an instruction.
func newOperand(opType OperandType, op any, disp ...int64) *Operand {
	switch opType {
	case RegisterOperand:
		return &Operand{Operand: RegisterOperand, Register: op.(Register)}

	case ImmediateOperand:
		return &Operand{Operand: ImmediateOperand, ArgInt: op.(int64)}

	case MemoryOperand:
		if len(disp) > 0 {
			return &Operand{Operand: MemoryOperand, Memory: op.(Register), Displacement: disp[0]}
		}

		return &Operand{Operand: MemoryOperand, Memory: op.(Register)}

	case LabelOperand:
		return &Operand{Operand: LabelOperand, Label: op.(string)}

	case JumpOperand:
		return &Operand{Operand: JumpOperand, Jump: op.(uint64)}

	default:
		return &Operand{} // empty operand will generate an error when used
	}
}

// String representation of a CPU target.
func (cpu CentralProcessingUnit) String() string {
	return cpuNames[cpu]
}

// String representation of an operation code.
func (oc OperationCode) String() string {
	return operationNames[oc]
}

// String representation of a register.
func (r Register) String() string {
	return registerNames[r]
}

// String representation of an operand.
func (o *Operand) String() string {
	switch o.Operand {
	case RegisterOperand:
		return o.Register.String()

	case ImmediateOperand:
		return fmt.Sprintf("%v", o.ArgInt)

	case MemoryOperand:
		if o.Displacement != 0 {
			return fmt.Sprintf("[%v%+d]", o.Memory, o.Displacement)
		}

		return fmt.Sprintf("[%v]", o.Memory)

	case LabelOperand:
		return o.Label

	case JumpOperand:
		return fmt.Sprintf("%v", o.Jump)

	default:
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unknownInstructionOperand, o.Operand, nil))
	}
}

// String representation of an instruction.
func (i *Instruction) String() string {
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

// Emit assembly code for the CPU target.
func (e *emitter) Emit(module cod.Module) (text TextSection, err error) {
	e.text = make(TextSection, 0)
	iterator := module.GetIterator()

	// compile-time parameters list for standard library function calls (procedures do not support parameters yet)
	parameters := list.New()

	// protect the emitter against non-valid structured intermediate code
	defer func() {
		if r := recover(); r != nil {
			err = cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, recoveredFromIllegalIntermediateCode, r, nil)
		}
	}()

	// perform an assembly instruction selection for each intermediate code instruction
	for i, l := iterator.First(), make([]string, 0); i != nil; i = iterator.Next() {
		switch i.Code.Operation {
		// append labels for the directly following instruction
		case cod.Target:
			l = append(l, i.Label)

		case cod.Allocate: // allocate space in an activation record for all local variables
			// group consecutive intermediate code allocate operations into one alloc instruction
			for j := 0; ; j++ {
				if iterator.Peek(j).Code.Operation != cod.Allocate {
					e.appendInstruction(Sub, l, newOperand(RegisterOperand, Rsp), newOperand(ImmediateOperand, int64(j+1)))
					iterator.Skip(j)
					break
				}
			}

		case cod.Prelude: // function body prelude
			// save caller's base pointer because it will be changed
			// this creates a 'dynamic link' chain of base pointers so that each callee knows the base pointer of its caller
			// an alternative naming from literature is 'control link' that points to the activation record of the caller
			e.appendInstruction(Push, l, newOperand(RegisterOperand, Rbp))

			// new base pointer points to start of local variables in the activation record
			e.appendInstruction(Mov, nil, newOperand(RegisterOperand, Rbp), newOperand(RegisterOperand, Rsp))

			// call runtime library function to create static link which provides the compile-time block nesting hierarchy at runtime
			e.appendInstruction(Call, nil, newOperand(LabelOperand, createStaticLinkLabel))

		case cod.Epilog: // function body epilog
			// clean allocated local variables from the activation record
			e.appendInstruction(Mov, l, newOperand(RegisterOperand, Rsp), newOperand(RegisterOperand, Rbp))

			// restore caller's base pointer
			e.appendInstruction(Pop, nil, newOperand(RegisterOperand, Rbp))

		case cod.ValueCopy: // push an immediate value onto the runtime control stack
			if i.Code.Arg1.Variant != cod.Literal {
				err = cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, unsupportedOperandVariant, i.Code.Arg1.Variant, nil)
				return
			}

			switch i.Code.Arg1.DataType {
			case cod.Integer64:
				if arg, err := strconv.ParseInt(i.Code.Arg1.Name, 10, integerBitSize); err != nil {
					return nil, newParsingError(i.Code.Arg1.Name, err)
				} else {
					e.appendInstruction(Push, l, newOperand(ImmediateOperand, arg))
				}

			default:
				return nil, validateDataType(cod.Integer64, i.Code.Arg1.DataType)
			}

		default:
			err = cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, unknownIntermediateCodeOperation, i.Code.Operation, nil)
			return
		}

		// collected labels must be used by the directly following instruction (one instruction consumes all collected labels)
		if i.Code.Operation != cod.Target {
			l = make([]string, 0)
		}
	}

	text = e.text
	return
}

// Append an instruction to the end of the text section with the given operation code, labels, and operands.
func (e *emitter) appendInstruction(op OperationCode, labels []string, operands ...*Operand) {
	e.text = append(e.text, newInstruction(op, labels, operands...))
}

// Append a set of instructions to create all runtime library functions.
func (e *emitter) appendRuntimeLibrary() {
	loopCondition := fmt.Sprintf("%v.1", followStaticLinkLabel)
	behindLoop := fmt.Sprintf("%v.2", followStaticLinkLabel)

	// runtime library function "create_static_link"
	e.appendInstruction(Mov, []string{createStaticLinkLabel}, newOperand(RegisterOperand, Rcx), newOperand(MemoryOperand, Rbp, descriptorSize-1))
	e.appendInstruction(Mov, nil, newOperand(RegisterOperand, Rbx), newOperand(MemoryOperand, Rbp))
	e.appendInstruction(Call, nil, newOperand(LabelOperand, loopCondition))
	e.appendInstruction(Mov, nil, newOperand(MemoryOperand, Rbp, descriptorSize-1), newOperand(RegisterOperand, Rbx))
	e.appendInstruction(Ret, nil)

	// runtime library function "follow_static_link"
	e.appendInstruction(Mov, []string{followStaticLinkLabel}, newOperand(RegisterOperand, Rbx), newOperand(RegisterOperand, Rbp))
	e.appendInstruction(Cmp, []string{loopCondition}, newOperand(RegisterOperand, Rcx), newOperand(ImmediateOperand, int64(0)))
	e.appendInstruction(Je, nil, newOperand(LabelOperand, behindLoop))
	e.appendInstruction(Mov, nil, newOperand(RegisterOperand, Rbx), newOperand(MemoryOperand, Rbx, descriptorSize-1))
	e.appendInstruction(Sub, nil, newOperand(RegisterOperand, Rcx), newOperand(ImmediateOperand, int64(1)))
	e.appendInstruction(Jmp, nil, newOperand(LabelOperand, loopCondition))
	e.appendInstruction(Ret, []string{behindLoop})
}

// The linker resolves jump and call label references to absolut code addresses.
func (e *emitter) linker() error {
	// return without linking if all labels have already been resolved
	if e.resolved {
		return nil
	}

	labels := make(map[string]uint64)

	// create a map of labels and their absolute addresses
	for i := range e.text {
		for _, label := range e.text[i].Labels {
			labels[label] = uint64(i)
		}
	}

	// resolve jump and call label references to absolute code addresses
	for _, asm := range e.text {
		switch asm.Operation {
		case Call, Jmp, Je, Jne, Jl, Jle, Jg, Jge:
			if address, ok := labels[asm.Operands[0].Label]; !ok {
				return cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, unresolvedLabelReference, asm.Operands[0].Label, nil)
			} else {
				asm.Operands[0] = newOperand(JumpOperand, address)
			}
		}
	}

	e.resolved = true
	return nil
}

// Validate the data type of the actuals and return an error if the data type is not as expected.
func validateDataType(expected cod.DataType, actuals ...cod.DataType) error {
	for _, actual := range actuals {
		if actual != expected {
			return cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, unsupportedOperandDataType, actual, nil)
		}
	}

	return nil
}

// Create a new parsing error with the given value and inner error.
func newParsingError(value any, inner error) error {
	return cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, operandParsingError, value, inner)
}
