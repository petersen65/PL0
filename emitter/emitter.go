// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emitter

import (
	"bytes"
	"container/list"
	"fmt"
	"strings"

	cod "github.com/petersen65/PL0/v2/code"
	cor "github.com/petersen65/PL0/v2/core"
)

const (
	descriptorSize        = 3                       // size of an activation record descriptor
	createStaticLinkLabel = "rt.create_static_link" // label for runtime library function "create_static_link"
	followStaticLinkLabel = "rt.follow_static_link" // label for runtime library function "follow_static_link"
)

// Private implementation of the assembly code emitter.
type emitter struct {
	target   CentralProcessingUnit // target CPU for the emitter
	text     TextSection           // text section with assembly instructions
	resolved bool                  // flag to indicate if all labels have been resolved in the text section
}

var (
	// Map CPU targets to their string representation.
	cpuNames = map[CentralProcessingUnit]string{
		Amd64: "amd64",
	}

	// Map CPU operation codes to their string representation.
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

	// Map CPU registers to their string representation.
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

// Create a new assembly instruction with an operation code, some labels, and operands.
func newInstruction(op OperationCode, labels []string, operands ...*Operand) *Instruction {
	return &Instruction{
		Operation: op,
		Operands:  operands,
		Labels:    labels,
	}
}

// Create a new operand for an assembly instruction.
func newOperand(kind OperandKind, value any, displacement ...int64) *Operand {
	switch kind {
	case RegisterOperand:
		return &Operand{OperandKind: RegisterOperand, Register: value.(Register)}

	case ImmediateOperand:
		return &Operand{OperandKind: ImmediateOperand, ArgInt: value.(int64)}

	case MemoryOperand:
		if len(displacement) > 0 {
			return &Operand{OperandKind: MemoryOperand, Memory: value.(Register), Displacement: displacement[0]}
		}

		return &Operand{OperandKind: MemoryOperand, Memory: value.(Register)}

	case LabelOperand:
		return &Operand{OperandKind: LabelOperand, Label: value.(string)}

	case JumpOperand:
		return &Operand{OperandKind: JumpOperand, Jump: value.(uint64)}

	default:
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unknownKindOfOperandInCpuOperation, kind, nil))
	}
}

// String representation of a CPU target.
func (cpu CentralProcessingUnit) String() string {
	return cpuNames[cpu]
}

// String representation of a CPU operation code.
func (oc OperationCode) String() string {
	return operationNames[oc]
}

// String representation of a CPU register.
func (r Register) String() string {
	return registerNames[r]
}

// String representation of an operand kind of CPU operations.
func (o *Operand) String() string {
	switch o.OperandKind {
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
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unknownKindOfOperandInCpuOperation, o.OperandKind, nil))
	}
}

// String representation of an assembly instruction.
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
func (e *emitter) Emit(module cod.Module) TextSection {
	e.text = make(TextSection, 0)
	iterator := module.GetIterator()

	// compile-time parameters list for standard library function calls (procedures do not support parameters yet)
	parameters := list.New()

	// perform an assembly instruction selection for each intermediate code instruction
	for i, l := iterator.First(), make([]string, 0); i != nil; i = iterator.Next() {
		// panic if the intermediate code instruction has not a valid addresses contract
		i.Code.ValidateAddressesContract()

		switch i.Code.Operation {
		case cod.Target: // append labels for the directly following non 'Target' instruction
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
			// panic if parsing of the literal into its value fails (unsupported value or data type)
			value := i.Code.Arg1.Parse()
			e.appendInstruction(Push, l, newOperand(ImmediateOperand, value))

		case cod.VariableLoad: // load a variable from its runtime control stack address onto the top of the stack
			// panic if parsing of the variable into its location fails (unsupported data type)
			location := i.Code.Arg1.Parse().(uint64)

			if i.DepthDifference == 0 {
				// push memory content at 'variables base - variable offset' onto runtime control stack
				e.appendInstruction(Push, l, newOperand(MemoryOperand, Rbp, -int64(location)))
			} else {
				// block nesting depth difference between variable use and variable declaration
				e.appendInstruction(Mov, l,
					newOperand(RegisterOperand, Rcx),
					newOperand(ImmediateOperand, int64(i.DepthDifference)))

				// call runtime library function to follow static link to determine the 'variables base' pointer
				e.appendInstruction(Call, nil, newOperand(LabelOperand, followStaticLinkLabel))

				// push memory content at 'variables base - variable offset' onto runtime control stack
				e.appendInstruction(Push, nil, newOperand(MemoryOperand, Rbx, -int64(location)))
			}

		case cod.VariableStore: // store the top of the runtime control stack into a variable's stack address
			// panic if parsing of the variable into its location fails (unsupported data type)
			location := i.Code.Result.Parse().(uint64)

			if i.DepthDifference == 0 {
				// pop content of the variable
				e.appendInstruction(Pop, l, newOperand(RegisterOperand, Rax))

				// copy content of the variable into memory location 'variables base - variable offset'
				e.appendInstruction(Mov, nil,
					newOperand(MemoryOperand, Rbp, -int64(location)),
					newOperand(RegisterOperand, Rax))
			} else {
				// block nesting depth difference between variable use and variable declaration
				e.appendInstruction(Mov, l,
					newOperand(RegisterOperand, Rcx),
					newOperand(ImmediateOperand, int64(i.DepthDifference)))

				// call runtime library function to follow static link to determine the 'variables base' pointer
				e.appendInstruction(Call, nil, newOperand(LabelOperand, followStaticLinkLabel))

				// pop content of the variable
				e.appendInstruction(Pop, nil, newOperand(RegisterOperand, Rax))

				// copy content of the variable into memory location 'variables base - variable offset'
				e.appendInstruction(Mov, nil,
					newOperand(MemoryOperand, Rbx, -int64(location)),
					newOperand(RegisterOperand, Rax))
			}

		case cod.Negate: // negate the top of the runtime control stack and leave the result on the stack
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.Code.Arg1.Parse()

			e.appendInstruction(Pop, l, newOperand(RegisterOperand, Rax))
			e.appendInstruction(Neg, nil, newOperand(RegisterOperand, Rax))
			e.appendInstruction(Push, nil, newOperand(RegisterOperand, Rax))

		case cod.Odd: // check if the top of the runtime control stack is an odd number and leave the result in the CPU flags register
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.Code.Arg1.Parse()

			e.appendInstruction(Pop, l, newOperand(RegisterOperand, Rax))
			e.appendInstruction(And, nil, newOperand(RegisterOperand, Rax))

		case cod.Plus, cod.Minus, cod.Times, cod.Divide: // perform an arithmetic operation on the top two elements of the runtime control stack and replace them with one result on the stack
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.Code.Arg1.Parse()
			_ = i.Code.Arg2.Parse()

			e.appendInstruction(Pop, l, newOperand(RegisterOperand, Rbx))
			e.appendInstruction(Pop, nil, newOperand(RegisterOperand, Rax))

			switch i.Code.Operation {
			case cod.Plus:
				e.appendInstruction(Add, nil, newOperand(RegisterOperand, Rax), newOperand(RegisterOperand, Rbx))

			case cod.Minus:
				e.appendInstruction(Sub, nil, newOperand(RegisterOperand, Rax), newOperand(RegisterOperand, Rbx))

			case cod.Times:
				e.appendInstruction(Imul, nil, newOperand(RegisterOperand, Rax), newOperand(RegisterOperand, Rbx))

			case cod.Divide:
				e.appendInstruction(Idiv, nil, newOperand(RegisterOperand, Rax), newOperand(RegisterOperand, Rbx))
			}

			e.appendInstruction(Push, nil, newOperand(RegisterOperand, Rax))

		case cod.Equal, cod.NotEqual, cod.Less, cod.LessEqual, cod.Greater, cod.GreaterEqual: // compare the top two elements of the runtime control stack, remove them, and leave the result in the CPU flags register
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.Code.Arg1.Parse()
			_ = i.Code.Arg2.Parse()

			e.appendInstruction(Pop, l, newOperand(RegisterOperand, Rbx))
			e.appendInstruction(Pop, nil, newOperand(RegisterOperand, Rax))
			e.appendInstruction(Cmp, nil, newOperand(RegisterOperand, Rax), newOperand(RegisterOperand, Rbx))

		case cod.Jump: // unconditionally jump to a label that is resolved by the linker
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg1.Parse().(string)
			e.appendInstruction(Jmp, l, newOperand(LabelOperand, name))

		case cod.JumpEqual: // jump to a label if the CPU flags register indicates that the top two elements of the stack were equal
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg1.Parse().(string)
			e.appendInstruction(Je, l, newOperand(LabelOperand, name))

		case cod.JumpNotEqual: // jump to a label if the CPU flags register indicates that the top two elements of the stack were not equal
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg1.Parse().(string)
			e.appendInstruction(Jne, l, newOperand(LabelOperand, name))

		case cod.JumpLess: // jump to a label if the CPU flags register indicates that the first element of the stack was less than the second top element
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg1.Parse().(string)
			e.appendInstruction(Jl, l, newOperand(LabelOperand, name))

		case cod.JumpLessEqual: // jump to a label if the CPU flags register indicates that the first element of the stack was less than or equal to the second top element
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg1.Parse().(string)
			e.appendInstruction(Jle, l, newOperand(LabelOperand, name))

		case cod.JumpGreater: // jump to a label if the CPU flags register indicates that the first element of the stack was greater than the second top element
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg1.Parse().(string)
			e.appendInstruction(Jg, l, newOperand(LabelOperand, name))

		case cod.JumpGreaterEqual: // jump to a label if the CPU flags register indicates that the first element of the stack was greater than or equal to the second top element
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg1.Parse().(string)
			e.appendInstruction(Jge, l, newOperand(LabelOperand, name))

		case cod.Parameter: // push a parameter onto the compile-time parameters list for a standard library function call
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.Code.Arg1.Parse()
			parameters.PushBack(i)

		case cod.Call: // call a function with 0 arguments by jumping to the function's label
			// panic if parsing of the parameters count into an unsigned integer fails (unsupported value or data type)
			count := i.Code.Arg1.Parse().(uint64)

			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg2.Parse().(string)

			if count != 0 {
				// procedures do not support parameters yet
				panic(cor.NewGeneralError(cor.Emulator, failureMap, cor.Fatal, unexpectedNumberOfFunctionArguments, nil, nil))
			} else {
				// push difference between use depth and declaration depth on runtime control stack
				e.appendInstruction(Push, l, newOperand(ImmediateOperand, int64(i.DepthDifference)))

				// push return address on runtime control stack and jump to callee
				e.appendInstruction(Call, nil, newOperand(LabelOperand, name))

				// remove difference from runtime control stack after return from callee
				e.appendInstruction(Add, nil, newOperand(RegisterOperand, Rsp), newOperand(ImmediateOperand, int64(1)))
			}

		case cod.Return: // return from a function to its caller
			e.appendInstruction(Ret, nil)

		case cod.Standard:
			// a standard function represents a function that is provided by the standard library of the programming language
			// the standard function has 2 direct parameters that are part of the intermediate code instruction at compile-time
			// the first parameter holds the number of parameters that the standard function expects
			// the second parameter holds the call code of the standard function in the programming language's standard library

			// panic if parsing of the parameters count into an unsigned integer fails (unsupported value or data type)
			count := i.Code.Arg1.Parse().(uint64)
			
			// panic if parsing of the call code into an integer fails (unsupported value or data type)
			code := i.Code.Arg2.Parse().(int64)
			
			// parameter instruction for the standard library function call
			pi := parameters.Back().Value.(*cod.Instruction)
			parameters.Remove(parameters.Back())

			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = pi.Code.Arg1.Parse()

			if count != 1 {
				// current standard library functions expect 1 parameter on the runtime control stack
				panic(cor.NewGeneralError(cor.Emulator, failureMap, cor.Fatal, unexpectedNumberOfFunctionArguments, nil, nil))
			} else {
				// the top element of the runtime control stack is either consumed or updated by the standard library function
				e.appendInstruction(StdCall, l, newOperand(ImmediateOperand, code))
			}

		default:
			panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unknownIntermediateCodeOperation, i.Code.Operation, nil))
		}

		// collected labels must be used by the directly following instruction (one instruction consumes all collected labels)
		if i.Code.Operation != cod.Target {
			l = make([]string, 0)
		}
	}

	return e.text
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

// The linker resolves jump and call label references to absolut code addresses in assembly code of the text section.
func (e *emitter) link() error {
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
			// for all jump and call operation codes, the first kind of operand must be 'LabelOperand'
			if asm.Operands[0].OperandKind != LabelOperand {
				return cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, unexpectedKindOfOperandInCpuOperation, asm.Operands[0].OperandKind, nil)
			}

			// replace the first operand with an operand kind 'JumpOperand' that holds the absolute address
			if address, ok := labels[asm.Operands[0].Label]; !ok {
				return cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, unresolvedLabelReferenceInAssemblyCode, asm.Operands[0].Label, nil)
			} else {
				asm.Operands[0] = newOperand(JumpOperand, address)
			}
		}
	}

	e.resolved = true
	return nil
}
