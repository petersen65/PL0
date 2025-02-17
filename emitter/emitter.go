// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emitter

import (
	"bytes"
	"container/list"
	"encoding/gob"
	"encoding/json"
	"fmt"
	"io"
	"strings"

	cor "github.com/petersen65/PL0/v2/core"
	gen "github.com/petersen65/PL0/v2/generator"
)

const (
	descriptorSize        = 3                       // size of an activation record descriptor
	defaultStartLabel     = "_start"                // label for the entry point of the program if none is provided
	createStaticLinkLabel = "rt.create_static_link" // label for runtime library function "create_static_link"
	followStaticLinkLabel = "rt.follow_static_link" // label for runtime library function "follow_static_link"
)

type (
	// Private implementation of the assembly code emitter.
	emitter struct {
		intermediateCode gen.IntermediateCodeUnit // intermediate code unit to generate assembly code for
		assemblyCode     *assemblyCodeUnit        // assembly code unit for the CPU target
		cpu              CentralProcessingUnit    // target CPU for the emitter
	}

	// Private implementation of the assembly code unit.
	assemblyCodeUnit struct {
		resolved    bool           // flag to indicate if all labels have been resolved
		textSection []*Instruction // text section with assembly instructions
	}
)

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
func newEmitter(cpu CentralProcessingUnit, intermediateCodeUnit gen.IntermediateCodeUnit) Emitter {
	if cpu != Amd64 {
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unsupportedCpuTarget, cpu, nil))
	}

	return &emitter{
		intermediateCode: intermediateCodeUnit,
		assemblyCode:     NewAssemblyCodeUnit().(*assemblyCodeUnit),
		cpu:              cpu,
	}
}

// Create a new assembly code unit and initialize it as unresolved so that it must be linked before exporting it.
func newAssemblyCodeUnit() AssemblyCodeUnit {
	return &assemblyCodeUnit{
		textSection: make([]*Instruction, 0),
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

// Append an instruction to the assembly code unit.
func (a *assemblyCodeUnit) AppendInstruction(op OperationCode, labels []string, operands ...*Operand) {
	a.textSection = append(a.textSection, newInstruction(op, labels, operands...))
}

// Append a set of instructions to create all runtime library functions.
func (a *assemblyCodeUnit) AppendRuntimeLibrary() {
	loopCondition := fmt.Sprintf("%v.1", followStaticLinkLabel)
	behindLoop := fmt.Sprintf("%v.2", followStaticLinkLabel)

	// runtime library function "create_static_link"
	a.AppendInstruction(Mov, []string{createStaticLinkLabel}, newOperand(RegisterOperand, Rcx), newOperand(MemoryOperand, Rbp, descriptorSize-1))
	a.AppendInstruction(Mov, nil, newOperand(RegisterOperand, Rbx), newOperand(MemoryOperand, Rbp))
	a.AppendInstruction(Call, nil, newOperand(LabelOperand, loopCondition))
	a.AppendInstruction(Mov, nil, newOperand(MemoryOperand, Rbp, descriptorSize-1), newOperand(RegisterOperand, Rbx))
	a.AppendInstruction(Ret, nil)

	// runtime library function "follow_static_link"
	a.AppendInstruction(Mov, []string{followStaticLinkLabel}, newOperand(RegisterOperand, Rbx), newOperand(RegisterOperand, Rbp))
	a.AppendInstruction(Cmp, []string{loopCondition}, newOperand(RegisterOperand, Rcx), newOperand(ImmediateOperand, int64(0)))
	a.AppendInstruction(Je, nil, newOperand(LabelOperand, behindLoop))
	a.AppendInstruction(Mov, nil, newOperand(RegisterOperand, Rbx), newOperand(MemoryOperand, Rbx, descriptorSize-1))
	a.AppendInstruction(Sub, nil, newOperand(RegisterOperand, Rcx), newOperand(ImmediateOperand, int64(1)))
	a.AppendInstruction(Jmp, nil, newOperand(LabelOperand, loopCondition))
	a.AppendInstruction(Ret, []string{behindLoop})
}

// Return the number of instructions in the assembly code unit.
func (a *assemblyCodeUnit) Length() int {
	return len(a.textSection)
}

// Return the instruction at the specified index in the assembly code unit.
func (a *assemblyCodeUnit) GetInstruction(index int) *Instruction {
	return a.textSection[index]
}

// Print the assembly code to the specified writer.
func (a *assemblyCodeUnit) Print(print io.Writer, args ...any) error {
	if _, err := fmt.Fprintf(print, "global %v\nsection .text\n%v:\n", defaultStartLabel, defaultStartLabel); err != nil {
		return cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, assemblyCodeExportFailed, nil, err)
	}

	for _, instr := range a.textSection {
		if _, err := fmt.Fprintf(print, "%v\n", instr); err != nil {
			return cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, assemblyCodeExportFailed, nil, err)
		}
	}

	return nil
}

// Export the assembly code to the specified writer in the specified format.
func (a *assemblyCodeUnit) Export(format cor.ExportFormat, print io.Writer) error {
	switch format {
	case cor.Json:
		// export the text section as a JSON object and wrap it in a struct to provide a field name for the text section
		if raw, err := json.MarshalIndent(struct {
			TextSection []*Instruction `json:"text_section"`
		}{TextSection: a.textSection}, "", "  "); err != nil {
			return cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, assemblyCodeExportFailed, nil, err)
		} else {
			_, err = print.Write(raw)

			if err != nil {
				err = cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, assemblyCodeExportFailed, nil, err)
			}

			return err
		}

	case cor.Text:
		// print is a convenience function to export the text section as a string to the print writer
		return a.Print(print)

	case cor.Binary:
		var buffer bytes.Buffer

		// cannot export binary target before a linking step has resolved all labels
		if !a.resolved {
			return cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, linkingStepMissing, nil, nil)
		}

		// encode the text section into a binary buffer
		if err := gob.NewEncoder(&buffer).Encode(a.textSection); err != nil {
			return cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, assemblyCodeExportFailed, nil, err)
		}

		// transfer the binary buffer to the print writer
		if _, err := buffer.WriteTo(print); err != nil {
			return cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, assemblyCodeExportFailed, nil, err)
		}

		return nil

	default:
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unknownExportFormat, format, nil))
	}
}

// Import the assembly code from the specified reader in the specified format.
func (a *assemblyCodeUnit) Import(format cor.ExportFormat, scan io.Reader) error {
	switch format {
	case cor.Binary:
		// decode the binary buffer into the text section
		if err := gob.NewDecoder(scan).Decode(&a.textSection); err != nil {
			return cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, assemblyCodeImportFailed, nil, err)
		}

		return nil

	default:
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unknownImportFormat, format, nil))
	}
}

// Emit assembly code for the CPU target.
func (e *emitter) Emit() {
	iterator := e.intermediateCode.GetIterator()

	// compile-time parameters list for standard library function calls (procedures do not support parameters yet)
	parameters := list.New()

	// perform an assembly instruction selection for each intermediate code instruction
	for i, l := iterator.First(), make([]string, 0); i != nil; i = iterator.Next() {
		// panic if the intermediate code instruction has not a valid addresses contract
		i.Code.ValidateAddressesContract()

		switch i.Code.Operation {
		case gen.Target: // append labels for the directly following non 'Target' instruction
			l = append(l, i.Label)

		case gen.Allocate: // allocate space in an activation record for all local variables
			// group consecutive intermediate code allocate operations into one alloc instruction
			for j := 0; ; j++ {
				if iterator.Peek(j).Code.Operation != gen.Allocate {
					e.assemblyCode.AppendInstruction(Sub, l,
						newOperand(RegisterOperand, Rsp),
						newOperand(ImmediateOperand, int64(j+1)))

					iterator.Skip(j)
					break
				}
			}

		case gen.Prelude: // function body prelude
			// save caller's base pointer because it will be changed
			// this creates a 'dynamic link' chain of base pointers so that each callee knows the base pointer of its caller
			// an alternative naming from literature is 'control link' that points to the activation record of the caller
			e.assemblyCode.AppendInstruction(Push, l, newOperand(RegisterOperand, Rbp))

			// new base pointer points to start of local variables in the activation record
			e.assemblyCode.AppendInstruction(Mov, nil, newOperand(RegisterOperand, Rbp), newOperand(RegisterOperand, Rsp))

			// call runtime library function to create static link which provides the compile-time block nesting hierarchy at runtime
			e.assemblyCode.AppendInstruction(Call, nil, newOperand(LabelOperand, createStaticLinkLabel))

		case gen.Epilog: // function body epilog
			// clean allocated local variables from the activation record
			e.assemblyCode.AppendInstruction(Mov, l, newOperand(RegisterOperand, Rsp), newOperand(RegisterOperand, Rbp))

			// restore caller's base pointer
			e.assemblyCode.AppendInstruction(Pop, nil, newOperand(RegisterOperand, Rbp))

		case gen.ValueCopy: // push an immediate value onto the runtime control stack
			// panic if parsing of the literal into its value fails (unsupported value or data type)
			value := i.Code.Arg1.Parse()
			e.assemblyCode.AppendInstruction(Push, l, newOperand(ImmediateOperand, value))

		case gen.VariableLoad: // load a variable from its runtime control stack address onto the top of the stack
			// panic if parsing of the variable into its location fails (unsupported data type)
			location := i.Code.Arg1.Parse().(uint64)

			if i.DepthDifference == 0 {
				// push memory content at 'variables base - variable offset' onto runtime control stack
				e.assemblyCode.AppendInstruction(Push, l, newOperand(MemoryOperand, Rbp, -int64(location)))
			} else {
				// block nesting depth difference between variable use and variable declaration
				e.assemblyCode.AppendInstruction(Mov, l,
					newOperand(RegisterOperand, Rcx),
					newOperand(ImmediateOperand, int64(i.DepthDifference)))

				// call runtime library function to follow static link to determine the 'variables base' pointer
				e.assemblyCode.AppendInstruction(Call, nil, newOperand(LabelOperand, followStaticLinkLabel))

				// push memory content at 'variables base - variable offset' onto runtime control stack
				e.assemblyCode.AppendInstruction(Push, nil, newOperand(MemoryOperand, Rbx, -int64(location)))
			}

		case gen.VariableStore: // store the top of the runtime control stack into a variable's stack address
			// panic if parsing of the variable into its location fails (unsupported data type)
			location := i.Code.Result.Parse().(uint64)

			if i.DepthDifference == 0 {
				// pop content of the variable
				e.assemblyCode.AppendInstruction(Pop, l, newOperand(RegisterOperand, Rax))

				// copy content of the variable into memory location 'variables base - variable offset'
				e.assemblyCode.AppendInstruction(Mov, nil,
					newOperand(MemoryOperand, Rbp, -int64(location)),
					newOperand(RegisterOperand, Rax))
			} else {
				// block nesting depth difference between variable use and variable declaration
				e.assemblyCode.AppendInstruction(Mov, l,
					newOperand(RegisterOperand, Rcx),
					newOperand(ImmediateOperand, int64(i.DepthDifference)))

				// call runtime library function to follow static link to determine the 'variables base' pointer
				e.assemblyCode.AppendInstruction(Call, nil, newOperand(LabelOperand, followStaticLinkLabel))

				// pop content of the variable
				e.assemblyCode.AppendInstruction(Pop, nil, newOperand(RegisterOperand, Rax))

				// copy content of the variable into memory location 'variables base - variable offset'
				e.assemblyCode.AppendInstruction(Mov, nil,
					newOperand(MemoryOperand, Rbx, -int64(location)),
					newOperand(RegisterOperand, Rax))
			}

		case gen.Negate: // negate the top of the runtime control stack and leave the result on the stack
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.Code.Arg1.Parse()

			e.assemblyCode.AppendInstruction(Pop, l, newOperand(RegisterOperand, Rax))
			e.assemblyCode.AppendInstruction(Neg, nil, newOperand(RegisterOperand, Rax))
			e.assemblyCode.AppendInstruction(Push, nil, newOperand(RegisterOperand, Rax))

		case gen.Odd: // check if the top of the runtime control stack is an odd number and leave the result in the CPU flags register
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.Code.Arg1.Parse()

			e.assemblyCode.AppendInstruction(Pop, l, newOperand(RegisterOperand, Rax))
			e.assemblyCode.AppendInstruction(And, nil, newOperand(RegisterOperand, Rax))

		case gen.Plus, gen.Minus, gen.Times, gen.Divide: // perform an arithmetic operation on the top two elements of the runtime control stack and replace them with one result on the stack
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.Code.Arg1.Parse()
			_ = i.Code.Arg2.Parse()

			e.assemblyCode.AppendInstruction(Pop, l, newOperand(RegisterOperand, Rbx))
			e.assemblyCode.AppendInstruction(Pop, nil, newOperand(RegisterOperand, Rax))

			switch i.Code.Operation {
			case gen.Plus:
				e.assemblyCode.AppendInstruction(Add, nil, newOperand(RegisterOperand, Rax), newOperand(RegisterOperand, Rbx))

			case gen.Minus:
				e.assemblyCode.AppendInstruction(Sub, nil, newOperand(RegisterOperand, Rax), newOperand(RegisterOperand, Rbx))

			case gen.Times:
				e.assemblyCode.AppendInstruction(Imul, nil, newOperand(RegisterOperand, Rax), newOperand(RegisterOperand, Rbx))

			case gen.Divide:
				e.assemblyCode.AppendInstruction(Idiv, nil, newOperand(RegisterOperand, Rax), newOperand(RegisterOperand, Rbx))
			}

			e.assemblyCode.AppendInstruction(Push, nil, newOperand(RegisterOperand, Rax))

		case gen.Equal, gen.NotEqual, gen.Less, gen.LessEqual, gen.Greater, gen.GreaterEqual: // compare the top two elements of the runtime control stack, remove them, and leave the result in the CPU flags register
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.Code.Arg1.Parse()
			_ = i.Code.Arg2.Parse()

			e.assemblyCode.AppendInstruction(Pop, l, newOperand(RegisterOperand, Rbx))
			e.assemblyCode.AppendInstruction(Pop, nil, newOperand(RegisterOperand, Rax))
			e.assemblyCode.AppendInstruction(Cmp, nil, newOperand(RegisterOperand, Rax), newOperand(RegisterOperand, Rbx))

		case gen.Jump: // unconditionally jump to a label that is resolved by the linker
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(Jmp, l, newOperand(LabelOperand, name))

		case gen.JumpEqual: // jump to a label if the CPU flags register indicates that the top two elements of the stack were equal
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(Je, l, newOperand(LabelOperand, name))

		case gen.JumpNotEqual: // jump to a label if the CPU flags register indicates that the top two elements of the stack were not equal
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(Jne, l, newOperand(LabelOperand, name))

		case gen.JumpLess: // jump to a label if the CPU flags register indicates that the first element of the stack was less than the second top element
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(Jl, l, newOperand(LabelOperand, name))

		case gen.JumpLessEqual: // jump to a label if the CPU flags register indicates that the first element of the stack was less than or equal to the second top element
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(Jle, l, newOperand(LabelOperand, name))

		case gen.JumpGreater: // jump to a label if the CPU flags register indicates that the first element of the stack was greater than the second top element
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(Jg, l, newOperand(LabelOperand, name))

		case gen.JumpGreaterEqual: // jump to a label if the CPU flags register indicates that the first element of the stack was greater than or equal to the second top element
			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg1.Parse().(string)
			e.assemblyCode.AppendInstruction(Jge, l, newOperand(LabelOperand, name))

		case gen.Parameter: // push a parameter onto the compile-time parameters list for a standard library function call
			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = i.Code.Arg1.Parse()
			parameters.PushBack(i)

		case gen.Call: // call a function with 0 arguments by jumping to the function's label
			// panic if parsing of the parameters count into an unsigned integer fails (unsupported value or data type)
			count := i.Code.Arg1.Parse().(uint64)

			// panic if parsing of the label into a string fails (unsupported data type)
			name := i.Code.Arg2.Parse().(string)

			if count != 0 {
				// procedures do not support parameters yet
				panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unexpectedNumberOfFunctionArguments, nil, nil))
			} else {
				// push difference between use depth and declaration depth on runtime control stack
				e.assemblyCode.AppendInstruction(Push, l, newOperand(ImmediateOperand, int64(i.DepthDifference)))

				// push return address on runtime control stack and jump to callee
				e.assemblyCode.AppendInstruction(Call, nil, newOperand(LabelOperand, name))

				// remove difference from runtime control stack after return from callee
				e.assemblyCode.AppendInstruction(Add, nil, newOperand(RegisterOperand, Rsp), newOperand(ImmediateOperand, int64(1)))
			}

		case gen.Return: // return from a function to its caller
			e.assemblyCode.AppendInstruction(Ret, nil)

		case gen.Standard:
			// a standard function represents a function that is provided by the standard library of the programming language
			// the standard function has 2 direct parameters that are part of the intermediate code instruction at compile-time
			// the first parameter holds the number of parameters that the standard function expects
			// the second parameter holds the call code of the standard function in the programming language's standard library

			// panic if parsing of the parameters count into an unsigned integer fails (unsupported value or data type)
			count := i.Code.Arg1.Parse().(uint64)

			// panic if parsing of the call code into an integer fails (unsupported value or data type)
			code := i.Code.Arg2.Parse().(int64)

			// parameter instruction for the standard library function call
			pi := parameters.Back().Value.(*gen.Instruction)
			parameters.Remove(parameters.Back())

			// panic if parsing of the temporary into nil fails (unsupported data type)
			_ = pi.Code.Arg1.Parse()

			if count != 1 {
				// current standard library functions expect 1 parameter on the runtime control stack
				panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unexpectedNumberOfFunctionArguments, nil, nil))
			} else {
				// the top element of the runtime control stack is either consumed or updated by the standard library function
				e.assemblyCode.AppendInstruction(StdCall, l, newOperand(ImmediateOperand, code))
			}

		default:
			panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unknownIntermediateCodeOperation, i.Code.Operation, nil))
		}

		// collected labels must be used by the directly following instruction (one instruction consumes all collected labels)
		if i.Code.Operation != gen.Target {
			l = make([]string, 0)
		}
	}
}

// Get access to the generated assembly code.
func (e *emitter) GetAssemblyCodeUnit() AssemblyCodeUnit {
	return e.assemblyCode
}

// The linker resolves jump and call label references to absolut code addresses in the assembly code unit.
func (e *emitter) Link() error {
	// return without linking if all labels have already been resolved
	if e.assemblyCode.resolved {
		return nil
	}

	labels := make(map[string]uint64)

	// create a map of labels and their absolute addresses
	for i := range e.assemblyCode.textSection {
		for _, label := range e.assemblyCode.textSection[i].Labels {
			labels[label] = uint64(i)
		}
	}

	// resolve jump and call label references to absolute code addresses
	for _, asm := range e.assemblyCode.textSection {
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

	e.assemblyCode.resolved = true
	return nil
}
