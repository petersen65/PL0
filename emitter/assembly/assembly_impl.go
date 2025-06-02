// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package assembly

import (
	"bytes"
	"encoding/gob"
	"encoding/json"
	"fmt"
	"io"

	cor "github.com/petersen65/PL0/v2/core"
)

// Private implementation of the assembly code unit.
type assemblyCodeUnit struct {
	resolved    bool           // flag to indicate if all labels have been resolved
	textSection []*Instruction // text section with assembly instructions
}

var (
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
		Rax:   "rax",
		Rbx:   "rbx",
		Rcx:   "rcx",
		Rdx:   "rdx",
		Rsi:   "rsi",
		Rdi:   "rdi",
		R8:    "r8",
		R9:    "r9",
		R10:   "r10",
		R11:   "r11",
		R12:   "r12",
		R13:   "r13",
		R14:   "r14",
		R15:   "r15",
		Rip:   "rip",
		Rsp:   "rsp",
		Rbp:   "rbp",
		Eax:   "eax",
		Ebx:   "ebx",
		Ecx:   "ecx",
		Edx:   "edx",
		Esi:   "esi",
		Edi:   "edi",
		R8d:   "r8d",
		R9d:   "r9d",
		R10d:  "r10d",
		R11d:  "r11d",
		R12d:  "r12d",
		R13d:  "r13d",
		R14d:  "r14d",
		R15d:  "r15d",
		Ax:    "ax",
		Bx:    "bx",
		Cx:    "cx",
		Dx:    "dx",
		Si:    "si",
		Di:    "di",
		R8w:   "r8w",
		R9w:   "r9w",
		R10w:  "r10w",
		R11w:  "r11w",
		R12w:  "r12w",
		R13w:  "r13w",
		R14w:  "r14w",
		R15w:  "r15w",
		Al:    "al",
		Bl:    "bl",
		Cl:    "cl",
		Dl:    "dl",
		Ah:    "ah",
		Bh:    "bh",
		Ch:    "ch",
		Dh:    "dh",
		R8b:   "r8b",
		R9b:   "r9b",
		R10b:  "r10b",
		R11b:  "r11b",
		R12b:  "r12b",
		R13b:  "r13b",
		R14b:  "r14b",
		R15b:  "r15b",
		Ymm0:  "ymm0",
		Ymm1:  "ymm1",
		Ymm2:  "ymm2",
		Ymm3:  "ymm3",
		Ymm4:  "ymm4",
		Ymm5:  "ymm5",
		Ymm6:  "ymm6",
		Ymm7:  "ymm7",
		Ymm8:  "ymm8",
		Ymm9:  "ymm9",
		Ymm10: "ymm10",
		Ymm11: "ymm11",
		Ymm12: "ymm12",
		Ymm13: "ymm13",
		Ymm14: "ymm14",
		Ymm15: "ymm15",
		Xmm0:  "xmm0",
		Xmm1:  "xmm1",
		Xmm2:  "xmm2",
		Xmm3:  "xmm3",
		Xmm4:  "xmm4",
		Xmm5:  "xmm5",
		Xmm6:  "xmm6",
		Xmm7:  "xmm7",
		Xmm8:  "xmm8",
		Xmm9:  "xmm9",
		Xmm10: "xmm10",
		Xmm11: "xmm11",
		Xmm12: "xmm12",
		Xmm13: "xmm13",
		Xmm14: "xmm14",
		Xmm15: "xmm15",
	}

	// Map CPU operand sizes to their string representation.
	operandSizeNames = map[OperandSize]string{
		Bits8:  "byte",
		Bits16: "word",
		Bits32: "dword",
		Bits64: "qword",
	}
)

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
func newOperand(kind OperandKind, value any, detail ...any) *Operand {
	switch kind {
	case RegisterOperand:
		return &Operand{Kind: RegisterOperand, Register: value.(Register)}

	case ImmediateOperand:
		if len(detail) > 0 {
			return &Operand{Kind: ImmediateOperand, Immediate: detail[0].(ImmediateDetail)}
		}

		return &Operand{Kind: ImmediateOperand, Immediate: ImmediateDetail{Size: Bits64, Value: value.(int64)}}

	case MemoryOperand:
		if len(detail) > 0 {
			return &Operand{Kind: MemoryOperand, Register: value.(Register), Memory: detail[0].(MemoryDetail)}
		}

		return &Operand{Kind: MemoryOperand, Register: value.(Register), Memory: MemoryDetail{Size: Bits64, Displacement: 0}}

	case LabelOperand:
		return &Operand{Kind: LabelOperand, Label: value.(string)}

	case JumpOperand:
		return &Operand{Kind: JumpOperand, Jump: value.(uint64)}

	default:
		panic(cor.NewGeneralError(cor.Assembly, failureMap, cor.Fatal, unknownKindOfOperandInCpuOperation, kind, nil))
	}
}

// Append an instruction to the assembly code unit.
func (a *assemblyCodeUnit) AppendInstruction(op OperationCode, labels []string, operands ...*Operand) {
	a.textSection = append(a.textSection, newInstruction(op, labels, operands...))
}

// Append a set of instructions to create all runtime library functions.
func (a *assemblyCodeUnit) AppendRuntimeLibrary() {
	loopCondition := fmt.Sprintf("%v.1", FollowStaticLinkLabel)
	behindLoop := fmt.Sprintf("%v.2", FollowStaticLinkLabel)
	detail := MemoryDetail{Size: Bits64, Displacement: DescriptorSize - PointerSize}

	// runtime library function "create_static_link"
	a.AppendInstruction(Mov, []string{CreateStaticLinkLabel}, newOperand(RegisterOperand, Rcx), newOperand(MemoryOperand, Rbp, detail))
	a.AppendInstruction(Mov, nil, newOperand(RegisterOperand, Rbx), newOperand(MemoryOperand, Rbp))
	a.AppendInstruction(Call, nil, newOperand(LabelOperand, loopCondition))
	a.AppendInstruction(Mov, nil, newOperand(MemoryOperand, Rbp, detail), newOperand(RegisterOperand, Rbx))
	a.AppendInstruction(Ret, nil)

	// runtime library function "follow_static_link"
	a.AppendInstruction(Mov, []string{FollowStaticLinkLabel}, newOperand(RegisterOperand, Rbx), newOperand(RegisterOperand, Rbp))
	a.AppendInstruction(Cmp, []string{loopCondition}, newOperand(RegisterOperand, Rcx), newOperand(ImmediateOperand, int64(0)))
	a.AppendInstruction(Je, nil, newOperand(LabelOperand, behindLoop))
	a.AppendInstruction(Mov, nil, newOperand(RegisterOperand, Rbx), newOperand(MemoryOperand, Rbx, detail))
	a.AppendInstruction(Sub, nil, newOperand(RegisterOperand, Rcx), newOperand(ImmediateOperand, int64(1)))
	a.AppendInstruction(Jmp, nil, newOperand(LabelOperand, loopCondition))
	a.AppendInstruction(Ret, []string{behindLoop})
}

// The linker resolves jump and call label references to absolut code addresses in the assembly code unit.
func (a *assemblyCodeUnit) Link() error {
	// return without linking if all labels have already been resolved
	if a.resolved {
		return nil
	}

	labels := make(map[string]uint64)

	// create a map of labels and their absolute addresses
	for i := range a.textSection {
		for _, label := range a.textSection[i].Labels {
			labels[label] = uint64(i)
		}
	}

	// resolve jump and call label references to absolute code addresses
	for _, asm := range a.textSection {
		switch asm.Operation {
		case Call, Jmp, Je, Jne, Jl, Jle, Jg, Jge:
			// for all jump and call operation codes, the first kind of operand must be 'LabelOperand'
			if asm.Operands[0].Kind != LabelOperand {
				return cor.NewGeneralError(cor.Assembly, failureMap, cor.Error, unexpectedKindOfOperandInCpuOperation, asm.Operands[0].Kind, nil)
			}

			// replace the first operand with an operand kind 'JumpOperand' that holds the absolute address
			if address, ok := labels[asm.Operands[0].Label]; !ok {
				return cor.NewGeneralError(cor.Assembly, failureMap, cor.Error, unresolvedLabelReferenceInAssemblyCode, asm.Operands[0].Label, nil)
			} else {
				asm.Operands[0] = newOperand(JumpOperand, address)
			}
		}
	}

	a.resolved = true
	return nil
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
	header := "# x86_64 assembly code\n" +
		"# generated by PL/0 compiler\n\n" +
		".intel_syntax noprefix\n" +
		".globl %[1]v\n" +
		".type %[1]v, @function\n" +
		".text\n" +
		"%[1]v:\n"

	if _, err := fmt.Fprintf(print, header, EntryPointLabel); err != nil {
		return cor.NewGeneralError(cor.Assembly, failureMap, cor.Error, assemblyCodeExportFailed, nil, err)
	}

	for _, instr := range a.textSection {
		if _, err := fmt.Fprintf(print, "%v\n", instr); err != nil {
			return cor.NewGeneralError(cor.Assembly, failureMap, cor.Error, assemblyCodeExportFailed, nil, err)
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
			return cor.NewGeneralError(cor.Assembly, failureMap, cor.Error, assemblyCodeExportFailed, nil, err)
		} else {
			_, err = print.Write(raw)

			if err != nil {
				err = cor.NewGeneralError(cor.Assembly, failureMap, cor.Error, assemblyCodeExportFailed, nil, err)
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
			return cor.NewGeneralError(cor.Assembly, failureMap, cor.Error, linkingStepMissing, nil, nil)
		}

		// encode the text section into a binary buffer
		if err := gob.NewEncoder(&buffer).Encode(a.textSection); err != nil {
			return cor.NewGeneralError(cor.Assembly, failureMap, cor.Error, assemblyCodeExportFailed, nil, err)
		}

		// transfer the binary buffer to the print writer
		if _, err := buffer.WriteTo(print); err != nil {
			return cor.NewGeneralError(cor.Assembly, failureMap, cor.Error, assemblyCodeExportFailed, nil, err)
		}

		return nil

	default:
		panic(cor.NewGeneralError(cor.Assembly, failureMap, cor.Fatal, unknownExportFormat, format, nil))
	}
}

// Import the assembly code from the specified reader in the specified format.
func (a *assemblyCodeUnit) Import(format cor.ExportFormat, scan io.Reader) error {
	switch format {
	case cor.Binary:
		// decode the binary buffer into the text section
		if err := gob.NewDecoder(scan).Decode(&a.textSection); err != nil {
			return cor.NewGeneralError(cor.Assembly, failureMap, cor.Error, assemblyCodeImportFailed, nil, err)
		}

		return nil

	default:
		panic(cor.NewGeneralError(cor.Assembly, failureMap, cor.Fatal, unknownImportFormat, format, nil))
	}
}
