// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package assembly

import (
	"encoding/json"
	"fmt"
	"io"
	"strings"

	cor "github.com/petersen65/PL0/v2/core"
)

// Private implementation of the assembly code unit.
type assemblyCodeUnit struct {
	outputKind  OutputKind     // kind of output that is produced by the assembly code
	textSection []*Instruction // text section with assembly instructions
}

var (
	// Map CPU operation codes to their string representation.
	operationNames = map[OperationCode]string{
		None:    "",
		Push:    "push",
		Pop:     "pop",
		Mov:     "mov",
		MovAbs:  "movabs",
		Cmp:     "cmp",
		Test:    "test",
		Cld:     "cld",
		Rep:     "rep",
		Stosq:   "stosq",
		Movsxd:  "movsxd",
		Movsx:   "movsx",
		Movzx:   "movzx",
		Movsd:   "movsd",
		Movss:   "movss",
		Movq:    "movq",
		Movd:    "movd",
		Xorpd:   "xorpd",
		Xorps:   "xorps",
		Jmp:     "jmp",
		Je:      "je",
		Jne:     "jne",
		Jl:      "jl",
		Jle:     "jle",
		Jg:      "jg",
		Jge:     "jge",
		Neg:     "neg",
		And:     "and",
		Xor:     "xor",
		Cqo:     "cqo",
		Add:     "add",
		Sub:     "sub",
		Imul:    "imul",
		Div:     "div",
		Idiv:    "idiv",
		Addsd:   "addsd",
		Addss:   "addss",
		Subsd:   "subsd",
		Subss:   "subss",
		Mulsd:   "mulsd",
		Mulss:   "mulss",
		Divsd:   "divsd",
		Divss:   "divss",
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

// Create a new assembly code unit and and set the kind of output it produces.
func newAssemblyCodeUnit(outputKind OutputKind) AssemblyCodeUnit {
	return &assemblyCodeUnit{
		outputKind:  outputKind,
		textSection: make([]*Instruction, 0),
	}
}

// Create a new assembly instruction with a prefix, an operation code, some labels, and operands.
func newInstruction(prefix, operation OperationCode, labels []string, operands ...*Operand) *Instruction {
	return &Instruction{
		Prefix:    prefix,
		Operation: operation,
		Operands:  operands,
		Labels:    labels,
	}
}

// Append an instruction to the assembly code unit.
func (a *assemblyCodeUnit) AppendInstruction(operation OperationCode, labels []string, operands ...*Operand) {
	a.textSection = append(a.textSection, newInstruction(None, operation, labels, operands...))
}

// Append a prefixed instruction to the assembly code unit.
func (a *assemblyCodeUnit) AppendPrefixedInstruction(prefix, operation OperationCode, labels []string, operands ...*Operand) {
	a.textSection = append(a.textSection, newInstruction(prefix, operation, labels, operands...))
}

// Append a set of instructions to create all runtime functions.
func (a *assemblyCodeUnit) AppendRuntime() {
	loopCondition := fmt.Sprintf("%v.1", FollowStaticLinkLabel)
	behindLoop := fmt.Sprintf("%v.2", FollowStaticLinkLabel)

	/*
		# -------------------------------------------------------------------------------
		# rt.create_static_link
		#
		# Purpose:
		#   Computes and stores the static link for a newly entered procedure.
		#
		# Assumptions:
		#   - r10d contains the lexical depth difference between the caller and callee.
		#     (e.g., 0 = immediate parent, 1 = skip one static scope, etc.)
		#   - The current stack frame is fully set up:
		#       - rbp points to the current frame (callee's frame).
		#       - [rbp] contains the saved rbp of the caller (dynamic link).
		#
		# Effects:
		#   - Calls follow_static_link to resolve the static parent.
		#   - Stores the resolved static link (frame pointer of parent scope) at [rbp-8].
		#     This is assumed to be the callee’s first local variable (invisible to ABI).
		#
		# Clobbers:
		#   - rax, rsi (internally used)
		#   - r10d remains unchanged (caller-saved)
		#
		# Details:
		#   - The static link is a pointer to the frame of a lexical parent.
		#   - The dynamic link is a pointer to the frame of the actual caller.
		#   - They are equal only when: the caller is the immediate parent of the callee.
		#   - r10d == 0 is not just a numerical fact, but a semantic assertion.
		# -------------------------------------------------------------------------------
		rt.create_static_link:
			mov         edi, r10d                    # move depth difference into edi (used as counter)
			mov         rsi, qword ptr [rbp]         # load caller's rbp (dynamic link) into rsi
			call        rt.follow_static_link.1      # follow static link chain depth times
			mov         qword ptr [rbp-8], rax       # store static link (resolved frame pointer) into callee’s locals
			ret
	*/
	a.AppendInstruction(Mov, []string{CreateStaticLinkLabel}, NewRegisterOperand(Edi), NewRegisterOperand(R10d))
	a.AppendInstruction(Mov, nil, NewRegisterOperand(Rsi), NewMemoryOperand(Rbp, Bits64, 0))
	a.AppendInstruction(Call, nil, NewLabelOperand(loopCondition))
	a.AppendInstruction(Mov, nil, NewMemoryOperand(Rbp, Bits64, -PointerSize), NewRegisterOperand(Rax))
	a.AppendInstruction(Ret, nil)

	/*
		# -------------------------------------------------------------------------------
		# rt.follow_static_link
		#
		# Purpose:
		#   Entry point for static link traversal.
		#
		# Effects:
		#   - Sets rsi = rbp (if needed for manual entry)
		#   - Falls through into rt.follow_static_link.1
		# -------------------------------------------------------------------------------
		rt.follow_static_link:
			mov         rsi, rbp                     # optional manual starting point for static link chain
													 # used if follow_static_link is called with current rbp
	*/
	a.AppendInstruction(Mov, []string{FollowStaticLinkLabel}, NewRegisterOperand(Rsi), NewRegisterOperand(Rbp))

	/*
		# -------------------------------------------------------------------------------
		# rt.follow_static_link.1
		#
		# Purpose:
		#   Recursively follows the static link chain `edi` times.
		#
		# Assumptions:
		#   - edi = depth difference (0 means: current level)
		#   - rsi = starting frame pointer
		#
		# Result:
		#   - rax = frame pointer of the statically enclosing scope
		#
		# Clobbers:
		#   - rsi, edi
		# -------------------------------------------------------------------------------
		rt.follow_static_link.1:
			cmp         edi, 0                       # have we reached the target lexical depth (difference == 0)?
			je          rt.follow_static_link.2      # yes → stop and return current frame pointer in rsi
			mov   		rdx, qword ptr [rsi-8]		 # load the next static link from current frame
			test  		rdx, rdx					 # check if the next static link is 0 (end of PL/0 static link chain)
			je    		rt.follow_static_link.2      # if 0 → stop to avoid entering non-PL/0 frames (e.g., C runtime)
			mov         rsi, qword ptr [rsi-8]       # follow the static link upward (i.e., to lexical parent frame)
			sub         edi, 1                       # decrease remaining lexical distance
			jmp         rt.follow_static_link.1      # repeat loop until depth is 0 or static link chain ends

		rt.follow_static_link.2:
			mov         rax, rsi                     # return the resolved static link (frame pointer of target lexical parent)
			ret 									 # return to caller with rax = static link result
	*/
	a.AppendInstruction(Cmp, []string{loopCondition}, NewRegisterOperand(Edi), NewImmediateOperand(Bits32, int32(0)))
	a.AppendInstruction(Je, nil, NewLabelOperand(behindLoop))
	a.AppendInstruction(Mov, nil, NewRegisterOperand(Rdx), NewMemoryOperand(Rsi, Bits64, -PointerSize))
	a.AppendInstruction(Test, nil, NewRegisterOperand(Rdx), NewRegisterOperand(Rdx))
	a.AppendInstruction(Je, nil, NewLabelOperand(behindLoop))
	a.AppendInstruction(Mov, nil, NewRegisterOperand(Rsi), NewRegisterOperand(Rdx))
	a.AppendInstruction(Sub, nil, NewRegisterOperand(Edi), NewImmediateOperand(Bits32, int32(1)))
	a.AppendInstruction(Jmp, nil, NewLabelOperand(loopCondition))
	a.AppendInstruction(Mov, []string{behindLoop}, NewRegisterOperand(Rax), NewRegisterOperand(Rsi))
	a.AppendInstruction(Ret, nil)
}

// Return the number of instructions in the assembly code unit.
func (a *assemblyCodeUnit) Length() int {
	return len(a.textSection)
}

// Return the instruction at the specified index in the assembly code unit.
func (a *assemblyCodeUnit) GetInstruction(index int) *Instruction {
	return a.textSection[index]
}

// Print the assembly code to the specified writer and optionally accept global symbols as arguments.
func (a *assemblyCodeUnit) Print(print io.Writer, args ...any) error {
	var globals []string
	var globalDirective string

	for _, arg := range args {
		globals = append(globals, fmt.Sprint(arg))
	}

	if len(globals) > 0 {
		globalDirective = ".global " + strings.Join(globals, ", ") + "\n"
	}

	header := "# x86_64 assembly code\n" +
		"# generated by PL/0 compiler\n\n" +
		".intel_syntax noprefix\n" +
		globalDirective +
		".text\n\n"

	if strings.Contains(strings.Join(globals, " "), EntryPointLabel) {
		header += fmt.Sprintf("%[1]v:\n", EntryPointLabel)
	}

	if _, err := fmt.Fprint(print, header); err != nil {
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
		switch a.outputKind {
		case Application:
			return a.Print(print, EntryPointLabel)

		case Runtime:
			return a.Print(print, CreateStaticLinkLabel, FollowStaticLinkLabel)

		default:
			panic(cor.NewGeneralError(cor.Assembly, failureMap, cor.Fatal, unknownExportFormat, a.outputKind, nil))
		}

	default:
		panic(cor.NewGeneralError(cor.Assembly, failureMap, cor.Fatal, unknownExportFormat, format, nil))
	}
}
