// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emitter

import (
	"bytes"
	"encoding/binary"
	"encoding/json"
	"fmt"
	"io"

	cor "github.com/petersen65/PL0/core"
)

// Private implementation of the IL/0 emitter.
type emitter struct {
	textSection TextSection
}

// Return the public interface of the private IL/0 emitter implementation.
func newEmitter() Emitter {
	return &emitter{
		textSection: make(TextSection, 0),
	}
}

// Import a raw byte slice into a text section.
func Import(raw []byte) (TextSection, error) {
	textSection := make(TextSection, len(raw)/binary.Size(Instruction{}))

	var buffer bytes.Buffer
	buffer.Write(raw)

	if err := binary.Read(&buffer, binary.LittleEndian, textSection); err != nil {
		return nil, cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, textSectionImportFailed, nil, err)
	}

	return textSection, nil
}

// Print the text section to a writer.
func (ts TextSection) Print(print io.Writer, args ...any) error {
	if _, err := print.Write([]byte(fmt.Sprintf("%-5v %-5v %-5v %-5v %-5v\n", "text", "op", "diff", "adr", "arg"))); err != nil {
		return cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, textSectionExportFailed, nil, err)
	}

	for text, instr := range ts {
		_, err := print.Write(
			[]byte(fmt.Sprintf("%-5v %-5v %-5v %-5v %-5v\n",
				text,
				OperationNames[instr.Operation],
				instr.BlockNestingDepthDifference,
				instr.Address,
				instr.ArgInt,
			)))

		if err != nil {
			return cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, textSectionExportFailed, nil, err)
		}
	}

	return nil
}

// Export the text section to a writer in the specified format.
func (ts TextSection) Export(format cor.ExportFormat, print io.Writer) error {
	switch format {
	case cor.Json:
		// export the text section as a JSON object and wrap it in a struct to provide a field name for the text section
		if bytes, err := json.MarshalIndent(struct {
			Text TextSection `json:"text_section"`
		}{Text: ts}, "", "  "); err != nil {
			return cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, textSectionExportFailed, nil, err)
		} else {
			_, err = print.Write(bytes)

			if err != nil {
				err = cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, textSectionExportFailed, nil, err)
			}

			return err
		}

	case cor.String:
		// print is a convenience function to export the text section as a string to the print writer
		return ts.Print(print)

	case cor.Binary:
		var buffer bytes.Buffer

		// write the raw bytes of the text section into a binary buffer
		if err := binary.Write(&buffer, binary.LittleEndian, ts); err != nil {
			return cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, textSectionExportFailed, nil, err)
		}

		// transfer the binary buffer to the print writer
		if _, err := buffer.WriteTo(print); err != nil {
			return cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, textSectionExportFailed, nil, err)
		}

	default:
		panic(cor.NewGeneralError(cor.Emitter, failureMap, cor.Fatal, unknownExportFormat, format, nil))
	}

	return nil
}

// Expose sections of the emitted IL/0 program.
func (e *emitter) GetSections() TextSection {
	return e.textSection
}

// Get the next free address in the text section.
func (e *emitter) GetNextAddress() Address {
	return Address(len(e.textSection))
}

// Update the target address and optionally the argument of an instruction in the text section.
func (e *emitter) Update(instruction, target Address, value any) error {
	if uint64(instruction) >= uint64(len(e.textSection)) {
		return cor.NewGeneralError(cor.Emitter, failureMap, cor.Error, instructionOutOfRange, instruction, nil)
	}

	e.textSection[instruction].Address = target

	if value != nil {
		e.textSection[instruction].ArgInt = value.(int64)
	}
	return nil
}

// Load a constant value onto the stack.
func (e *emitter) Constant(value any) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Ldc, ArgInt: value.(int64)})
	return Address(len(e.textSection) - 1)
}

// Load a variable value onto the stack.
func (e *emitter) LoadVariable(offset Offset, difference int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Ldv, BlockNestingDepthDifference: difference, Address: Address(offset)})
	return Address(len(e.textSection) - 1)
}

// Store a variable value into the stack.
func (e *emitter) StoreVariable(offset Offset, difference int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Stv, BlockNestingDepthDifference: difference, Address: Address(offset)})
	return Address(len(e.textSection) - 1)
}

// Add two values from top of stack and store the result onto the stack.
func (e *emitter) Add() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Add})
	return Address(len(e.textSection) - 1)
}

// Subtract two values from top of stack and store the result onto the stack.
func (e *emitter) Subtract() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Sub})
	return Address(len(e.textSection) - 1)
}

// Multiply two values from top of stack and store the result onto the stack.
func (e *emitter) Multiply() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Mul})
	return Address(len(e.textSection) - 1)
}

// Divide two values from top of stack and store the result onto the stack.
func (e *emitter) Divide() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Div})
	return Address(len(e.textSection) - 1)
}

// Negate the value onto the stack.
func (e *emitter) Negate() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Neg})
	return Address(len(e.textSection) - 1)
}

// Test if the value onto the stack is odd.
func (e *emitter) Odd() Address {
	e.textSection = append(e.textSection, Instruction{Operation: And})
	return Address(len(e.textSection) - 1)
}

// Test if two values from top of stack are equal.
func (e *emitter) Equal() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Eq})
	return Address(len(e.textSection) - 1)
}

// Test if two values from top of stack are not equal.
func (e *emitter) NotEqual() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Neq})
	return Address(len(e.textSection) - 1)
}

// Test if the value from top of stack is less than the value from top of stack minus one.
func (e *emitter) Less() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Lss})
	return Address(len(e.textSection) - 1)
}

// Test if the value from top of stack is less than or equal to the value from top of stack minus one.
func (e *emitter) LessEqual() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Leq})
	return Address(len(e.textSection) - 1)
}

// Test if the value from top of stack is greater than the value from top of stack minus one.
func (e *emitter) Greater() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Gtr})
	return Address(len(e.textSection) - 1)
}

// Test if the value from top of stack is greater than or equal to the value from top of stack minus one.
func (e *emitter) GreaterEqual() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Geq})
	return Address(len(e.textSection) - 1)
}

// Unconditionally jump to the target address.
func (e *emitter) Jump(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jmp, Address: target})
	return Address(len(e.textSection) - 1)
}

// Jump to the target address if the last comparison was equal.
func (e *emitter) JumpEqual(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Je, Address: target})
	return Address(len(e.textSection) - 1)
}

// Jump to the target address if the last comparison was not equal.
func (e *emitter) JumpNotEqual(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jne, Address: target})
	return Address(len(e.textSection) - 1)
}

// Jump to the target address if the last comparison was less.
func (e *emitter) JumpLess(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jl, Address: target})
	return Address(len(e.textSection) - 1)
}

// Jump to the target address if the last comparison was less or equal.
func (e *emitter) JumpLessEqual(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jle, Address: target})
	return Address(len(e.textSection) - 1)
}

// Jump to the target address if the last comparison was greater.
func (e *emitter) JumpGreater(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jg, Address: target})
	return Address(len(e.textSection) - 1)
}

// Jump to the target address if the last comparison was greater or equal.
func (e *emitter) JumpGreaterEqual(target Address) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Jge, Address: target})
	return Address(len(e.textSection) - 1)
}

// Allocate stack space for variables.
func (e *emitter) AllocateStackSpace(varOffset Offset) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Alc, Address: Address(varOffset)})
	return Address(len(e.textSection) - 1)
}

// Call a procedure at the target address from a caller.
func (e *emitter) Call(target Address, difference int32) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Cal, Address: target, BlockNestingDepthDifference: difference})
	return Address(len(e.textSection) - 1)
}

// Return from a procedure to the caller.
func (e *emitter) Return() Address {
	e.textSection = append(e.textSection, Instruction{Operation: Ret})
	return Address(len(e.textSection) - 1)
}

// System call for operating system calls.
func (e *emitter) System(call SystemCall) Address {
	e.textSection = append(e.textSection, Instruction{Operation: Sys, Address: Address(call)})
	return Address(len(e.textSection) - 1)
}
