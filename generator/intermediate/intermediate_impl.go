// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package intermediate

import (
	"container/list"
	"encoding/json"
	"fmt"
	"io"
	"strconv"
	"unicode/utf8"

	"github.com/google/uuid"
	cor "github.com/petersen65/PL0/v2/core"
)

type (
	// Represents a logical unit of instructions created from one source file.
	intermediateCodeUnit struct {
		UniqueId     string             `json:"unique_id"`    // unique identifier of the intermediate code unit
		names        []string           `json:"-"`            // enable deterministic iteration over the symbol table in the order of past inserts
		symbolTable  map[string]*Symbol `json:"-"`            // symbol table for intermediate code flattened names
		Instructions *list.List         `json:"instructions"` // intermediate code instructions as doubly linked list that allows reordering
	}

	// Navigation implementation for the unit's intermediate code instructions.
	iterator struct {
		current      *list.Element
		instructions *list.List
	}

	// A contract for addresses describes a valid set of address variants for a three-address code operation.
	addressesContract struct {
		Arg1   Variant // first address variant
		Arg2   Variant // second address variant
		Result Variant // third address variant
	}
)

var (
	// variantNames maps an address variant to its string representation.
	variantNames = map[Variant]string{
		Empty:      "ety",
		Diagnostic: "dbg",
		Temporary:  "tmp",
		Literal:    "lit",
		Variable:   "var",
		Label:      "lbl",
		Count:      "cnt",
		Code:       "cod",
	}

	// dataTypeNames maps an address datatype to its string representation.
	dataTypeNames = map[DataType]string{
		Void:              "void",
		String:            "string",
		UnsignedInteger64: "uint64",
		Integer64:         "int64",
		Integer32:         "int32",
		Integer16:         "int16",
		Integer8:          "int8",
		Float64:           "float64",
		Float32:           "float32",
		Rune32:            "rune32",
		Boolean8:          "bool8",
	}

	// Map three-address code operations of the intermediate code to their string representation.
	operationNames = map[Operation]string{
		Odd:              "odd",
		Negate:           "negate",
		Plus:             "add",
		Minus:            "subtract",
		Times:            "multiply",
		Divide:           "divide",
		Equal:            "eq",
		NotEqual:         "neq",
		Less:             "lss",
		LessEqual:        "lssEq",
		Greater:          "gtr",
		GreaterEqual:     "gtrEq",
		Jump:             "jmp",
		JumpEqual:        "jmpEq",
		JumpNotEqual:     "jmpNeq",
		JumpLess:         "jmpLss",
		JumpLessEqual:    "jmpLssEq",
		JumpGreater:      "jmpGtr",
		JumpGreaterEqual: "jmpGtrEq",
		Parameter:        "param",
		Call:             "call",
		Prelude:          "prelude",
		Epilog:           "epilog",
		Return:           "return",
		Standard:         "standard",
		Target:           "target",
		Allocate:         "alloc",
		ValueCopy:        "valCopy",
		VariableLoad:     "varLoad",
		VariableStore:    "varStore",
	}

	// The intermediate code contract maps all three-address code operations to their addresses contracts for validation.
	intermediateCodeContract = map[Operation][]addressesContract{
		Odd:              {{Arg1: Temporary, Arg2: Empty, Result: Empty}},
		Negate:           {{Arg1: Temporary, Arg2: Empty, Result: Temporary}},
		Plus:             {{Arg1: Temporary, Arg2: Temporary, Result: Temporary}},
		Minus:            {{Arg1: Temporary, Arg2: Temporary, Result: Temporary}},
		Times:            {{Arg1: Temporary, Arg2: Temporary, Result: Temporary}},
		Divide:           {{Arg1: Temporary, Arg2: Temporary, Result: Temporary}},
		Equal:            {{Arg1: Temporary, Arg2: Temporary, Result: Empty}},
		NotEqual:         {{Arg1: Temporary, Arg2: Temporary, Result: Empty}},
		Less:             {{Arg1: Temporary, Arg2: Temporary, Result: Empty}},
		LessEqual:        {{Arg1: Temporary, Arg2: Temporary, Result: Empty}},
		Greater:          {{Arg1: Temporary, Arg2: Temporary, Result: Empty}},
		GreaterEqual:     {{Arg1: Temporary, Arg2: Temporary, Result: Empty}},
		Jump:             {{Arg1: Label, Arg2: Empty, Result: Empty}},
		JumpEqual:        {{Arg1: Label, Arg2: Empty, Result: Empty}},
		JumpNotEqual:     {{Arg1: Label, Arg2: Empty, Result: Empty}},
		JumpLess:         {{Arg1: Label, Arg2: Empty, Result: Empty}},
		JumpLessEqual:    {{Arg1: Label, Arg2: Empty, Result: Empty}},
		JumpGreater:      {{Arg1: Label, Arg2: Empty, Result: Empty}},
		JumpGreaterEqual: {{Arg1: Label, Arg2: Empty, Result: Empty}},
		Parameter:        {{Arg1: Temporary, Arg2: Empty, Result: Empty}},
		Call:             {{Arg1: Count, Arg2: Label, Result: Empty}},
		Prelude:          {{Arg1: Empty, Arg2: Empty, Result: Empty}},
		Epilog:           {{Arg1: Empty, Arg2: Empty, Result: Empty}},
		Return:           {{Arg1: Empty, Arg2: Empty, Result: Empty}},
		Standard:         {{Arg1: Count, Arg2: Code, Result: Empty}},
		Target:           {{Arg1: Empty, Arg2: Empty, Result: Empty}, {Arg1: Diagnostic, Arg2: Empty, Result: Empty}},
		Allocate:         {{Arg1: Diagnostic, Arg2: Empty, Result: Variable}},
		ValueCopy:        {{Arg1: Literal, Arg2: Empty, Result: Temporary}},
		VariableLoad:     {{Arg1: Variable, Arg2: Empty, Result: Temporary}},
		VariableStore:    {{Arg1: Temporary, Arg2: Empty, Result: Variable}},
	}
)

// Create a new intermediate code unit and initialize it with a unique identifier.
func newIntermediateCodeUnit() IntermediateCodeUnit {
	return &intermediateCodeUnit{
		UniqueId:     uuid.NewString(),
		names:        make([]string, 0),
		symbolTable:  make(map[string]*Symbol),
		Instructions: list.New(),
	}
}

// Create a new instruction for the intermediate code.
func newInstruction(operatiom Operation, arg1, arg2, result *Address, options ...any) *Instruction {
	instruction := &Instruction{
		Label:           NoLabel,
		DepthDifference: UnusedDifference,
		Code:            Quadruple{Operation: operatiom, Arg1: arg1, Arg2: arg2, Result: result},
	}

	for _, option := range options {
		switch opt := option.(type) {
		case string:
			instruction.Label = opt

		case int32:
			instruction.DepthDifference = opt

		case int:
			instruction.TokenStreamIndex = opt

		default:
			panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownInstructionOption, option, nil))
		}
	}

	return instruction
}

// Append an instruction to the intermediate code.
func (m *intermediateCodeUnit) AppendInstruction(instruction *Instruction) *list.Element {
	return m.Instructions.PushBack(instruction)
}

// Get an instruction iterator for the intermediate code unit.
func (m *intermediateCodeUnit) GetIterator() Iterator {
	return &iterator{current: m.Instructions.Front(), instructions: m.Instructions}
}

// Insert a symbol into the intermediate code symbol table. If the symbol already exists, it will be overwritten.
func (m *intermediateCodeUnit) Insert(symbol *Symbol) {
	if m.Lookup(symbol.Name) == nil {
		m.names = append(m.names, symbol.Name)
	}

	m.symbolTable[symbol.Name] = symbol
}

// Lookup a symbol in the the intermediate code symbol table. If the symbol is not found, nil is returned.
func (m *intermediateCodeUnit) Lookup(name string) *Symbol {
	if symbol, ok := m.symbolTable[name]; ok {
		return symbol
	}

	return nil
}

// Marshal the intermediate code unit to a JSON object.
func (m *intermediateCodeUnit) MarshalJSON() ([]byte, error) {
	type Embedded intermediateCodeUnit
	instructions := make([]Instruction, 0, m.Instructions.Len())

	// copy the doubly linked instruction-list to a slice of instructions
	for e := m.Instructions.Front(); e != nil; e = e.Next() {
		instructions = append(instructions, *e.Value.(*Instruction))
	}

	// replace the doubly linked instruction-list with a slice of instructions
	mj := &struct {
		*Embedded
		Instructions []Instruction `json:"instructions"`
	}{
		Embedded:     (*Embedded)(m),
		Instructions: instructions,
	}

	return json.Marshal(mj)
}

// Unmarshal the intermediate code unit from a JSON object.
func (m *intermediateCodeUnit) UnmarshalJSON(raw []byte) error {
	type Embedded intermediateCodeUnit

	// struct to unmarshal the JSON object to
	mj := &struct {
		*Embedded
		Instructions []Instruction `json:"instructions"`
	}{
		Embedded: (*Embedded)(m),
	}

	if err := json.Unmarshal(raw, mj); err != nil {
		return err
	}

	// replace the slice of instructions with a doubly linked instruction-list
	for _, i := range mj.Instructions {
		m.AppendInstruction(&i)
	}

	return nil
}

// Print the intermediate code unit to the specified writer.
func (m *intermediateCodeUnit) Print(print io.Writer, args ...any) error {
	// enumerate all instructions in the unit and print them to the writer
	for e := m.Instructions.Front(); e != nil; e = e.Next() {
		if _, err := fmt.Fprintf(print, "%v\n", e.Value); err != nil {
			return cor.NewGeneralError(cor.Generator, failureMap, cor.Error, intermediateCodeExportFailed, nil, err)
		}
	}

	return nil
}

// Export the intermediate code unit to the specified writer in the specified format.
func (m *intermediateCodeUnit) Export(format cor.ExportFormat, print io.Writer) error {
	switch format {
	case cor.Json:
		// export the unit as a JSON object
		if raw, err := json.MarshalIndent(m, "", "  "); err != nil {
			return cor.NewGeneralError(cor.Generator, failureMap, cor.Error, intermediateCodeExportFailed, nil, err)
		} else {
			_, err = print.Write(raw)

			if err != nil {
				err = cor.NewGeneralError(cor.Generator, failureMap, cor.Error, intermediateCodeExportFailed, nil, err)
			}

			return err
		}

	case cor.Text:
		// print is a convenience function to export the intermediate code unit as a string to the print writer
		return m.Print(print)

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownExportFormat, format, nil))
	}
}

// Parse a three-address code address into a value based on its variant and datatype.
func (a *Address) Parse() any {
	switch a.Variant {
	case Literal:
		switch a.DataType {
		case Integer64, Integer32, Integer16, Integer8:
			if decoded, err := strconv.ParseInt(a.Name, 10, a.DataType.BitSize()); err != nil {
				panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, intermediateCodeAddressParsingError, a, err))
			} else {
				if a.DataType == Integer32 {
					return int32(decoded)
				} else if a.DataType == Integer16 {
					return int16(decoded)
				} else if a.DataType == Integer8 {
					return int8(decoded)
				} else {
					return decoded
				}
			}

		case Float64, Float32:
			if decoded, err := strconv.ParseFloat(a.Name, a.DataType.BitSize()); err != nil {
				panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, intermediateCodeAddressParsingError, a, err))
			} else {
				if a.DataType == Float32 {
					return float32(decoded)
				} else {
					return decoded
				}
			}

		case Rune32:
			if decoded, _ := utf8.DecodeRuneInString(a.Name); decoded == utf8.RuneError {
				panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, intermediateCodeAddressParsingError, a, nil))
			} else {
				return decoded
			}

		case Boolean8:
			if decoded, err := strconv.ParseBool(a.Name); err != nil {
				panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, intermediateCodeAddressParsingError, a, err))
			} else {
				return decoded
			}

		default:
			panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unsupportedDataTypeInIntermediateCodeAddress, a, nil))
		}

	case Variable:
		switch a.DataType {
		case Integer64, Integer32, Integer16, Integer8, Float64, Float32, Rune32, Boolean8:
			return a.Offset

		default:
			panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unsupportedDataTypeInIntermediateCodeAddress, a, nil))
		}

	case Temporary:
		switch a.DataType {
		case Integer64, Integer32, Integer16, Integer8, Float64, Float32, Rune32, Boolean8:
			return nil

		default:
			panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unsupportedDataTypeInIntermediateCodeAddress, a, nil))
		}

	case Label:
		switch a.DataType {
		case String:
			return a.Name

		default:
			panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unsupportedDataTypeInIntermediateCodeAddress, a, nil))
		}

	case Count:
		switch a.DataType {
		case UnsignedInteger64:
			if decoded, err := strconv.ParseUint(a.Name, 10, a.DataType.BitSize()); err != nil {
				panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, intermediateCodeAddressParsingError, a, err))
			} else {
				return decoded
			}

		default:
			panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unsupportedDataTypeInIntermediateCodeAddress, a, nil))
		}

	case Code:
		switch a.DataType {
		case Integer64:
			if decoded, err := strconv.ParseInt(a.Name, 10, a.DataType.BitSize()); err != nil {
				panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, intermediateCodeAddressParsingError, a, err))
			} else {
				return decoded
			}

		default:
			panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unsupportedDataTypeInIntermediateCodeAddress, a, nil))
		}

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unexceptedVariantInIntermediateCodeAddress, a, nil))
	}
}

// Validate the set of address variants for a three-address code operation.
func (q *Quadruple) ValidateAddressesContract() {
	contract := intermediateCodeContract[q.Operation]

	for _, addresses := range contract {
		if addresses.Arg1 == q.Arg1.Variant && addresses.Arg2 == q.Arg2.Variant && addresses.Result == q.Result.Variant {
			return
		}
	}

	panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, invalidAddressesContract, q, nil))
}

// Get the instruction at the current position in the list.
func (i *iterator) Current() *Instruction {
	return i.Peek(0)
}

// Move to the first instruction in the list.
func (i *iterator) First() *Instruction {
	if i.instructions.Len() == 0 {
		return nil
	}

	i.current = i.instructions.Front().Next()
	return i.instructions.Front().Value.(*Instruction)
}

// Move to the last instruction in the list.
func (i *iterator) Last() *Instruction {
	if i.instructions.Len() == 0 {
		return nil
	}

	i.current = nil
	return i.instructions.Back().Value.(*Instruction)
}

// Move the iterator to the next instruction.
func (i *iterator) Next() *Instruction {
	if i.current == nil {
		return nil
	}

	instruction := i.current.Value.(*Instruction)
	i.current = i.current.Next()

	return instruction
}

// Move the iterator to the previous instruction.
func (i *iterator) Previous() *Instruction {
	if i.current == nil {
		return nil
	}

	instruction := i.current.Value.(*Instruction)
	i.current = i.current.Prev()

	return instruction
}

// Move the iterator N instructions backward or forward
func (i *iterator) Skip(offset int) *Instruction {
	if offset < 0 {
		for j := 0; j > offset; j-- {
			i.Previous()
		}
	} else if offset > 0 {
		for j := 0; j < offset; j++ {
			i.Next()
		}
	}

	if i.current == nil {
		return nil
	}

	return i.current.Value.(*Instruction)
}

// Peek the instruction at the specified offset from the current instruction.
func (i *iterator) Peek(offset int) *Instruction {
	element := i.current

	if offset < 0 {
		for j := 0; element != nil && j > offset; j-- {
			element = element.Prev()
		}
	} else if offset > 0 {
		for j := 0; element != nil && j < offset; j++ {
			element = element.Next()
		}
	}

	if element == nil {
		return nil
	}

	return element.Value.(*Instruction)
}
