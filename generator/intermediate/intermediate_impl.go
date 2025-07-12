// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package intermediate

import (
	"container/list"
	"encoding/json"
	"fmt"
	"io"
	"strconv"

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
)

var (
	// variantNames maps an address variant to its string representation.
	variantNames = map[Variant]string{
		Empty:    "ety",
		Register: "reg",
		Literal:  "lit",
		Variable: "var",
	}

	// dataTypeNames maps an address datatype to its string representation.
	dataTypeNames = map[DataType]string{
		Untyped:    "untyped",
		Integer64:  "int64",
		Integer32:  "int32",
		Integer16:  "int16",
		Integer8:   "int8",
		Float64:    "float64",
		Float32:    "float32",
		Unsigned64: "uint64",
		Unsigned32: "uint32",
		Unsigned16: "uint16",
		Unsigned8:  "uint8",
		Boolean:    "bool",
		Character:  "char32",
		String:     "string",
	}

	// Map three-address code operations of the intermediate code to their string representation.
	operationNames = map[Operation]string{
		Odd:              "odd",
		Negate:           "negate",
		Plus:             "add",
		Minus:            "subtract",
		Times:            "multiply",
		Divide:           "divide",
		Equal:            "equal",
		NotEqual:         "notEqual",
		Less:             "less",
		LessEqual:        "lessEqual",
		Greater:          "greater",
		GreaterEqual:     "greaterEqual",
		Jump:             "jump",
		JumpEqual:        "jumpEqual",
		JumpNotEqual:     "jumpNotEqual",
		JumpLess:         "jumpLess",
		JumpLessEqual:    "jumpLessEqual",
		JumpGreater:      "jumpGreater",
		JumpGreaterEqual: "jumpGreaterEqual",
		Parameter:        "parameter",
		Call:             "call",
		Prologue:         "prologue",
		Epilogue:         "epilogue",
		Setup:            "setup",
		Return:           "return",
		BranchTarget:     "branchTarget",
		AllocateVariable: "allocateVariable",
		CopyLiteral:      "copyLiteral",
		LoadVariable:     "loadVariable",
		StoreVariable:    "storeVariable",
	}

	// The intermediate code contract maps all three-address code operations to their addresses contracts for validation.
	intermediateCodeContract = map[Operation][]AddressesContract{
		Odd: {
			{Arg1: Register, Arg2: Empty, Result: Empty},
		},
		Negate: {
			{Arg1: Register, Arg2: Empty, Result: Register},
		},
		Plus: {
			{Arg1: Register, Arg2: Register, Result: Register},
		},
		Minus: {
			{Arg1: Register, Arg2: Register, Result: Register},
		},
		Times: {
			{Arg1: Register, Arg2: Register, Result: Register},
		},
		Divide: {
			{Arg1: Register, Arg2: Register, Result: Register},
		},
		Equal: {
			{Arg1: Register, Arg2: Register, Result: Empty},
		},
		NotEqual: {
			{Arg1: Register, Arg2: Register, Result: Empty},
		},
		Less: {
			{Arg1: Register, Arg2: Register, Result: Empty},
		},
		LessEqual: {
			{Arg1: Register, Arg2: Register, Result: Empty},
		},
		Greater: {
			{Arg1: Register, Arg2: Register, Result: Empty},
		},
		GreaterEqual: {
			{Arg1: Register, Arg2: Register, Result: Empty},
		},
		Jump: {
			{Arg1: Literal, Arg2: Empty, Result: Empty},
		},
		JumpEqual: {
			{Arg1: Literal, Arg2: Empty, Result: Empty},
		},
		JumpNotEqual: {
			{Arg1: Literal, Arg2: Empty, Result: Empty},
		},
		JumpLess: {
			{Arg1: Literal, Arg2: Empty, Result: Empty},
		},
		JumpLessEqual: {
			{Arg1: Literal, Arg2: Empty, Result: Empty},
		},
		JumpGreater: {
			{Arg1: Literal, Arg2: Empty, Result: Empty},
		},
		JumpGreaterEqual: {
			{Arg1: Literal, Arg2: Empty, Result: Empty},
		},
		Parameter: {
			{Arg1: Register, Arg2: Empty, Result: Empty},
			{Arg1: Literal, Arg2: Empty, Result: Empty},
			{Arg1: Variable, Arg2: Empty, Result: Empty},
		},
		Call: {
			{Arg1: Literal, Arg2: Empty, Result: Empty},
		},
		Prologue: {
			{Arg1: Empty, Arg2: Empty, Result: Empty},
		},
		Epilogue: {
			{Arg1: Empty, Arg2: Empty, Result: Empty},
		},
		Setup: {
			{Arg1: Literal, Arg2: Empty, Result: Empty},
		},
		Return: {
			{Arg1: Empty, Arg2: Empty, Result: Empty},
			{Arg1: Literal, Arg2: Empty, Result: Empty},
		},
		BranchTarget: {
			{Arg1: Empty, Arg2: Empty, Result: Literal},
		},
		AllocateVariable: {
			{Arg1: Empty, Arg2: Empty, Result: Variable},
		},
		CopyLiteral: {
			{Arg1: Literal, Arg2: Empty, Result: Register},
		},
		LoadVariable: {
			{Arg1: Variable, Arg2: Empty, Result: Register},
		},
		StoreVariable: {
			{Arg1: Register, Arg2: Empty, Result: Variable},
		},
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
func newInstruction(operation Operation, arg1, arg2, result *Address, tokenStreamIndex int) *Instruction {
	return &Instruction{
		Quadruple:        Quadruple{Operation: operation, Arg1: arg1, Arg2: arg2, Result: result},
		TokenStreamIndex: tokenStreamIndex,
	}
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
			return cor.NewGeneralError(cor.Intermediate, failureMap, cor.Error, intermediateCodeExportFailed, nil, err)
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
			return cor.NewGeneralError(cor.Intermediate, failureMap, cor.Error, intermediateCodeExportFailed, nil, err)
		} else {
			_, err = print.Write(raw)

			if err != nil {
				err = cor.NewGeneralError(cor.Intermediate, failureMap, cor.Error, intermediateCodeExportFailed, nil, err)
			}

			return err
		}

	case cor.Text:
		// print is a convenience function to export the intermediate code unit as a string to the print writer
		return m.Print(print)

	default:
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unknownExportFormat, format, nil))
	}
}

// Parse a three-address code address into a value based on its variant and datatype.
func (a *Address) Parse() any {
	switch a.Variant {
	case Empty:
		switch a.DataType {
		case Untyped:
			return nil

		default:
			panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unsupportedDataTypeInIntermediateCodeAddress, a, nil))
		}

	case Literal:
		switch a.DataType {
		case Integer64, Integer32, Integer16, Integer8:
			if decoded, err := strconv.ParseInt(a.Name, 10, a.DataType.bitSize()); err != nil {
				panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, intermediateCodeAddressParsingError, a, err))
			} else {
				switch a.DataType {
				case Integer32:
					return int32(decoded)
				case Integer16:
					return int16(decoded)
				case Integer8:
					return int8(decoded)
				default:
					return decoded
				}
			}

		case Float64, Float32:
			if decoded, err := strconv.ParseFloat(a.Name, a.DataType.bitSize()); err != nil {
				panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, intermediateCodeAddressParsingError, a, err))
			} else {
				if a.DataType == Float32 {
					return float32(decoded)
				} else {
					return decoded
				}
			}

		case Unsigned64, Unsigned32, Unsigned16, Unsigned8:
			if decoded, err := strconv.ParseUint(a.Name, 10, a.DataType.bitSize()); err != nil {
				panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, intermediateCodeAddressParsingError, a, err))
			} else {
				switch a.DataType {
				case Unsigned32:
					return uint32(decoded)
				case Unsigned16:
					return uint16(decoded)
				case Unsigned8:
					return uint8(decoded)
				default:
					return decoded
				}
			}

		case Boolean:
			if decoded, err := strconv.ParseBool(a.Name); err != nil {
				panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, intermediateCodeAddressParsingError, a, err))
			} else {
				return decoded
			}

		default:
			panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unsupportedDataTypeInIntermediateCodeAddress, a, nil))
		}

	case Variable, Register:
		switch a.DataType {
		case Integer64, Integer32, Integer16, Integer8, Float64, Float32, Unsigned64, Unsigned32, Unsigned16, Unsigned8, Boolean:
			return nil

		default:
			panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unsupportedDataTypeInIntermediateCodeAddress, a, nil))
		}

	default:
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unexceptedVariantInIntermediateCodeAddress, a, nil))
	}
}

// Validate the set of address variants for a three-address code operation and return the index of the contract.
func (q *Quadruple) ValidateAddressesContract() AddressesContract {
	contract := intermediateCodeContract[q.Operation]

	for _, addresses := range contract {
		if addresses.Arg1 == q.Arg1.Variant && addresses.Arg2 == q.Arg2.Variant && addresses.Result == q.Result.Variant {
			return addresses
		}
	}

	panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, invalidAddressesContract, q, nil))
}

// Get the instruction at the current position in the list.
func (i *iterator) Current() *Instruction {
	if i.current == nil {
		return nil
	}

	return i.current.Value.(*Instruction)
}

// Move to the first instruction in the list.
func (i *iterator) First() *Instruction {
	i.current = i.instructions.Front()
	return i.Current()
}

// Move to the last instruction in the list.
func (i *iterator) Last() *Instruction {
	i.current = i.instructions.Back()
	return i.Current()
}

// Move the iterator to the next instruction.
func (i *iterator) Next() *Instruction {
	if i.current == nil {
		return nil
	}

	i.current = i.current.Next()
	return i.Current()
}

// Move the iterator to the previous instruction.
func (i *iterator) Previous() *Instruction {
	if i.current == nil {
		return nil
	}

	i.current = i.current.Prev()
	return i.Current()
}

// Move the iterator N instructions backward or forward
func (i *iterator) Skip(offset int) *Instruction {
	if offset < 0 {
		for range -offset {
			i.Previous()
		}
	} else if offset > 0 {
		for range offset {
			i.Next()
		}
	}

	return i.Current()
}

// Peek the instruction at the specified offset from the current instruction.
func (i *iterator) Peek(offset int) *Instruction {
	element := i.current

	if offset < 0 {
		for range -offset {
			if element == nil {
				break
			}

			element = element.Prev()
		}
	} else if offset > 0 {
		for range offset {
			if element == nil {
				break
			}

			element = element.Next()
		}
	}

	if element == nil {
		return nil
	}

	return element.Value.(*Instruction)
}

// Determine the bit size of a datatype or return -1 if there is no defined bit size.
func (dataType DataType) bitSize() int {
	switch dataType {
	case Integer64, Float64, Unsigned64:
		return 64

	case Integer32, Float32, Unsigned32, Character:
		return 32

	case Integer16, Unsigned16:
		return 16

	case Integer8, Unsigned8, Boolean:
		return 8

	default:
		return -1 // no defined bit size for this datatype
	}
}
