// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package intermediate

import (
	"container/list"
	"encoding/json"
	"fmt"
	"io"

	cor "github.com/petersen65/pl0/v3/core"
)

// Prefixes for pointer and reference modifier string representations.
const (
	pointerPrefix   = "ptr."
	referencePrefix = "ref."
)

type (
	// Represents a logical unit of instructions created from one source file.
	intermediateCodeUnit struct {
		Names        []string           `json:"names"`        // enable deterministic iteration over the symbol table in the order of past inserts
		SymbolTable  map[string]*Symbol `json:"symbol_table"` // symbol table for intermediate code flattened names
		Instructions *list.List         `json:"-"`            // intermediate code instructions as doubly linked list that allows reordering
	}

	// Navigation implementation for the unit's intermediate code instructions.
	iterator struct {
		current      *list.Element `json:"-"`
		instructions *list.List    `json:"-"`
	}
)

var (
	// Map an address variant to its string representation.
	variantNames = map[Variant]string{
		Empty:     "empty",
		Temporary: "temporary",
		Literal:   "literal",
		Variable:  "variable",
	}

	// Map an address datatype to its string representation that is based on the C language.
	dataTypeNames = map[DataType]string{
		Untyped:    "void",
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
		Boolean:    "uint8",
		Character:  "codepoint",
		String:     "text",
	}

	// Map three-address code operations of the intermediate code to their string representation.
	operationNames = map[Operation]string{
		// unary arithmetic or logical operation
		Odd:    "odd",
		Negate: "negate",

		// binary arithmetic operation
		Plus:   "add",
		Minus:  "subtract",
		Times:  "multiply",
		Divide: "divide",

		// binary comparison operation
		Equal:        "equal",
		NotEqual:     "notEqual",
		Less:         "less",
		LessEqual:    "lessEqual",
		Greater:      "greater",
		GreaterEqual: "greaterEqual",

		// unconditional or conditional jump
		Jump:             "jump",
		JumpEqual:        "jumpEqual",
		JumpNotEqual:     "jumpNotEqual",
		JumpLess:         "jumpLess",
		JumpLessEqual:    "jumpLessEqual",
		JumpGreater:      "jumpGreater",
		JumpGreaterEqual: "jumpGreaterEqual",
		BranchTarget:     "branchTarget",

		// function call and parameter passing
		Parameter: "parameter",
		Call:      "call",
		Return:    "return",

		// function entry and exit sequences
		Prologue: "prologue",
		Epilogue: "epilogue",
		Setup:    "setup",

		// memory management and handling of variables or literals
		AllocateVariable: "allocateVariable",
		CopyLiteral:      "copyLiteral",
		LoadVariable:     "loadVariable",
		StoreVariable:    "storeVariable",
	}

	// The intermediate code contract maps all three-address code operations to their addresses contracts for validation.
	intermediateCodeContract = map[Operation][]AddressesContract{
		// unary arithmetic or logical operation
		Odd:    {{Arg1: Temporary, Arg2: Empty, Result: Empty}},
		Negate: {{Arg1: Temporary, Arg2: Empty, Result: Temporary}},

		// binary arithmetic operation
		Plus:   {{Arg1: Temporary, Arg2: Temporary, Result: Temporary}},
		Minus:  {{Arg1: Temporary, Arg2: Temporary, Result: Temporary}},
		Times:  {{Arg1: Temporary, Arg2: Temporary, Result: Temporary}},
		Divide: {{Arg1: Temporary, Arg2: Temporary, Result: Temporary}},

		// binary comparison operation
		Equal:        {{Arg1: Temporary, Arg2: Temporary, Result: Empty}},
		NotEqual:     {{Arg1: Temporary, Arg2: Temporary, Result: Empty}},
		Less:         {{Arg1: Temporary, Arg2: Temporary, Result: Empty}},
		LessEqual:    {{Arg1: Temporary, Arg2: Temporary, Result: Empty}},
		Greater:      {{Arg1: Temporary, Arg2: Temporary, Result: Empty}},
		GreaterEqual: {{Arg1: Temporary, Arg2: Temporary, Result: Empty}},

		// unconditional or conditional jump
		Jump:             {{Arg1: Empty, Arg2: Empty, Result: Literal}},
		JumpEqual:        {{Arg1: Empty, Arg2: Empty, Result: Literal}},
		JumpNotEqual:     {{Arg1: Empty, Arg2: Empty, Result: Literal}},
		JumpLess:         {{Arg1: Empty, Arg2: Empty, Result: Literal}},
		JumpLessEqual:    {{Arg1: Empty, Arg2: Empty, Result: Literal}},
		JumpGreater:      {{Arg1: Empty, Arg2: Empty, Result: Literal}},
		JumpGreaterEqual: {{Arg1: Empty, Arg2: Empty, Result: Literal}},
		BranchTarget:     {{Arg1: Empty, Arg2: Empty, Result: Literal}},

		// function call and parameter passing
		Parameter: {{Arg1: Temporary, Arg2: Empty, Result: Empty}},
		Call:      {{Arg1: Literal, Arg2: Literal, Result: Empty}, {Arg1: Literal, Arg2: Literal, Result: Temporary}},
		Return:    {{Arg1: Literal, Arg2: Empty, Result: Empty}, {Arg1: Literal, Arg2: Empty, Result: Temporary}},

		// function entry and exit sequences
		Prologue: {{Arg1: Literal, Arg2: Empty, Result: Empty}},
		Epilogue: {{Arg1: Empty, Arg2: Empty, Result: Empty}},
		Setup:    {{Arg1: Literal, Arg2: Empty, Result: Empty}},

		// memory management and handling of variables or literals
		AllocateVariable: {{Arg1: Empty, Arg2: Empty, Result: Variable}},
		CopyLiteral:      {{Arg1: Literal, Arg2: Literal, Result: Temporary}},
		LoadVariable:     {{Arg1: Variable, Arg2: Literal, Result: Temporary}},
		StoreVariable:    {{Arg1: Temporary, Arg2: Literal, Result: Variable}},
	}
)

// Create a new intermediate code unit and initialize it with a unique identifier.
func newIntermediateCodeUnit() IntermediateCodeUnit {
	return &intermediateCodeUnit{
		Names:        make([]string, 0),
		SymbolTable:  make(map[string]*Symbol),
		Instructions: list.New(),
	}
}

// String representation of a datatype with its modifiers.
func (dt DataType) String() string {
	var prefix string

	if dt.IsPointer() {
		prefix = pointerPrefix
	} else if dt.IsReference() {
		prefix = referencePrefix
	}

	return fmt.Sprintf("%v%v", prefix, dataTypeNames[dt.AsPlain()])
}

// String representation of the three-address code address.
func (a *Address) String() string {
	const maxWidth = 30
	var representation string

	switch {
	case a.Variant == Empty:
		representation = fmt.Sprintf("%v", a.Variant)

	case len(a.Name) > 0 && a.Value != nil:
		representation = fmt.Sprintf("%v %v %v %v", a.Variant, a.DataType, a.Name, a.Value)

	case len(a.Name) > 0:
		representation = fmt.Sprintf("%v %v %v", a.Variant, a.DataType, a.Name)

	case a.Value != nil:
		representation = fmt.Sprintf("%v %v %v", a.Variant, a.DataType, a.Value)

	default:
		representation = fmt.Sprintf("%v %v", a.Variant, a.DataType)
	}

	if len(representation) > maxWidth {
		representation = representation[:maxWidth]
	}

	return representation
}

// String representation of a three-address code quadruple.
func (q *Quadruple) String() string {
	const operationWidth = 20
	const argWidth = 30

	return fmt.Sprintf(
		"%-*v %-*v %-*v %-*v",
		operationWidth, q.Operation,
		argWidth, q.Arg1,
		argWidth, q.Arg2,
		argWidth, q.Result,
	)
}

// Append a new instruction to the intermediate code.
func (u *intermediateCodeUnit) AppendInstruction(operation Operation, arg1, arg2, result *Address, tokenStreamIndex int) *list.Element {
	return u.Instructions.PushBack(NewInstruction(operation, arg1, arg2, result, tokenStreamIndex))
}

// Append an existing instruction to the intermediate code.
func (u *intermediateCodeUnit) AppendExistingInstruction(instruction *Instruction) *list.Element {
	return u.Instructions.PushBack(instruction)
}

// Get an instruction iterator for the intermediate code unit.
func (u *intermediateCodeUnit) GetIterator() Iterator {
	return &iterator{current: u.Instructions.Front(), instructions: u.Instructions}
}

// Insert a symbol into the intermediate code symbol table. If the symbol already exists, it will be overwritten.
func (u *intermediateCodeUnit) Insert(symbol *Symbol) {
	if u.Lookup(symbol.Name) == nil {
		u.Names = append(u.Names, symbol.Name)
	}

	u.SymbolTable[symbol.Name] = symbol
}

// Lookup a symbol in the the intermediate code symbol table. If the symbol is not found, nil is returned.
func (u *intermediateCodeUnit) Lookup(name string) *Symbol {
	if symbol, ok := u.SymbolTable[name]; ok {
		return symbol
	}

	return nil
}

// Marshal the intermediate code unit to a JSON object because the JSON encoder does not support the "list.List" type directly.
func (u *intermediateCodeUnit) MarshalJSON() ([]byte, error) {
	type embedded intermediateCodeUnit
	instructions := make([]Instruction, 0, u.Instructions.Len())

	// copy the doubly linked instruction-list to a slice of instructions
	for e := u.Instructions.Front(); e != nil; e = e.Next() {
		instructions = append(instructions, *e.Value.(*Instruction))
	}

	// create a JSON-compliant intermediate code unit structure that embeds the original unit
	// note: replace the doubly linked instruction-list with a slice of instructions
	jsonCompliantIntermediateCodeUnit := &struct {
		*embedded
		Instructions []Instruction `json:"instructions"`
	}{
		embedded:     (*embedded)(u),
		Instructions: instructions,
	}

	// marshal the intermediate code unit as data type "embedded" to prevent recursion
	return json.Marshal(jsonCompliantIntermediateCodeUnit)
}

// Unmarshal the intermediate code unit from a JSON object because the JSON decoder does not support the "list.List" type directly.
func (u *intermediateCodeUnit) UnmarshalJSON(raw []byte) error {
	type embedded intermediateCodeUnit

	// unmarshal the JSON object into a JSON-compliant intermediate code unit structure
	jsonCompliantIntermediateCodeUnit := &struct {
		*embedded
		Instructions []Instruction `json:"instructions"`
	}{
		embedded: (*embedded)(u),
	}

	// unmarshal the intermediate code unit as data type "embedded" to prevent recursion
	if err := json.Unmarshal(raw, jsonCompliantIntermediateCodeUnit); err != nil {
		return err
	}

	// replace the slice of instructions with a doubly linked instruction-list
	for _, i := range jsonCompliantIntermediateCodeUnit.Instructions {
		u.AppendExistingInstruction(&i)
	}

	return nil
}

// Print the intermediate code unit to the specified writer.
func (u *intermediateCodeUnit) Print(print io.Writer, args ...any) error {
	// enumerate all instructions in the unit and print them to the writer
	for e := u.Instructions.Front(); e != nil; e = e.Next() {
		if _, err := fmt.Fprintf(print, "%v\n", e.Value); err != nil {
			return cor.NewGeneralError(cor.Intermediate, failureMap, cor.Error, intermediateCodeExportFailed, nil, err)
		}
	}

	return nil
}

// Export the intermediate code unit to the specified writer in the specified format.
func (u *intermediateCodeUnit) Export(format cor.ExportFormat, print io.Writer) error {
	// JSON formatting requires a prefix and indent for pretty printing
	const prefix, indent = "", "  "

	switch format {
	case cor.Json:
		// export the intermediate code unit as a JSON object
		if raw, err := json.MarshalIndent(u, prefix, indent); err != nil {
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
		return u.Print(print)

	default:
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unknownExportFormat, format, nil))
	}
}

// Validate the set of addresses for a three-address code operation and return the index of the contract.
func (q *Quadruple) ValidateAddressesContract() AddressesContract {
	// validate each address in the quadruple
	if !q.Arg1.Validate() || !q.Arg2.Validate() || !q.Result.Validate() {
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, invalidIntermediateCodeAddress, q, nil))
	}

	// determine the contract for the operation
	contract := intermediateCodeContract[q.Operation]

	// check whether the addresses of the operation match any of its contracts
	for _, addresses := range contract {
		if addresses.Arg1 == q.Arg1.Variant && addresses.Arg2 == q.Arg2.Variant && addresses.Result == q.Result.Variant {
			return addresses
		}
	}

	panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, invalidAddressesContract, q, nil))
}

// Validate variants and data types for a three-address code address and return a boolean indicating whether the address is valid.
func (a *Address) Validate() bool {
	switch a.Variant {
	case Empty:
		return a.DataType.IsUntyped()

	case Temporary, Literal, Variable:
		return a.DataType.IsSupported()

	default:
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unexceptedVariantInIntermediateCodeAddress, a, nil))
	}
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
