// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package intermediate implements the intermediate code language which is based on the three-address code concept.
package intermediate

import (
	"container/list"
	"fmt"
	"io"

	cor "github.com/petersen65/PL0/v2/core"
)

// UnusedDifference states that an intermediate code instruction does not use a block nesting depth difference.
const UnusedDifference = -1

// NoLabel is a constant for an empty label in an intermediate code instruction.
const NoLabel = ""

// Three-address code operations of the intermediate code.
const (
	_ = Operation(iota)

	// x = op y, where op is a unary arithmetic or logical operation and x, y are addresses
	Odd    // unary logical operation 'odd'
	Negate // unary arithmetic operation 'negation'

	// x = y op z, where op is a binary arithmetic or relational operation, x, y, z are addresses
	Plus         // binary arithmetic operation '+'
	Minus        // binary arithmetic operation '-'
	Times        // binary arithmetic operation '*'
	Divide       // binary arithmetic operation '/'
	Equal        // binary relational operation '='
	NotEqual     // binary relational operation '#'
	Less         // binary relational operation '<'
	LessEqual    // binary relational operation '<='
	Greater      // binary relational operation '>'
	GreaterEqual // binary relational operation '>='

	// an unconditional or conditional jump goto L: the instruction with label L is the next to be executed
	Jump             // unconditional jump
	JumpEqual        // conditional jump '='
	JumpNotEqual     // conditional jump '#'
	JumpLess         // conditional jump '<'
	JumpLessEqual    // conditional jump '<='
	JumpGreater      // conditional jump '>'
	JumpGreaterEqual // conditional jump '>='

	// call f, n and y = call f, n for function calls, n is the number of actual parameters in "call f, n"
	Parameter // pass parameter to function
	Call      // call function
	Prelude   // prelude inside the body of a function
	Epilog    // epilog inside the body of a function
	Return    // return from function
	Standard  // call function of the external standard library
	Target    // target for any branching operation

	Allocate      // allocate memory for a variable in its logical memory space
	ValueCopy     // copy the value of a constant or literal into an address
	VariableLoad  // load variable value from the logical memory space into an address
	VariableStore // store variable value from an address into the logical memory space
)

// Variants of an address in the three-address code concept.
const (
	Empty     = Variant(iota) // empty address holds no address
	Metadata                  // metadata address used to store any additional information about an operation
	Temporary                 // temporary address holds a result of an operation
	Literal                   // literal address holds a constant or literal value
	Variable                  // variable address holds an argument of an operation
	Label                     // label address used as target for jumps and calls is resolved by the linker
	Count                     // count address is used for counting purposes like the number of parameters in a function call
	Code                      // code address holds a call code for the external standard library (e.g. readln, writeln)
)

// Data types supported for an address of the three-address code concept.
const (
	Void   = DataType(iota) // an address that does not have a data type
	String                  // the string data type is used for labels in label addresses

	// data types supported for constants, literals, variables, and temporaries (postfix specifies the bit size)
	Integer64  // signed 64-bit integer
	Integer32  // signed 32-bit integer
	Integer16  // signed 16-bit integer
	Integer8   // signed 8-bit integer
	Float64    // signed 64-bit floating point number
	Float32    // signed 32-bit floating point number
	Unsigned64 // unsigned 64-bit integer
	Unsigned32 // unsigned 32-bit integer
	Unsigned16 // unsigned 16-bit integer
	Unsigned8  // unsigned 8-bit integer
	Rune32     // unsigned 32-bit rune (UTF-8 character)
	Boolean8   // unsigned 8-bit boolean value (true/false)
)

// Prefixes for address names in the three-address code concept if a name does not contain a value.
const (
	LabelPrefix    = PrefixType(iota) // the label prefix is used for instruction labels and the address variant 'Label'
	ResultPrefix                      // an address variant 'Temporary' always has a name that starts with prefix 'ResultPrefix'
	ConstantPrefix                    // the constant prefix is used for constance names in the intermediate code
	VariablePrefix                    // the variable prefix is used for variable names in the intermediate code
	FunctionPrefix                    // the function prefix is used for function names in the intermediate code
)

// Kind of supported symbol entry.
const (
	ConstantSymbol = Entry(iota)
	VariableSymbol
	FunctionSymbol
)

// External standard functions provided for the programming language.
// needs to be alligned with call codes for the programming language standard library
const (
	ReadLn  = StandardFunction(iota) // readln function reads a line from the standard input stream
	WriteLn                          // writeln function writes a line to the standard output stream
)

type (
	// The variant type is used to distinguish between different kinds of addresses in the three-address code concept.
	Variant int

	// The data type of an address in the three-address code concept.
	DataType int

	// String representation of a data type.
	DataTypeRepresentation string

	// Enumeration of prefixes used for names of addresses.
	PrefixType int

	// Type for three-address code operations.
	Operation int

	// Kind of symbol entries.
	Entry int

	// Enumeration of standard functions that belong to the external standard library.
	StandardFunction int64

	// Address is the data structure of an address in the three-address code concept.
	Address struct {
		Name     string   `json:"name"`      // name or value of an address (a value needs to be converted to a string)
		Variant  Variant  `json:"variant"`   // variant of what the address represents
		DataType DataType `json:"data_type"` // data type of the address
	}

	// Quadruple represents a single three-address code operation with its three addresses (arg1, arg2, result).
	Quadruple struct {
		Operation Operation `json:"operation"` // three-address code operation
		Arg1      *Address  `json:"arg_1"`     // first address (argument 1)
		Arg2      *Address  `json:"arg_2"`     // second address (argument 2)
		Result    *Address  `json:"result"`    // third address (result)
	}

	// A contract for addresses describes a valid set of address variants for a three-address code operation.
	AddressesContract struct {
		Arg1   Variant // first address variant
		Arg2   Variant // second address variant
		Result Variant // third address variant
	}

	// Instruction represents a single three-address code operation with its required metadata (e.g. label, block nesting depth difference).
	Instruction struct {
		Label            string    `json:"label"`              // branch-label for any branching operation
		DepthDifference  int32     `json:"depth_difference"`   // block nesting depth difference between variable use and variable declaration
		TokenStreamIndex int       `json:"token_stream_index"` // index of the token in the token stream
		ThreeAddressCode Quadruple `json:"code"`               // three-address code operation
	}

	// A Symbol represents a flattened name in the intermediate code.
	Symbol struct {
		Name       string        // flattened name in the intermediate code
		Kind       Entry         // kind of symbol entry
		DataType   DataType      // datatype of the symbol
		Definition *list.Element // instruction where the symbol is defined
	}

	// IntermediateCodeUnit represents a logical unit of instructions created from one source file.
	IntermediateCodeUnit interface {
		AppendInstruction(instruction *Instruction) *list.Element
		GetIterator() Iterator
		Insert(symbol *Symbol)
		Lookup(name string) *Symbol
		Print(print io.Writer, args ...any) error
		Export(format cor.ExportFormat, print io.Writer) error
	}

	// The iterator interface provides navigation methods for the units's intermediate code instructions.
	Iterator interface {
		Current() *Instruction
		First() *Instruction
		Last() *Instruction
		Next() *Instruction
		Previous() *Instruction
		Skip(offset int) *Instruction
		Peek(offset int) *Instruction
	}
)

// Return the interface of the intermediate code unit implementation.
func NewIntermediateCodeUnit() IntermediateCodeUnit {
	return newIntermediateCodeUnit()
}

// Create a new three-address code instruction with an operation, two arguments, a result, and some options.
func NewInstruction(operation Operation, arg1, arg2, result *Address, options ...any) *Instruction {
	return newInstruction(operation, arg1, arg2, result, options...)
}

// Create a new three-address code argument or result address.
func NewAddress(name any, variant Variant, dataType DataType) *Address {
	return &Address{Name: fmt.Sprintf("%v", name), Variant: variant, DataType: dataType}
}

// Create new symbol for the intermediate code.
func NewSymbol(name string, kind Entry, dataType DataType) *Symbol {
	return &Symbol{Name: name, Kind: kind, DataType: dataType}
}

// String representation of a variant.
func (v Variant) String() string {
	return variantNames[v]
}

// String representation of a datatype.
func (dt DataType) String() string {
	return dataTypeNames[dt]
}

// Get a datatype from its representation.
func (dtr DataTypeRepresentation) DataType() DataType {
	for dataType, representation := range dataTypeNames {
		if representation == string(dtr) {
			return dataType
		}
	}

	panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unknownDataTypeRepresentation, dtr, nil))
}

// String representation of the three-address code address.
func (a *Address) String() string {
	representation := fmt.Sprintf("%v %v %v", a.Variant, a.DataType, a.Name)

	if a.Variant == Empty {
		representation = ""
	}

	if len(representation) > 22 {
		representation = representation[:22]
	}

	return representation
}

// String representation of an three-address code operation.
func (o Operation) String() string {
	return operationNames[o]
}

// String representation of a three-address code quadruple.
func (q *Quadruple) String() string {
	return fmt.Sprintf("%-12v %-22v %-22v %-22v", q.Operation, q.Arg1, q.Arg2, q.Result)
}

// String representation of an intermediate code instruction.
func (i *Instruction) String() string {
	var depthDifference any = i.DepthDifference

	if i.DepthDifference == UnusedDifference {
		depthDifference = ""
	}

	return fmt.Sprintf(
		"%-8v %4v    %-12v   %-22v   %-22v   %-22v",
		i.Label,
		depthDifference,
		i.ThreeAddressCode.Operation,
		i.ThreeAddressCode.Arg1,
		i.ThreeAddressCode.Arg2,
		i.ThreeAddressCode.Result)
}

// Supported data types for constants, literals, variables, and temporaries.
func (dataType DataType) IsSupported() bool {
	return dataType >= Integer64 && dataType <= Boolean8
}

// Check whether the datatype has a signed representation.
func (dataType DataType) IsSigned() bool {
	return dataType >= Integer64 && dataType <= Float32
}

// Check whether the datatype has an unsigned representation.
func (dataType DataType) IsUnsigned() bool {
	return dataType >= Unsigned64 && dataType <= Boolean8
}

// Check whether the datatype is a signed integer.
func (dataType DataType) IsSignedInteger() bool {
	return dataType >= Integer64 && dataType <= Integer8
}

// Check whether the datatype is an unsigned integer.
func (dataType DataType) IsUnsignedInteger() bool {
	return dataType >= Unsigned64 && dataType <= Unsigned8
}

// Check whether the datatype is an integer.
func (dataType DataType) IsInteger() bool {
	return dataType.IsSignedInteger() || dataType.IsUnsignedInteger()
}

// Check whether the datatype is a floating point number.
func (dataType DataType) IsFloatingPoint() bool {
	return dataType == Float64 || dataType == Float32
}

// Check whether the datatype is a character.
func (dataType DataType) IsCharacter() bool {
	return dataType == Rune32
}

// Check whether the datatype is a boolean.
func (dataType DataType) IsBoolean() bool {
	return dataType == Boolean8
}
