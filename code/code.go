// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package code

import (
	"container/list"
	"encoding/json"
	"fmt"
	"io"
	"strconv"

	"github.com/google/uuid"
	ast "github.com/petersen65/PL0/v2/ast"
	cor "github.com/petersen65/PL0/v2/core"
)

// Number of bits of a signed or unsigned integer.
const integerBitSize = 64

// Display name of entry point only used for informational purposes.
const entryPointDisplayName = "@main"

// Abstract syntax extension for the scope.
const scopeExtension ast.ExtensionType = 16

// Abstract syntax extension for the symbol.
const symbolExtension ast.ExtensionType = 17

// Kind of supported symbol entry.
const (
	_ = entry(iota)
	constant
	variable
	function
)

type (
	// Intermediate code generation compiler phase. It implements the Visitor interface to traverse the AST and generate code.
	intermediateCode struct {
		abstractSyntax ast.Block  // abstract syntax tree to generate code for
		module         *module    // module to store the generated intermediate code
		results        *list.List // last-in-first-out results-list holding temporary results from expressions
	}

	// Module represents a logical unit of instructions created from one source file so that a program can be linked together from multiple modules.
	module struct {
		UniqueId     string             `json:"unique_id"`    // unique identifier of the module
		names        []string           `json:"-"`            // enable deterministic iteration over the symbol table in the order of past inserts
		symbolTable  map[string]*symbol `json:"-"`            // symbol table for intermediate code flattened names
		Instructions *list.List         `json:"instructions"` // intermediate code instructions as doubly linked list that allows reordering
	}

	// Metadata for each scope in the abstract syntax tree.
	scopeMetaData struct {
		counter uint64 // counter for all variables in an abstract syntax scope
	}

	// Metadata for each symbol in the abstract syntax tree.
	symbolMetaData struct {
		name string // intermediate code flattened name created from a scoped abstract syntax symbol
	}

	// Kind of symbol entries.
	entry int

	// A symbol represents a flattened name in the intermediate code that was created from a scoped abstract syntax symbol.
	symbol struct {
		name       string        // flattened name in the intermediate code
		kind       entry         // kind of symbol entry
		dataType   DataType      // data type of the symbol
		location   uint64        // location in the logical memory space
		definition *list.Element // instruction where the symbol is defined
	}

	// Navigation implementation for the module's intermediate code instructions.
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
	// Map abstract syntax data types to intermediate code data types (they have separate type systems)
	dataTypeMap = map[ast.DataType]DataType{
		ast.Integer64: Integer64,
	}

	// variantNames maps an address variant to its string representation.
	variantNames = map[Variant]string{
		Empty:      "empty",
		Diagnostic: "diagnostic",
		Temporary:  "temporary",
		Literal:    "literal",
		Variable:   "variable",
		Label:      "label",
		Count:      "count",
		Code:       "code",
	}

	// dataTypeNames maps an address data type to its string representation.
	dataTypeNames = map[DataType]string{
		Void:              "void",
		String:            "string",
		UnsignedInteger64: "uint64",
		Integer64:         "int64",
	}

	// Prefixes used for names of addresses.
	prefix = map[PrefixType]rune{
		LabelPrefix:    'l',
		ResultPrefix:   't',
		ConstantPrefix: 'c',
		VariablePrefix: 'v',
		FunctionPrefix: 'f',
	}

	// noAddress represents an unused address in the three-address code concept.
	noAddress = &Address{Name: "-", Variant: Empty, DataType: Void, Location: 0}

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

	// The intermediate code contract maps all three-address code operations to their address contracts for validation.
	intermediateCodeContract = map[Operation][]addressesContract{
		Odd:              {{Arg1: Temporary, Arg2: Empty, Result: Empty}},
		Negate:           {{Arg1: Temporary, Arg2: Empty, Result: Empty}},
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

// Create a new intermediate code generator.
func newIntermediateCode(abstractSyntax ast.Block) IntermediateCode {
	return &intermediateCode{
		abstractSyntax: abstractSyntax,
		module:         NewModule().(*module),
		results:        list.New(),
	}
}

// Create a new intermediate code module and initialize it with a unique identifier.
func newModule() Module {
	return &module{
		UniqueId:     uuid.NewString(),
		names:        make([]string, 0),
		symbolTable:  make(map[string]*symbol),
		Instructions: list.New(),
	}
}

// Create new symbol for the intermediate code.
func newSymbol(name string, kind entry, dataType DataType) *symbol {
	return &symbol{name: name, kind: kind, dataType: dataType}
}

// Create metadata for a scope in the abstract syntax tree.
func newScopeMetaData() *scopeMetaData {
	return &scopeMetaData{counter: 1}
}

// Create metadata for a symbol in the abstract syntax tree.
func newSymbolMetaData(name string) *symbolMetaData {
	return &symbolMetaData{name: name}
}

// String representation of a variant.
func (v Variant) String() string {
	return variantNames[v]
}

// String representation of a data type.
func (dt DataType) String() string {
	return dataTypeNames[dt]
}

// Get a data type from its representation.
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
	representation := fmt.Sprintf("%v:%v:%v:%v", a.Variant, a.DataType, a.Location, a.Name)

	if len(representation) > 20 {
		return representation[:20]
	}

	return representation
}

// String representation of an three-address code operation.
func (o Operation) String() string {
	return operationNames[o]
}

// String representation of a three-address code quadruple.
func (q *Quadruple) String() string {
	return fmt.Sprintf("%-12v %-20v %-20v %-20v", q.Operation, q.Arg1, q.Arg2, q.Result)
}

// String representation of an intermediate code instruction.
func (i *Instruction) String() string {
	var depthDifference any = i.DepthDifference

	if i.DepthDifference == UnusedDifference {
		depthDifference = ""
	}

	return fmt.Sprintf(
		"%-8v %4v    %-12v    %-20v    %-20v    %-20v",
		i.Label,
		depthDifference,
		i.Code.Operation,
		i.Code.Arg1,
		i.Code.Arg2,
		i.Code.Result)
}

// Parse a three-address code address into a value based on its variant and data type.
func (a *Address) Parse() any {
	switch a.Variant {
	case Literal:
		switch a.DataType {
		case Integer64:
			if arg, err := strconv.ParseInt(a.Name, 10, integerBitSize); err != nil {
				panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, intermediateCodeAddressParsingError, a, err))
			} else {
				return arg
			}

		default:
			panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unsupportedDataTypeInIntermediateCodeAddress, a, nil))
		}

	case Variable:
		switch a.DataType {
		case Integer64:
			return a.Location

		default:
			panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unsupportedDataTypeInIntermediateCodeAddress, a, nil))
		}

	case Temporary:
		switch a.DataType {
		case Integer64:
			return nil

		default:
			panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unsupportedDataTypeInIntermediateCodeAddress, a, nil))
		}

	case Label:
		switch a.DataType {
		case String:
			return a.Name

		default:
			panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unsupportedDataTypeInIntermediateCodeAddress, a, nil))
		}

	case Count:
		switch a.DataType {
		case UnsignedInteger64:
			if arg, err := strconv.ParseUint(a.Name, 10, integerBitSize); err != nil {
				panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, intermediateCodeAddressParsingError, a, err))
			} else {
				return arg
			}

		default:
			panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unsupportedDataTypeInIntermediateCodeAddress, a, nil))
		}

	case Code:
		switch a.DataType {
		case Integer64:
			if arg, err := strconv.ParseInt(a.Name, 10, integerBitSize); err != nil {
				panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, intermediateCodeAddressParsingError, a, err))
			} else {
				return arg
			}

		default:
			panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unsupportedDataTypeInIntermediateCodeAddress, a, nil))
		}

	default:
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unexceptedVariantInIntermediateCodeAddress, a, nil))
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

	panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, invalidAddressesContract, q, nil))
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

// Get an instruction iterator for the module.
func (m *module) GetIterator() Iterator {
	return &iterator{current: m.Instructions.Front(), instructions: m.Instructions}
}

// Marshal the module to a JSON object.
func (m *module) MarshalJSON() ([]byte, error) {
	type Embedded module
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

// Unmarshal the module from a JSON object.
func (m *module) UnmarshalJSON(raw []byte) error {
	type Embedded module

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
		m.append(&i)
	}

	return nil
}

// Print the module to the specified writer.
func (m *module) Print(print io.Writer, args ...any) error {
	// enumerate all instructions in the module and print them to the writer
	for e := m.Instructions.Front(); e != nil; e = e.Next() {
		if _, err := fmt.Fprintf(print, "%v\n", e.Value); err != nil {
			return cor.NewGeneralError(cor.Intermediate, failureMap, cor.Error, intermediateCodeExportFailed, nil, err)
		}
	}

	return nil
}

// Export the module to the specified writer in the specified format.
func (m *module) Export(format cor.ExportFormat, print io.Writer) error {
	switch format {
	case cor.Json:
		// export the module as a JSON object
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
		// print is a convenience function to export the module as a string to the print writer
		return m.Print(print)

	default:
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unknownExportFormat, format, nil))
	}
}

// Insert a symbol into the intermediate code symbol table. If the symbol already exists, it will be overwritten.
func (m *module) insert(symbol *symbol) {
	if m.lookup(symbol.name) == nil {
		m.names = append(m.names, symbol.name)
	}

	m.symbolTable[symbol.name] = symbol
}

// Lookup a symbol in the the intermediate code symbol table. If the symbol is not found, nil is returned.
func (m *module) lookup(name string) *symbol {
	if symbol, ok := m.symbolTable[name]; ok {
		return symbol
	}

	return nil
}

// Append an instruction to the intermediate code.
func (m *module) append(instruction *Instruction) *list.Element {
	return m.Instructions.PushBack(instruction)
}

// Configure abstract syntax extensions and fill the symbol table of the module.
func configureSymbols(node ast.Node, code any) {
	module := code.(*intermediateCode).module

	switch n := node.(type) {
	case *ast.BlockNode:
		n.Scope.Extension[scopeExtension] = newScopeMetaData()

	case *ast.ConstantDeclarationNode:
		name := n.Scope.NewIdentifier(prefix[ConstantPrefix])
		n.Scope.LookupCurrent(n.Name).Extension[symbolExtension] = newSymbolMetaData(name)
		module.insert(newSymbol(name, constant, dataTypeMap[n.DataType]))

	case *ast.VariableDeclarationNode:
		name := n.Scope.NewIdentifier(prefix[VariablePrefix])
		n.Scope.LookupCurrent(n.Name).Extension[symbolExtension] = newSymbolMetaData(name)
		module.insert(newSymbol(name, variable, dataTypeMap[n.DataType]))

	case *ast.ProcedureDeclarationNode:
		name := n.Block.(*ast.BlockNode).Scope.NewIdentifier(prefix[FunctionPrefix])
		n.Scope.LookupCurrent(n.Name).Extension[symbolExtension] = newSymbolMetaData(name)
		module.insert(newSymbol(name, function, Void))
	}
}

// Generate intermediate code for the abstract syntax tree.
// The generator itself is performing a top down, left to right, and leftmost derivation walk on the abstract syntax tree.
func (i *intermediateCode) Generate() {
	// pre-create symbol table for intermediate code
	ast.Walk(i.abstractSyntax, ast.PreOrder, i, configureSymbols)

	// generate intermediate code for the abstract syntax tree
	i.abstractSyntax.Accept(i)
}

// Get the generated intermediate code module.
func (i *intermediateCode) GetModule() Module {
	return i.module
}

// Append an instruction to the intermediate code module.
func (i *intermediateCode) AppendInstruction(instruction *Instruction) *list.Element {
	return i.module.append(instruction)
}

// Create a new instruction for the intermediate code.
func (i *intermediateCode) NewInstruction(operatiom Operation, arg1, arg2, result *Address, options ...any) *Instruction {
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
			panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unknownInstructionOption, option, nil))
		}
	}

	return instruction
}

// Generate code for a block, all nested procedure blocks, and its statement.
func (i *intermediateCode) VisitBlock(bn *ast.BlockNode) {
	// only main block has no parent procedure declaration
	if bn.ParentNode == nil {
		blockBegin := bn.Scope.NewIdentifier(prefix[FunctionPrefix])

		// append a target instruction with a branch-label to mark the beginning of the block
		instruction := i.NewInstruction(
			Target, // target for any branching operation
			NewAddress(entryPointDisplayName, Diagnostic, Void, 0), // for diagnostic purposes only
			noAddress,
			noAddress,
			blockBegin) // branch-label as target for any branching operation

		i.AppendInstruction(instruction)
	} else {
		astSymbol := bn.Scope.Lookup(bn.ParentNode.(*ast.ProcedureDeclarationNode).Name)
		blockBegin := astSymbol.Extension[symbolExtension].(*symbolMetaData).name

		// append a target instruction with a branch-label to mark the beginning of the block
		instruction := i.NewInstruction(
			Target, // target for any branching operation
			NewAddress(astSymbol.Name, Diagnostic, Void, 0), // for diagnostic purposes only
			noAddress,
			noAddress,
			blockBegin) // branch-label as target for any branching operation

		element := i.AppendInstruction(instruction)

		// update intermediate code function symbol with the instruction that marks the beginning of the block
		codeSymbol := i.module.lookup(blockBegin)
		codeSymbol.definition = element
	}

	// create prelude for the block
	i.AppendInstruction(i.NewInstruction(Prelude, noAddress, noAddress, noAddress))

	// all declarations except blocks of nested procedures
	for _, declaration := range bn.Declarations {
		if declaration.Type() != ast.ProcedureDeclarationType {
			declaration.Accept(i)
		}
	}

	// statement of the block
	bn.Statement.Accept(i)

	// create epilog for the block
	i.AppendInstruction(i.NewInstruction(Epilog, noAddress, noAddress, noAddress))

	// return from the block and mark the end of the block
	i.AppendInstruction(i.NewInstruction(Return, noAddress, noAddress, noAddress))

	// all blocks of nested procedure declarations (makes a procedure declaration a top-level construct in intermediate code)
	for _, declaration := range bn.Declarations {
		if declaration.Type() == ast.ProcedureDeclarationType {
			declaration.Accept(i)
		}
	}
}

// Generate code for a constant declaration.
func (i *intermediateCode) VisitConstantDeclaration(declaration *ast.ConstantDeclarationNode) {
	// not required for code generation
}

// Generate code for a variable declaration.
func (i *intermediateCode) VisitVariableDeclaration(vd *ast.VariableDeclarationNode) {
	// access intermediate code metadata from abstract syntax scope
	scopeMetaData := vd.Scope.Extension[scopeExtension].(*scopeMetaData)

	// determine the intermediate code name of the abstract syntax variable declaration
	codeName := vd.Scope.LookupCurrent(vd.Name).Extension[symbolExtension].(*symbolMetaData).name

	// get the intermediate code symbol table entry of the abstract syntax variable declaration
	codeSymbol := i.module.lookup(codeName)

	// set the location of the variable in its logical memory space
	codeSymbol.location = scopeMetaData.counter
	scopeMetaData.counter++

	// allocate memory for the variable in its logical memory space
	instruction := i.NewInstruction(
		Allocate, // allocate memory based on a location
		NewAddress(vd.Name, Diagnostic, codeSymbol.dataType, codeSymbol.location), // for diagnostic purposes only
		noAddress,
		NewAddress(codeSymbol.name, Variable, codeSymbol.dataType, codeSymbol.location), // location for the variable
		vd.TokenStreamIndex) // variable declaration in the token stream

	// append allocate instruction to the module and set it as definition for the intermediate code variable
	codeSymbol.definition = i.AppendInstruction(instruction)
}

// Generate code for a procedure declaration.
func (i *intermediateCode) VisitProcedureDeclaration(pd *ast.ProcedureDeclarationNode) {
	// generate code for the block of the procedure
	pd.Block.Accept(i)
}

// Generate code for a literal.
func (i *intermediateCode) VisitLiteral(ln *ast.LiteralNode) {
	// create a value copy instruction to store the literal in an temporary result
	instruction := i.NewInstruction(
		ValueCopy, // copy the value of the literal to a temporary result
		NewAddress(ln.Value, Literal, dataTypeMap[ln.DataType], 0), // literal value
		noAddress,
		NewAddress(ln.Scope.NewIdentifier(prefix[ResultPrefix]), Temporary, dataTypeMap[ln.DataType], 0), // temporary result
		ln.TokenStreamIndex) // literal use in the token stream

	// push the temporary result onto the results-list and append the instruction to the module
	i.pushResult(instruction.Code.Result)
	i.AppendInstruction(instruction)
}

// Generate code for an identifier use.
func (i *intermediateCode) VisitIdentifierUse(iu *ast.IdentifierUseNode) {
	switch iu.Context {
	case ast.Constant:
		// get constant declaration of the constant to load
		constantDeclaration := iu.Scope.Lookup(iu.Name).Declaration.(*ast.ConstantDeclarationNode)

		// determine the intermediate code name of the abstract syntax constant declaration
		codeName := iu.Scope.Lookup(iu.Name).Extension[symbolExtension].(*symbolMetaData).name

		// get the intermediate code symbol table entry of the abstract syntax constant declaration
		codeSymbol := i.module.lookup(codeName)

		// create a value copy instruction to store the constant value in an temporary result
		instruction := i.NewInstruction(
			ValueCopy, // copy the value of the constant to a temporary result
			NewAddress(constantDeclaration.Value, Literal, dataTypeMap[constantDeclaration.DataType], 0), // literal value
			noAddress,
			NewAddress(iu.Scope.NewIdentifier(prefix[ResultPrefix]), Temporary, codeSymbol.dataType, 0), // temporary result
			iu.TokenStreamIndex) // constant use in the token stream

		// push the temporary result onto the results-list and append the instruction to the module
		i.pushResult(instruction.Code.Result)
		i.AppendInstruction(instruction)

	case ast.Variable:
		// get variable declaration of the variable to load
		variableDeclaration := iu.Scope.Lookup(iu.Name).Declaration.(*ast.VariableDeclarationNode)

		// determine the block nesting depth of the variable declaration
		declarationDepth := ast.SearchBlock(ast.CurrentBlock, variableDeclaration).Depth

		// determine the block nesting depth of the variable use from inside an expression or statement
		useDepth := ast.SearchBlock(ast.CurrentBlock, iu).Depth

		// determine the intermediate code name of the abstract syntax variable declaration
		codeName := iu.Scope.Lookup(iu.Name).Extension[symbolExtension].(*symbolMetaData).name

		// get the intermediate code symbol table entry of the abstract syntax variable declaration
		codeSymbol := i.module.lookup(codeName)

		// create a variable load instruction to load the variable value into a temporary result
		instruction := i.NewInstruction(
			VariableLoad, // load the value of the variable from its location into a temporary result
			NewAddress(codeSymbol.name, Variable, codeSymbol.dataType, codeSymbol.location), // variable location
			noAddress,
			NewAddress(iu.Scope.NewIdentifier(prefix[ResultPrefix]), Temporary, codeSymbol.dataType, 0), // temporary result
			useDepth-declarationDepth, // block nesting depth difference between variable use and variable declaration
			iu.TokenStreamIndex)       // variable use in the token stream

		// push the temporary result onto the results-list and append the instruction to the module
		i.pushResult(instruction.Code.Result)
		i.AppendInstruction(instruction)

	case ast.Procedure:
		// not required for code generation

	default:
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, invalidContextInIdentifierUse, nil, nil))
	}
}

// Generate code for a unary operation.
func (i *intermediateCode) VisitUnaryOperation(uo *ast.UnaryOperationNode) {
	// load the temporary result of the expression from the results-list
	uo.Operand.Accept(i)
	result := i.popResult()

	// perform the unary operation on the temporary result
	switch uo.Operation {
	case ast.Odd:
		// create an odd instruction to check if the temporary result is odd
		instruction := i.NewInstruction(
			Odd,
			result, // consumed temporary result
			noAddress,
			noAddress,           // consumed temporary result is checked in-place, boolean result must be hold externally
			uo.TokenStreamIndex) // unary operation in the token stream

		// append the instruction to the module (boolean results are not stored on the results-list)
		i.AppendInstruction(instruction)

	case ast.Negate:
		// create a negate instruction to negate the temporary result
		instruction := i.NewInstruction(
			Negate,
			result, // consumed temporary result
			noAddress,
			result,              // consumed temporary result is negated in-place (read, negate, write back negated result)
			uo.TokenStreamIndex) // unary operation in the token stream

		// push the temporary result onto the results-list and append the instruction to the module
		i.pushResult(instruction.Code.Result)
		i.AppendInstruction(instruction)

	default:
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unknownUnaryOperation, nil, nil))
	}
}

// Generate code for a binary arithmetic operation.
func (i *intermediateCode) VisitBinaryOperation(bo *ast.BinaryOperationNode) {
	// determine block and its scope where the binary operation is located
	scope := ast.SearchBlock(ast.CurrentBlock, bo).Scope

	// load the temporary results of the left and right expressions from the results-list
	bo.Left.Accept(i)
	bo.Right.Accept(i)
	right := i.popResult()
	left := i.popResult()

	// perform the binary arithmetic operation on the left- and right-hand-side temporary results
	switch bo.Operation {
	case ast.Plus, ast.Minus, ast.Times, ast.Divide:
		var operation Operation

		// map the AST binary operation to the corresponding three-address code binary arithmetic operation
		switch bo.Operation {
		case ast.Plus:
			operation = Plus

		case ast.Minus:
			operation = Minus

		case ast.Times:
			operation = Times

		case ast.Divide:
			operation = Divide
		}

		// create a binary arithmetic operation instruction to perform the operation on the left- and right-hand-side results
		instruction := i.NewInstruction(
			operation,
			left,  // consumed left-hand-side temporary result
			right, // consumed right-hand-side temporary result
			NewAddress(scope.NewIdentifier(prefix[ResultPrefix]), Temporary, left.DataType, 0), // arithmetic operation result
			bo.TokenStreamIndex) // arithmetic operation in the token stream

		// push the temporary result result onto the results-list and append the instruction to the module
		i.pushResult(instruction.Code.Result)
		i.AppendInstruction(instruction)

	default:
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unknownBinaryOperation, nil, nil))
	}
}

// Generate code for a binary relational operation.
func (i *intermediateCode) VisitConditionalOperation(co *ast.ConditionalOperationNode) {
	// load the temporary results of the left and right expressions from the results-list
	co.Left.Accept(i)
	co.Right.Accept(i)
	right := i.popResult()
	left := i.popResult()

	// perform the binary relational operation on the left- and right-hand-side temporary results
	switch co.Operation {
	case ast.Equal, ast.NotEqual, ast.Less, ast.LessEqual, ast.Greater, ast.GreaterEqual:
		var operation Operation

		// map the AST binary operation to the corresponding three-address code binary relational operation
		switch co.Operation {
		case ast.Equal:
			operation = Equal

		case ast.NotEqual:
			operation = NotEqual

		case ast.Less:
			operation = Less

		case ast.LessEqual:
			operation = LessEqual

		case ast.Greater:
			operation = Greater

		case ast.GreaterEqual:
			operation = GreaterEqual
		}

		// create a binary relational operation instruction to perform the operation on the left- and right-hand-side results
		instruction := i.NewInstruction(
			operation,
			left,                // consumed left-hand-side temporary result
			right,               // consumed right-hand-side temporary result
			noAddress,           // consumed temporary results are checked in-place, boolean result must be hold externally
			co.TokenStreamIndex) // conditional operation in the token stream

		// append the instruction to the module (boolean results are not stored on the results-list)
		i.AppendInstruction(instruction)

	default:
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unknownConditionalOperation, nil, nil))
	}
}

// Generate code for an assignment statement.
func (i *intermediateCode) VisitAssignmentStatement(s *ast.AssignmentStatementNode) {
	// load the value from the temporary result of the right-hand-side expression of the assignment
	s.Expression.Accept(i)
	right := i.popResult()

	// get the variable declaration on the left-hand-side of the assignment
	variableUse := s.Variable.(*ast.IdentifierUseNode)
	variableDeclaration := variableUse.Scope.Lookup(variableUse.Name).Declaration.(*ast.VariableDeclarationNode)

	// determine the block nesting depth of the variable declaration
	declarationDepth := ast.SearchBlock(ast.CurrentBlock, variableDeclaration).Depth

	// determine the block nesting depth of the assignment statement where the variable is used
	assignmentDepth := ast.SearchBlock(ast.CurrentBlock, s).Depth

	// determine the intermediate code name of the abstract syntax variable declaration
	codeName := variableUse.Scope.Lookup(variableUse.Name).Extension[symbolExtension].(*symbolMetaData).name

	// get the intermediate code symbol table entry of the abstract syntax variable declaration
	codeSymbol := i.module.lookup(codeName)

	// store the resultant value from the right-hand-side expression in the variable on the left-hand-side of the assignment
	instruction := i.NewInstruction(
		VariableStore, // store the value of the temporary result into the location of a variable
		right,         // consumed right-hand-side temporary result
		noAddress,
		NewAddress(codeSymbol.name, Variable, codeSymbol.dataType, codeSymbol.location),
		assignmentDepth-declarationDepth, // block nesting depth difference between variable use and variable declaration
		s.TokenStreamIndex)               // assignment statement in the token stream

	// append the instruction to the module
	i.AppendInstruction(instruction)
}

// Generate code for a read statement.
func (i *intermediateCode) VisitReadStatement(s *ast.ReadStatementNode) {
	// determine block and its scope where the read statement is located
	scope := ast.SearchBlock(ast.CurrentBlock, s).Scope

	// get the variable declaration of the variable to read into
	variableUse := s.Variable.(*ast.IdentifierUseNode)
	variableDeclaration := variableUse.Scope.Lookup(variableUse.Name).Declaration.(*ast.VariableDeclarationNode)

	// determine the block nesting depth of the variable declaration
	declarationDepth := ast.SearchBlock(ast.CurrentBlock, variableDeclaration).Depth

	// determine the block nesting depth of the read statement where the variable is used
	readDepth := ast.SearchBlock(ast.CurrentBlock, s).Depth

	// determine the intermediate code name of the abstract syntax variable declaration
	codeName := variableUse.Scope.Lookup(variableUse.Name).Extension[symbolExtension].(*symbolMetaData).name

	// get the intermediate code symbol table entry of the abstract syntax variable declaration
	codeSymbol := i.module.lookup(codeName)

	// create a variable load instruction to load the variable value into a temporary result
	load := i.NewInstruction(
		VariableLoad, // load the value of the variable from its location into a temporary result
		NewAddress(codeSymbol.name, Variable, codeSymbol.dataType, codeSymbol.location), // variable location
		noAddress,
		NewAddress(scope.NewIdentifier(prefix[ResultPrefix]), Temporary, codeSymbol.dataType, 0), // temporary result
		readDepth-declarationDepth, // block nesting depth difference between variable use and variable declaration
		s.TokenStreamIndex)         // read statement in the token stream

	// parameter 1 for the readln standard function
	param := i.NewInstruction(
		Parameter,        // parameter for a standard function
		load.Code.Result, // temporary result will be replaced by the standard function resultant value
		noAddress,
		noAddress,
		s.TokenStreamIndex) // read statement in the token stream

	// call the readln standard function with 1 parameter
	readln := i.NewInstruction(
		Standard, // function call to the external standard library
		NewAddress(1, Count, UnsignedInteger64, 0),     // number of parameters for the standard function
		NewAddress(ReadLn, Code, UnsignedInteger64, 0), // code of standard function to call
		noAddress,
		s.TokenStreamIndex) // read statement in the token stream

	// get the intermediate code symbol table entry of the abstract syntax variable declaration
	codeSymbol = i.module.lookup(codeName)

	// store the resultant value into the variable used by the read statement
	store := i.NewInstruction(
		VariableStore,   // store the value of the standard function result into the location of a variable
		param.Code.Arg1, // standard function resultant value
		noAddress,
		NewAddress(codeSymbol.name, Variable, codeSymbol.dataType, codeSymbol.location), // variable location
		readDepth-declarationDepth, // block nesting depth difference between variable use and variable declaration
		s.TokenStreamIndex)         // read statement in the token stream

	// append the instructions to the module
	i.AppendInstruction(load)
	i.AppendInstruction(param)
	i.AppendInstruction(readln)
	i.AppendInstruction(store)
}

// Generate code for a write statement.
func (i *intermediateCode) VisitWriteStatement(s *ast.WriteStatementNode) {
	// load the value from the result of the expression on the right-hand-side of the write statement
	s.Expression.Accept(i)
	right := i.popResult()

	// parameter 1 for the writeln standard function
	param := i.NewInstruction(
		Parameter, // parameter for a standard function
		right,     // consumed right-hand-side temporary result
		noAddress,
		noAddress,
		s.TokenStreamIndex) // write statement in the token stream

	// call the writeln standard function with 1 parameter
	writeln := i.NewInstruction(
		Standard, // function call to the external standard library
		NewAddress(1, Count, UnsignedInteger64, 0),      // number of parameters for the standard function
		NewAddress(WriteLn, Code, UnsignedInteger64, 0), // code of standard function to call
		noAddress,
		s.TokenStreamIndex) // write statement in the token stream

	// append the instructions to the module
	i.AppendInstruction(param)
	i.AppendInstruction(writeln)
}

// Generate code for a call statement.
func (i *intermediateCode) VisitCallStatement(s *ast.CallStatementNode) {
	// get the declaration of the procedure to call
	procedureUse := s.Procedure.(*ast.IdentifierUseNode)
	procedureDeclaration := procedureUse.Scope.Lookup(procedureUse.Name).Declaration.(*ast.ProcedureDeclarationNode)

	// determine the block nesting depth of the procedure declaration
	declarationDepth := ast.SearchBlock(ast.CurrentBlock, procedureDeclaration).Depth

	// determine the block nesting depth of the call statement where the procedure is called
	callDepth := ast.SearchBlock(ast.CurrentBlock, s).Depth

	// determine the intermediate code name of the abstract syntax procedure declaration
	codeName := procedureUse.Scope.Lookup(procedureUse.Name).Extension[symbolExtension].(*symbolMetaData).name

	// call the intermediate code function with 0 parameters
	call := i.NewInstruction(
		Call, // call to an intermediate code function
		NewAddress(0, Count, UnsignedInteger64, 0), // number of parameters for the function
		NewAddress(codeName, Label, String, 0),     // label of intermediate code function to call
		noAddress,
		callDepth-declarationDepth, // block nesting depth difference between procedure call and procedure declaration
		s.TokenStreamIndex)         // call statement in the token stream

	// append the instruction to the module
	i.AppendInstruction(call)
}

// Generate code for an if-then statement.
func (i *intermediateCode) VisitIfStatement(s *ast.IfStatementNode) {
	// determine block and its scope where the if-then statement is located
	scope := ast.SearchBlock(ast.CurrentBlock, s).Scope
	behindStatement := scope.NewIdentifier(prefix[LabelPrefix])

	// calculate the result of the condition expression
	s.Condition.Accept(i)

	// jump behind the statement if the condition is false
	i.jumpConditional(s.Condition, false, behindStatement)

	// execute statement if the condition is true
	s.Statement.Accept(i)

	// append a target instruction behind the statement instructions
	i.AppendInstruction(i.NewInstruction(Target, noAddress, noAddress, noAddress, behindStatement, s.TokenStreamIndex))
}

// Generate code for a while-do statement.
func (i *intermediateCode) VisitWhileStatement(s *ast.WhileStatementNode) {
	// determine block and its scope where the while-do statement is located
	scope := ast.SearchBlock(ast.CurrentBlock, s).Scope
	beforeCondition := scope.NewIdentifier(prefix[LabelPrefix])
	behindStatement := scope.NewIdentifier(prefix[LabelPrefix])

	// append a target instruction before the conditional expression instructions
	i.AppendInstruction(i.NewInstruction(Target, noAddress, noAddress, noAddress, beforeCondition, s.TokenStreamIndex))

	// calculate the result of the conditional expression
	s.Condition.Accept(i)

	// jump behind the statement if the condition is false
	i.jumpConditional(s.Condition, false, behindStatement)

	// execute statement if the condition is true
	s.Statement.Accept(i)

	// append a jump instruction to jump back to the conditional expression instructions
	beforeConditionAddress := NewAddress(beforeCondition, Label, String, 0)
	i.AppendInstruction(i.NewInstruction(Jump, beforeConditionAddress, noAddress, noAddress, s.TokenStreamIndex))

	// append a target instruction behind the statement instructions
	i.AppendInstruction(i.NewInstruction(Target, noAddress, noAddress, noAddress, behindStatement, s.TokenStreamIndex))
}

// Generate code for a compound begin-end statement.
func (i *intermediateCode) VisitCompoundStatement(s *ast.CompoundStatementNode) {
	// generate code for all statements in the compound statement
	for _, statement := range s.Statements {
		statement.Accept(i)
	}
}

// Conditional jump instruction based on an expression that must be a unary or conditional operation node.
func (i *intermediateCode) jumpConditional(expression ast.Expression, jumpIfCondition bool, label string) {
	var jump *Instruction
	address := NewAddress(label, Label, String, 0)

	// odd operation or conditional operations are valid for conditional jumps
	switch condition := expression.(type) {
	// unary operation node with the odd operation
	case *ast.UnaryOperationNode:
		if condition.Operation == ast.Odd {
			if jumpIfCondition {
				jump = i.NewInstruction(JumpNotEqual, address, noAddress, noAddress, condition.TokenStreamIndex)
			} else {
				jump = i.NewInstruction(JumpEqual, address, noAddress, noAddress, condition.TokenStreamIndex)
			}
		} else {
			panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unknownUnaryOperation, nil, nil))
		}

	// conditional operation node with the equal, not equal, less, less equal, greater, or greater equal operation
	case *ast.ConditionalOperationNode:
		if jumpIfCondition {
			// jump if the condition is true
			switch condition.Operation {
			case ast.Equal:
				jump = i.NewInstruction(JumpEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.NotEqual:
				jump = i.NewInstruction(JumpNotEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.Less:
				jump = i.NewInstruction(JumpLess, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.LessEqual:
				jump = i.NewInstruction(JumpLessEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.Greater:
				jump = i.NewInstruction(JumpGreater, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.GreaterEqual:
				jump = i.NewInstruction(JumpGreaterEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			default:
				panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unknownConditionalOperation, nil, nil))
			}
		} else {
			// jump if the condition is false
			switch condition.Operation {
			case ast.Equal:
				jump = i.NewInstruction(JumpNotEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.NotEqual:
				jump = i.NewInstruction(JumpEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.Less:
				jump = i.NewInstruction(JumpGreaterEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.LessEqual:
				jump = i.NewInstruction(JumpGreater, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.Greater:
				jump = i.NewInstruction(JumpLessEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.GreaterEqual:
				jump = i.NewInstruction(JumpLess, address, noAddress, noAddress, condition.TokenStreamIndex)

			default:
				panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unknownConditionalOperation, nil, nil))
			}
		}

	default:
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unknownConditionalOperation, nil, nil))
	}

	// append the conditional jump instruction to the module
	i.AppendInstruction(jump)
}

// Push a result onto the results-list of temporary results.
func (i *intermediateCode) pushResult(result *Address) {
	i.results.PushBack(result)
}

// Pop a result from the results-list of temporary results.
func (i *intermediateCode) popResult() *Address {
	result := i.results.Back()

	if result == nil {
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unexpectedIntermediateCodeResult, nil, nil))
	}

	i.results.Remove(result)
	return result.Value.(*Address)
}
