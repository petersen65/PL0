// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package code

import (
	"container/list"
	"encoding/json"
	"fmt"
	"io"
	"strings"

	"github.com/google/uuid"
	ast "github.com/petersen65/PL0/v2/ast"
	cor "github.com/petersen65/PL0/v2/core"
)

// Display name of entry point only used for informational purposes
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
	// Intermediate code generation compiler pass. It implements the Visitor interface to traverse the AST and generate code.
	intermediateCode struct {
		abstractSyntax ast.Block  // abstract syntax tree to generate code for
		module         *module    // module to store the generated intermediate code
		results        *list.List // lifo stack holding intermediate code results from expressions
	}

	// Module represents a logical unit of instructions created from one source file so that a program can be linked together from multiple modules.
	module struct {
		UniqueId     string             `json:"unique_id"`    // unique identifier of the module
		targets      []string           `json:"-"`            // enable deterministic iteration over the symbol table in the order of past inserts
		symbolTable  map[string]*symbol `json:"-"`            // symbol table for intermediate code targets
		Instructions *list.List         `json:"instructions"` // intermediate code instructions as doubly linked list that allows reordering
	}

	// Metadata for each scope in the abstract syntax tree.
	scopeMetaData struct {
		offsetCounter uint64 // offset counter for all variables of a scope
	}

	// Metadata for each symbol in the abstract syntax tree.
	symbolMetaData struct {
		target string // target name in the intermediate code
	}

	// Kind of symbol entries.
	entry int

	// Symbol represents a symbol in the intermediate code that maps its target to where it was defined.
	// A target is the structured name of any abstract syntax identifier mapped to the intermediate code.
	// A target is always relativ to a scope where it is defined and is unique accross all definitions within a module.
	symbol struct {
		target     string        // target name in the intermediate code
		kind       entry         // kind of symbol entry
		dataType   DataType      // data type of the symbol
		offset     uint64        // variable offset in the logical memory space
		definition *list.Element // instruction where the symbol is defined
	}

	// Navigation implementation for the module's intermediate code instructions.
	iterator struct {
		current      *list.Element
		instructions *list.List
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

// Create a new intermediate code module and initialize it with a unique identifier based on a UUID.
func newModule() Module {
	return &module{
		UniqueId:     uuid.NewString(),
		targets:      make([]string, 0),
		symbolTable:  make(map[string]*symbol),
		Instructions: list.New(),
	}
}

// create new symbol for the intermediate code
func newSymbol(target string, kind entry, dataType DataType) *symbol {
	return &symbol{target: target, kind: kind, dataType: dataType}
}

// Create metadata for a scope in the abstract syntax tree.
func newScopeMetaData() *scopeMetaData {
	return &scopeMetaData{offsetCounter: 1}
}

// Create metadata for a symbol in the abstract syntax tree.
func newSymbolMetaData(target string) *symbolMetaData {
	return &symbolMetaData{target: target}
}

// String representation of a data type.
func (dt DataType) String() string {
	return DataTypeNames[dt]
}

// Get a data type from its representation.
func (dtr DataTypeRepresentation) DataType() DataType {
	for dataType, representation := range DataTypeNames {
		if representation == string(dtr) {
			return dataType
		}
	}

	panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unknownDataTypeRepresentation, dtr, nil))
}

// String representation of the three-address code address.
func (a *Address) String() string {
	representation := fmt.Sprintf("%v:%v:%v", a.DataType, a.Offset, a.Variable)

	if len(representation) > 20 {
		return representation[:20]
	}

	return representation
}

// String representation of an intermediate code operation.
func (o Operation) String() string {
	return OperationNames[o]
}

// String representation of an intermediate code instruction.
func (i *Instruction) String() string {
	return fmt.Sprintf(
		"%-8v %4v    %-12v    %-20v    %-20v    %-20v",
		i.Label,
		i.DepthDifference,
		i.Code.Operation,
		i.Code.Arg1,
		i.Code.Arg2,
		i.Code.Result)
}

// Update symbol metadata in the abstract syntax tree.
func (s *symbolMetaData) update(newTargetVersion string) {
	targetParts := strings.Split(s.target, ".")
	newTargetVersionParts := strings.Split(newTargetVersion, ".")

	if s.target == newTargetVersion ||
		len(targetParts) != 2 || len(newTargetVersionParts) != 2 || targetParts[0] != newTargetVersionParts[0] {
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, symbolMetaDataUpdateFailed, s.target, nil))
	}

	s.target = newTargetVersion
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

// Insert a symbol into the symbol table of the module. If the symbol already exists, it will be overwritten.
func (m *module) insert(symbol *symbol) {
	if m.lookup(symbol.target) == nil {
		m.targets = append(m.targets, symbol.target)
	}

	m.symbolTable[symbol.target] = symbol
}

// Lookup a symbol in the symbol table of the module. If the symbol is not found, nil is returned.
func (m *module) lookup(target string) *symbol {
	if symbol, ok := m.symbolTable[target]; ok {
		return symbol
	}

	return nil
}

// Update symbol table from target with new version from that target
func (m *module) update(target, newTargetVersion string) {
	var found bool

	for i := 0; i < len(m.targets); i++ {
		if m.targets[i] == target {
			m.targets[i] = newTargetVersion
			found = true
			break
		}
	}

	if target == newTargetVersion || !found {
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, symbolTableUpdateFailed, target, nil))
	}

	targetParts := strings.Split(target, ".")
	newTargetVersionParts := strings.Split(newTargetVersion, ".")

	if len(targetParts) != 2 || len(newTargetVersionParts) != 2 || targetParts[0] != newTargetVersionParts[0] {
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, symbolTableUpdateFailed, target, nil))
	}

	m.symbolTable[newTargetVersion] = m.symbolTable[target]
	m.symbolTable[newTargetVersion].target = newTargetVersion
	delete(m.symbolTable, target)
}

// Append an instruction to the intermediate code.
func (m *module) AppendInstruction(instruction *Instruction) *list.Element {
	return m.Instructions.PushBack(instruction)
}

// Iterate over all instructions in the module.
func (m *module) IterateInstruction() <-chan *Instruction {
	instructions := make(chan *Instruction)

	go func() {
		for e := m.Instructions.Front(); e != nil; e = e.Next() {
			instructions <- e.Value.(*Instruction)
		}

		close(instructions)
	}()

	return instructions
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
		Embedded
		Instructions []Instruction `json:"instructions"`
	}{
		Embedded:     (Embedded)(*m),
		Instructions: instructions,
	}

	return json.Marshal(mj)
}

// Unmarshal the module from a JSON object.
func (m *module) UnmarshalJSON(raw []byte) error {
	type Embedded module

	// target struct to unmarshal the JSON object to
	mj := &struct {
		Embedded
		Instructions []Instruction `json:"instructions"`
	}{
		Embedded: (Embedded)(*m),
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

// Print the module to the specified writer.
func (m *module) Print(print io.Writer, args ...any) error {
	// enumerate all instructions in the module and print them to the writer
	for e := m.Instructions.Front(); e != nil; e = e.Next() {
		_, err := fmt.Fprintf(print, "%v\n", e.Value)

		if err != nil {
			return cor.NewGeneralError(cor.Intermediate, failureMap, cor.Error, intermediateCodeExportFailed, nil, err)
		}
	}

	return nil
}

// Export the module of the intermediate code generator.
func (m *module) Export(format cor.ExportFormat, print io.Writer) error {
	switch format {
	case cor.Json:
		// export the module as a JSON object and wrap it in a struct to provide a field name for the module
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

// Generate intermediate code for the abstract syntax tree.
// The generator itself is performing a top down, left to right, and leftmost derivation walk on the abstract syntax tree.
func (i *intermediateCode) Generate() {
	// pre-create symbol table for intermediate code
	ast.Walk(i.abstractSyntax, ast.PreOrder, i, configureSymbols)

	// generate intermediate code for the abstract syntax tree
	i.abstractSyntax.Accept(i)
}

// Configure abstract syntax extensions and fill the symbol table of the intermediate code
func configureSymbols(node ast.Node, code any) {
	module := code.(*intermediateCode).module

	switch n := node.(type) {
	case *ast.BlockNode:
		n.Scope.Extension[scopeExtension] = newScopeMetaData()

	case *ast.ConstantDeclarationNode:
		target := n.Scope.NewIdentifier(Prefix[ConstantPrefix])
		n.Scope.LookupCurrent(n.Name).Extension[symbolExtension] = newSymbolMetaData(target)
		module.insert(newSymbol(target, constant, DataTypeMap[n.DataType]))

	case *ast.VariableDeclarationNode:
		target := n.Scope.NewIdentifier(Prefix[VariablePrefix])
		n.Scope.LookupCurrent(n.Name).Extension[symbolExtension] = newSymbolMetaData(target)
		module.insert(newSymbol(target, variable, DataTypeMap[n.DataType]))

	case *ast.ProcedureDeclarationNode:
		target := n.Block.(*ast.BlockNode).Scope.NewIdentifier(Prefix[FunctionPrefix])
		n.Scope.LookupCurrent(n.Name).Extension[symbolExtension] = newSymbolMetaData(target)
		module.insert(newSymbol(target, function, Void))
	}
}

// Get the generated intermediate code module.
func (i *intermediateCode) GetModule() Module {
	return i.module
}

// Append an instruction to the intermediate code module.
func (i *intermediateCode) AppendInstruction(instruction *Instruction) *list.Element {
	return i.module.AppendInstruction(instruction)
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
	var blockBegin string

	// only main block has no parent procedure declaration
	if bn.ParentNode == nil {
		blockBegin = bn.Scope.NewIdentifier(Prefix[FunctionPrefix])

		// append a branch instruction with a branch-label to mark the beginning of the block
		instruction := i.NewInstruction(
			Branch,
			NewAddress(Void, 0, entryPointDisplayName),
			NoAddress,
			NewAddress(Void, 0, blockBegin),
			blockBegin)

		i.AppendInstruction(instruction)
	} else {
		astSymbol := bn.Scope.Lookup(bn.ParentNode.(*ast.ProcedureDeclarationNode).Name)
		blockBegin = astSymbol.Extension[symbolExtension].(*symbolMetaData).target

		// append a branch instruction with a branch-label to mark the beginning of the block
		instruction := i.NewInstruction(
			Branch,
			NewAddress(Void, 0, astSymbol.Name),
			NoAddress,
			NewAddress(Void, 0, blockBegin),
			blockBegin)

		element := i.AppendInstruction(instruction)

		// update intermediate code function symbol with the instruction that marks the beginning of the block
		codeSymbol := i.module.lookup(blockBegin)
		codeSymbol.definition = element

	}

	// create prelude for the block
	i.AppendInstruction(i.NewInstruction(Prelude, NoAddress, NoAddress, NoAddress))

	// all declarations except blocks of nested procedures
	for _, declaration := range bn.Declarations {
		if declaration.Type() != ast.ProcedureDeclarationType {
			declaration.Accept(i)
		}
	}

	// statement of the block
	bn.Statement.Accept(i)

	// create epilog for the block
	i.AppendInstruction(i.NewInstruction(Epilog, NoAddress, NoAddress, NoAddress))

	// return from the block and mark the end of the block
	i.AppendInstruction(i.NewInstruction(Return, NoAddress, NoAddress, NoAddress, blockBegin))

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

	// determine the intermediate code target name of the abstract syntax variable declaration
	target := vd.Scope.LookupCurrent(vd.Name).Extension[symbolExtension].(*symbolMetaData).target

	// get the intermediate code symbol table entry of the abstract syntax variable declaration
	codeSymbol := i.module.lookup(target)

	// set the location of the variable in its logical memory space
	codeSymbol.offset = scopeMetaData.offsetCounter
	scopeMetaData.offsetCounter++

	// allocate memory for the variable in its logical memory space
	instruction := i.NewInstruction(
		Allocate,
		NewAddress(codeSymbol.dataType, codeSymbol.offset, vd.Name),
		NoAddress,
		NewAddress(codeSymbol.dataType, codeSymbol.offset, codeSymbol.target),
		vd.TokenStreamIndex)

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
	// create a value copy instruction to store the literal in an intermediate code result
	instruction := i.NewInstruction(
		ValueCopy,
		NewAddress(DataTypeMap[ln.DataType], 0, ln.Value),
		NoAddress,
		NewAddress(DataTypeMap[ln.DataType], 0, ln.Scope.NewIdentifier(Prefix[ResultPrefix])),
		ln.TokenStreamIndex)

	// push the intermediate code result onto the stack and append the instruction to the module
	i.pushResult(instruction.Code.Result)
	i.AppendInstruction(instruction)
}

// Generate code for an identifier use.
func (i *intermediateCode) VisitIdentifierUse(iu *ast.IdentifierUseNode) {
	switch iu.Context {
	case ast.Constant:
		// get constant declaration of the constant to load
		constantDeclaration := iu.Scope.Lookup(iu.Name).Declaration.(*ast.ConstantDeclarationNode)

		// determine the intermediate code target name of the abstract syntax constant declaration
		target := iu.Scope.Lookup(iu.Name).Extension[symbolExtension].(*symbolMetaData).target

		// get the intermediate code symbol table entry of the abstract syntax constant declaration
		codeSymbol := i.module.lookup(target)

		// create a value copy instruction to store the constant value in an intermediate code result
		instruction := i.NewInstruction(
			ValueCopy,
			NewAddress(DataTypeMap[constantDeclaration.DataType], 0, constantDeclaration.Value),
			NoAddress,
			NewAddress(codeSymbol.dataType, 0, iu.Scope.NewIdentifier(Prefix[ResultPrefix])),
			iu.TokenStreamIndex)

		// push the intermediate code result onto the stack and append the instruction to the module
		i.pushResult(instruction.Code.Result)
		i.AppendInstruction(instruction)

	case ast.Variable:
		// get variable declaration of the variable to load
		variableDeclaration := iu.Scope.Lookup(iu.Name).Declaration.(*ast.VariableDeclarationNode)

		// determine the block nesting depth of the variable declaration
		declarationDepth := ast.SearchBlock(ast.CurrentBlock, variableDeclaration).Depth

		// determine the block nesting depth of the variable use from inside an expression or statement
		useDepth := ast.SearchBlock(ast.CurrentBlock, iu).Depth

		// determine the intermediate code target name of the abstract syntax variable declaration
		target := iu.Scope.Lookup(iu.Name).Extension[symbolExtension].(*symbolMetaData).target

		// get the intermediate code symbol table entry of the abstract syntax variable declaration
		codeSymbol := i.module.lookup(target)

		// create a variable load instruction to load the variable value into an intermediate code result
		instruction := i.NewInstruction(
			VariableLoad,
			NewAddress(codeSymbol.dataType, codeSymbol.offset, codeSymbol.target),
			NoAddress,
			NewAddress(codeSymbol.dataType, 0, iu.Scope.NewIdentifier(Prefix[ResultPrefix])),
			useDepth-declarationDepth,
			iu.TokenStreamIndex)

		// push the intermediate code result onto the stack and append the instruction to the module
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
	// load the intermediate code result of the expression from the stack
	uo.Operand.Accept(i)
	result := i.popResult()

	// perform the unary operation on the intermediate code result
	switch uo.Operation {
	case ast.Odd:
		// create an odd instruction to check if the intermediate code result is odd
		instruction := i.NewInstruction(
			Odd,
			result,
			NoAddress,
			NoAddress,
			uo.TokenStreamIndex)

		// append the instruction to the module (boolean results are not stored on the stack)
		i.AppendInstruction(instruction)

	case ast.Negate:
		// create a negate instruction to negate the intermediate code result
		instruction := i.NewInstruction(
			Negate,
			result,
			NoAddress,
			result, // the intermediate code result is negated in-place (read, negate, write back)
			uo.TokenStreamIndex)

		// push the intermediate code result onto the stack and append the instruction to the module
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

	// load the intermediate code results of the left and right expressions from the stack
	bo.Left.Accept(i)
	bo.Right.Accept(i)
	right := i.popResult()
	left := i.popResult()

	// perform the binary arithmetic operation on the left- and right-hand-side intermediate code results
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
			left,
			right,
			NewAddress(left.DataType, 0, scope.NewIdentifier(Prefix[ResultPrefix])),
			bo.TokenStreamIndex)

		// push the intermediate code result result onto the stack and append the instruction to the module
		i.pushResult(instruction.Code.Result)
		i.AppendInstruction(instruction)

	default:
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unknownBinaryOperation, nil, nil))
	}
}

// Generate code for a binary relational operation.
func (i *intermediateCode) VisitConditionalOperation(co *ast.ConditionalOperationNode) {
	// load the intermediate code results of the left and right expressions from the stack
	co.Left.Accept(i)
	co.Right.Accept(i)
	right := i.popResult()
	left := i.popResult()

	// perform the binary relational operation on the left- and right-hand-side intermediate code results
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
			left,
			right,
			NoAddress,
			co.TokenStreamIndex)

		// append the instruction to the module (boolean results are not stored on the stack)
		i.AppendInstruction(instruction)

	default:
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unknownConditionalOperation, nil, nil))
	}
}

// Generate code for an assignment statement.
func (i *intermediateCode) VisitAssignmentStatement(s *ast.AssignmentStatementNode) {
	// load the value from the intermediate code result of the right-hand-side expression of the assignment
	s.Expression.Accept(i)
	right := i.popResult()

	// get the variable declaration on the left-hand-side of the assignment
	variableUse := s.Variable.(*ast.IdentifierUseNode)
	variableDeclaration := variableUse.Scope.Lookup(variableUse.Name).Declaration.(*ast.VariableDeclarationNode)

	// determine the block nesting depth of the variable declaration
	declarationDepth := ast.SearchBlock(ast.CurrentBlock, variableDeclaration).Depth

	// determine the block nesting depth of the assignment statement where the variable is used
	assignmentDepth := ast.SearchBlock(ast.CurrentBlock, s).Depth

	// determine the intermediate code target name of the abstract syntax variable declaration
	target := variableUse.Scope.Lookup(variableUse.Name).Extension[symbolExtension].(*symbolMetaData).target

	// variables are immutable and hence a new version is created every time a variable is assigned a new value
	newTargetVersion := variableDeclaration.Scope.NewIdentifierVersion(target)
	variableUse.Scope.Lookup(variableUse.Name).Extension[symbolExtension].(*symbolMetaData).update(newTargetVersion)
	i.module.update(target, newTargetVersion)

	// get the intermediate code symbol table entry of the abstract syntax variable declaration
	codeSymbol := i.module.lookup(newTargetVersion)

	// store the resultant value from the right-hand-side expression in the variable on the left-hand-side of the assignment
	instruction := i.NewInstruction(
		VariableStore,
		right,
		NewAddress(codeSymbol.dataType, codeSymbol.offset, target),
		NewAddress(codeSymbol.dataType, codeSymbol.offset, codeSymbol.target),
		assignmentDepth-declarationDepth,
		s.TokenStreamIndex)

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

	// determine the intermediate code target name of the abstract syntax variable declaration
	target := variableUse.Scope.Lookup(variableUse.Name).Extension[symbolExtension].(*symbolMetaData).target

	// get the intermediate code symbol table entry of the abstract syntax variable declaration
	codeSymbol := i.module.lookup(target)

	// create a variable load instruction to load the variable value into an intermediate code result
	load := i.NewInstruction(
		VariableLoad,
		NewAddress(codeSymbol.dataType, codeSymbol.offset, codeSymbol.target),
		NoAddress,
		NewAddress(codeSymbol.dataType, 0, scope.NewIdentifier(Prefix[ResultPrefix])),
		readDepth-declarationDepth,
		s.TokenStreamIndex)

	// parameter 1 for the readln runtime function
	param := i.NewInstruction(
		Parameter,
		load.Code.Result,
		NoAddress,
		NoAddress,
		s.TokenStreamIndex)

	// call the readln runtime function with 1 parameter
	readln := i.NewInstruction(
		Standard,
		NewAddress(UnsignedInteger64, 0, uint64(1)),
		NewAddress(UnsignedInteger64, 0, uint64(ReadLn)),
		NoAddress,
		s.TokenStreamIndex)

	// variables are immutable and hence a new version is created every time a variable is assigned a new value
	newTargetVersion := variableDeclaration.Scope.NewIdentifierVersion(target)
	variableUse.Scope.Lookup(variableUse.Name).Extension[symbolExtension].(*symbolMetaData).update(newTargetVersion)
	i.module.update(target, newTargetVersion)

	// get the intermediate code symbol table entry of the abstract syntax variable declaration
	codeSymbol = i.module.lookup(newTargetVersion)

	// store the resultant value into the variable used by the read statement
	store := i.NewInstruction(
		VariableStore,
		param.Code.Arg1,
		NewAddress(codeSymbol.dataType, codeSymbol.offset, target),
		NewAddress(codeSymbol.dataType, codeSymbol.offset, codeSymbol.target),
		readDepth-declarationDepth,
		s.TokenStreamIndex)

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

	// parameter 1 for the writeln runtime function
	param := i.NewInstruction(
		Parameter,
		right,
		NoAddress,
		NoAddress,
		s.TokenStreamIndex)

	// call the writeln runtime function with 1 parameter
	write := i.NewInstruction(
		Standard,
		NewAddress(UnsignedInteger64, 0, uint64(1)),
		NewAddress(UnsignedInteger64, 0, uint64(WriteLn)),
		NoAddress,
		s.TokenStreamIndex)

	// append the instructions to the module
	i.AppendInstruction(param)
	i.AppendInstruction(write)
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

	// determine the intermediate code target name of the abstract syntax procedure declaration
	target := procedureUse.Scope.Lookup(procedureUse.Name).Extension[symbolExtension].(*symbolMetaData).target

	// call the function with 0 parameters
	call := i.NewInstruction(
		Call,
		NewAddress(UnsignedInteger64, 0, uint64(0)),
		NewAddress(Label, 0, target),
		NoAddress,
		callDepth-declarationDepth,
		s.TokenStreamIndex)

	// append the instruction to the module
	i.AppendInstruction(call)
}

// Generate code for an if-then statement.
func (i *intermediateCode) VisitIfStatement(s *ast.IfStatementNode) {
	// determine block and its scope where the if-then statement is located
	scope := ast.SearchBlock(ast.CurrentBlock, s).Scope
	behindStatement := scope.NewLabel()

	// calculate the result of the condition expression
	s.Condition.Accept(i)

	// jump behind the statement if the condition is false
	i.jumpConditional(s.Condition, false, behindStatement)

	// execute statement if the condition is true
	s.Statement.Accept(i)

	// append a branch instruction behind the statement instructions
	i.AppendInstruction(i.NewInstruction(Branch, NoAddress, NoAddress, NoAddress, behindStatement, s.TokenStreamIndex))
}

// Generate code for a while-do statement.
func (i *intermediateCode) VisitWhileStatement(s *ast.WhileStatementNode) {
	// determine block and its scope where the while-do statement is located
	scope := ast.SearchBlock(ast.CurrentBlock, s).Scope
	beforeCondition := scope.NewLabel()
	behindStatement := scope.NewLabel()

	// append a branch instruction before the conditional expression instructions
	i.AppendInstruction(i.NewInstruction(Branch, NoAddress, NoAddress, NoAddress, beforeCondition, s.TokenStreamIndex))

	// calculate the result of the conditional expression
	s.Condition.Accept(i)

	// jump behind the statement if the condition is false
	i.jumpConditional(s.Condition, false, behindStatement)

	// execute statement if the condition is true
	s.Statement.Accept(i)

	// append a jump instruction to jump back to the conditional expression instructions
	beforeConditionAddress := NewAddress(Label, 0, beforeCondition)
	i.AppendInstruction(i.NewInstruction(Jump, beforeConditionAddress, NoAddress, NoAddress, s.TokenStreamIndex))

	// append a branch instruction behind the statement instructions
	i.AppendInstruction(i.NewInstruction(Branch, NoAddress, NoAddress, NoAddress, behindStatement, s.TokenStreamIndex))
}

// Generate code for a compound begin-end statement.
func (i *intermediateCode) VisitCompoundStatement(s *ast.CompoundStatementNode) {
	// generate code for all statements in the compound statement
	for _, statement := range s.Statements {
		statement.Accept(i)
	}
}

// Conditional jump instruction based on an expression that must be a unary or conditional operation node.
func (i *intermediateCode) jumpConditional(expression ast.Expression, jumpIfCondition bool, target string) {
	var jump *Instruction
	address := NewAddress(Label, 0, target)

	// odd operation or conditional operations are valid for conditional jumps
	switch condition := expression.(type) {
	// unary operation node with the odd operation
	case *ast.UnaryOperationNode:
		if condition.Operation == ast.Odd {
			if jumpIfCondition {
				jump = i.NewInstruction(JumpNotEqual, address, NoAddress, NoAddress, condition.TokenStreamIndex)
			} else {
				jump = i.NewInstruction(JumpEqual, address, NoAddress, NoAddress, condition.TokenStreamIndex)
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
				jump = i.NewInstruction(JumpEqual, address, NoAddress, NoAddress, condition.TokenStreamIndex)

			case ast.NotEqual:
				jump = i.NewInstruction(JumpNotEqual, address, NoAddress, NoAddress, condition.TokenStreamIndex)

			case ast.Less:
				jump = i.NewInstruction(JumpLess, address, NoAddress, NoAddress, condition.TokenStreamIndex)

			case ast.LessEqual:
				jump = i.NewInstruction(JumpLessEqual, address, NoAddress, NoAddress, condition.TokenStreamIndex)

			case ast.Greater:
				jump = i.NewInstruction(JumpGreater, address, NoAddress, NoAddress, condition.TokenStreamIndex)

			case ast.GreaterEqual:
				jump = i.NewInstruction(JumpGreaterEqual, address, NoAddress, NoAddress, condition.TokenStreamIndex)

			default:
				panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unknownConditionalOperation, nil, nil))
			}
		} else {
			// jump if the condition is false
			switch condition.Operation {
			case ast.Equal:
				jump = i.NewInstruction(JumpNotEqual, address, NoAddress, NoAddress, condition.TokenStreamIndex)

			case ast.NotEqual:
				jump = i.NewInstruction(JumpEqual, address, NoAddress, NoAddress, condition.TokenStreamIndex)

			case ast.Less:
				jump = i.NewInstruction(JumpGreaterEqual, address, NoAddress, NoAddress, condition.TokenStreamIndex)

			case ast.LessEqual:
				jump = i.NewInstruction(JumpGreater, address, NoAddress, NoAddress, condition.TokenStreamIndex)

			case ast.Greater:
				jump = i.NewInstruction(JumpLessEqual, address, NoAddress, NoAddress, condition.TokenStreamIndex)

			case ast.GreaterEqual:
				jump = i.NewInstruction(JumpLess, address, NoAddress, NoAddress, condition.TokenStreamIndex)

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

// Push a result onto the stack of intermediate code results.
func (i *intermediateCode) pushResult(result *Address) {
	i.results.PushBack(result)
}

// Pop a result from the stack of intermediate code results.
func (i *intermediateCode) popResult() *Address {
	result := i.results.Back()

	if result == nil {
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unexpectedIntermediateCodeResult, nil, nil))
	}

	i.results.Remove(result)
	return result.Value.(*Address)
}
