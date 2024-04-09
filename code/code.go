// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package code

import (
	"container/list"
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
		results        *list.List // lifo stack holding temporary results from expressions
	}

	// Module represents a logical unit of instructions created from one source file so that a program can be linked together from multiple modules.
	module struct {
		uniqueId     string             // unique identifier of the module
		targets      []string           // enable deterministic iteration over the symbol table in the order of past inserts
		symbolTable  map[string]*symbol // symbol table for intermediate code targets
		instructions *list.List         // intermediate code instructions as doubly linked list that allows reordering
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
	symbol struct {
		target     string        // target name in the intermediate code
		kind       entry         // kind of symbol entry
		dataType   DataType      // data type of the symbol
		offset     uint64        // variable offset in the logical memory space
		definition *list.Element // instruction where the symbol is defined
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
		uniqueId:     uuid.NewString(),
		targets:      make([]string, 0),
		symbolTable:  make(map[string]*symbol),
		instructions: list.New(),
	}
}

// create new symbol for the intermediate code
func newSymbol(target string, kind entry, dataType DataType) *symbol {
	return &symbol{target: target, kind: kind, dataType: dataType}
}

// Create metadata for a scope in the abstract syntax tree.
func newScopeMetaData() *scopeMetaData {
	return &scopeMetaData{}
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

// Deterministically iterate over all symbols in the symbol table of the module.
func (m *module) iterate() <-chan *symbol {
	symbols := make(chan *symbol)

	go func() {
		for _, name := range m.targets {
			symbols <- m.symbolTable[name]
		}

		close(symbols)
	}()

	return symbols
}

// Append an instruction to the intermediate code.
func (m *module) AppendInstruction(instruction *Instruction) *list.Element {
	return m.instructions.PushBack(instruction)
}

// Print the module to the specified writer.
func (m *module) Print(print io.Writer, args ...any) error {
	// enumerate all instructions in the module and print them to the writer
	for e := m.instructions.Front(); e != nil; e = e.Next() {
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
		target := n.Scope.NewIdentifier(TargetPrefix[ConstantPrefix])
		n.Scope.LookupCurrent(n.Name).Extension[symbolExtension] = newSymbolMetaData(target)
		module.insert(newSymbol(target, constant, DataTypeMap[n.DataType]))

	case *ast.VariableDeclarationNode:
		target := n.Scope.NewIdentifier(TargetPrefix[VariablePrefix])
		n.Scope.LookupCurrent(n.Name).Extension[symbolExtension] = newSymbolMetaData(target)
		module.insert(newSymbol(target, variable, DataTypeMap[n.DataType]))

	case *ast.ProcedureDeclarationNode:
		target := n.Block.(*ast.BlockNode).Scope.NewIdentifier(TargetPrefix[FunctionPrefix])
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
func (i *intermediateCode) NewInstruction(operatiom Operation, label string, difference int32, arg1, arg2, result *Address) *Instruction {
	return &Instruction{
		Label:           label,
		DepthDifference: difference,
		Code:            Quadruple{Operation: operatiom, Arg1: arg1, Arg2: arg2, Result: result},
	}
}

// Generate code for a block, all nested procedure blocks, and its statement.
func (i *intermediateCode) VisitBlock(bn *ast.BlockNode) {
	var blockBegin string

	// only main block has no parent procedure declaration
	if bn.ParentNode == nil {
		blockBegin = bn.Scope.NewIdentifier(TargetPrefix[FunctionPrefix])

		// append a target instruction with a target-label to mark the beginning of the block
		instruction := i.NewInstruction(
			Target,
			blockBegin,
			UnusedDifference,
			NewAddress(Void, 0, entryPointDisplayName),
			NoAddress,
			NewAddress(Void, 0, blockBegin))

		i.AppendInstruction(instruction)
	} else {
		astSymbol := bn.Scope.Lookup(bn.ParentNode.(*ast.ProcedureDeclarationNode).Name)
		blockBegin = astSymbol.Extension[symbolExtension].(*symbolMetaData).target

		// append a target instruction with a target-label to mark the beginning of the block
		instruction := i.NewInstruction(
			Target,
			blockBegin,
			UnusedDifference,
			NewAddress(Void, 0, astSymbol.Name),
			NoAddress,
			NewAddress(Void, 0, blockBegin))

		element := i.AppendInstruction(instruction)

		// update intermediate code function symbol with the instruction that marks the beginning of the block
		funcSymbol := i.module.lookup(blockBegin)
		funcSymbol.definition = element
	}

	// all declarations except blocks of nested procedures
	for _, declaration := range bn.Declarations {
		if declaration.Type() != ast.ProcedureDeclarationType {
			declaration.Accept(i)
		}
	}

	// statement of the block
	bn.Statement.Accept(i)

	// return from the block and mark the end of the block
	i.AppendInstruction(i.NewInstruction(Return, blockBegin, UnusedDifference, NoAddress, NoAddress, NoAddress))

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
	// // access intermediate code metadata from abstract syntax scope
	scopeMetaData := vd.Scope.Extension[scopeExtension].(*scopeMetaData)

	// determine the intermediate code target name of the abstract syntax variable declaration
	target := vd.Scope.LookupCurrent(vd.Name).Extension[symbolExtension].(*symbolMetaData).target

	// get the intermediate code symbol table entry of the abstract syntax variable declaration
	codeSymbol := i.module.lookup(target)

	// // set the location of the variable in its logical memory space
	codeSymbol.offset = scopeMetaData.offsetCounter
	scopeMetaData.offsetCounter++

	// // allocate memory for the variable in its logical memory space
	instruction := i.NewInstruction(
		Allocate,
		NoLabel,
		UnusedDifference,
		NewAddress(codeSymbol.dataType, codeSymbol.offset, vd.Name),
		NoAddress,
		NewAddress(codeSymbol.dataType, codeSymbol.offset, codeSymbol.target))

	// append allocate instruction to the module and set it as definition for intermediate code variable
	codeSymbol.definition = i.AppendInstruction(instruction)
}

// Generate code for a procedure declaration.
func (i *intermediateCode) VisitProcedureDeclaration(pd *ast.ProcedureDeclarationNode) {
	// generate code for the block of the procedure
	pd.Block.Accept(i)
}

// Generate code for a literal.
func (i *intermediateCode) VisitLiteral(ln *ast.LiteralNode) {
	// // create a value copy instruction to store the literal in an intermediate code result
	instruction := i.NewInstruction(
		ValueCopy,
		NoLabel,
		UnusedDifference,
		NewAddress(DataTypeMap[ln.DataType], 0, ln.Value),
		NoAddress,
		NewAddress(DataTypeMap[ln.DataType], 0, ln.Scope.NewIdentifier(TargetPrefix[ResultPrefix])))

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
			NoLabel,
			UnusedDifference,
			NewAddress(DataTypeMap[constantDeclaration.DataType], 0, constantDeclaration.Value),
			NoAddress,
			NewAddress(codeSymbol.dataType, 0, iu.Scope.NewIdentifier(TargetPrefix[ResultPrefix])))

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
			NoLabel,
			useDepth-declarationDepth,
			NewAddress(codeSymbol.dataType, codeSymbol.offset, codeSymbol.target),
			NoAddress,
			NewAddress(codeSymbol.dataType, 0, iu.Scope.NewIdentifier(TargetPrefix[ResultPrefix])))

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
			NoLabel,
			UnusedDifference,
			result,
			NoAddress,
			NoAddress)

		// append the instruction to the module (boolean results are not stored on the stack)
		i.AppendInstruction(instruction)

	case ast.Negate:
		// create a negate instruction to negate the intermediate code result
		instruction := i.NewInstruction(
			Negate,
			NoLabel,
			UnusedDifference,
			result,
			NoAddress,
			result) // the intermediate code result is negated in-place (read, negate, write back)

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
			NoLabel,
			UnusedDifference,
			left,
			right,
			NewAddress(left.DataType, 0, scope.NewIdentifier(TargetPrefix[ResultPrefix])))

		// push the intermediate code result result onto the stack and append the instruction to the module
		i.pushResult(instruction.Code.Result)
		i.AppendInstruction(instruction)

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownBinaryOperation, nil, nil))
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
			NoLabel,
			UnusedDifference,
			left,
			right,
			NoAddress)

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
		NoLabel,
		assignmentDepth-declarationDepth,
		right,
		NewAddress(codeSymbol.dataType, codeSymbol.offset, target),
		NewAddress(codeSymbol.dataType, codeSymbol.offset, codeSymbol.target))

	// // append the instruction to the module
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

	// parameter 1 for the readln runtime function
	param := i.NewInstruction(
		Parameter,
		NoLabel,
		UnusedDifference,
		NoAddress,
		NoAddress,
		NewAddress(codeSymbol.dataType, 0, scope.NewIdentifier(TargetPrefix[ResultPrefix])))

	// call the readln runtime function with 1 parameter
	readln := i.NewInstruction(
		Runtime,
		NoLabel,
		UnusedDifference,
		NewAddress(UnsignedInteger64, 0, uint64(1)),
		NewAddress(UnsignedInteger64, 0, uint64(ReadLn)),
		NoAddress)

	// store the resultant value into the variable used by the read statement
	store := i.NewInstruction(
		VariableStore,
		NoLabel,
		readDepth-declarationDepth,
		param.Code.Result,
		NoAddress,
		NewAddress(codeSymbol.dataType, codeSymbol.offset, codeSymbol.target))

	// append the instructions to the module
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
		NoLabel,
		UnusedDifference,
		right,
		NoAddress,
		NoAddress)

	// call the writeln runtime function with 1 parameter
	write := i.NewInstruction(
		Runtime,
		NoLabel,
		UnusedDifference,
		NewAddress(UnsignedInteger64, 0, uint64(1)),
		NewAddress(UnsignedInteger64, 0, uint64(WriteLn)),
		NoAddress)

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
		NoLabel,
		callDepth-declarationDepth,
		NewAddress(UnsignedInteger64, 0, uint64(0)),
		NewAddress(Label, 0, target),
		NoAddress)

	// // append the instruction to the module
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

	// append a target instruction behind the statement instructions
	i.AppendInstruction(i.NewInstruction(Target, behindStatement, UnusedDifference, NoAddress, NoAddress, NoAddress))
}

// Generate code for a while-do statement.
func (i *intermediateCode) VisitWhileStatement(s *ast.WhileStatementNode) {
	// determine block and its scope where the while-do statement is located
	scope := ast.SearchBlock(ast.CurrentBlock, s).Scope
	beforeCondition := scope.NewLabel()
	behindStatement := scope.NewLabel()

	// append a target instruction before the conditional expression instructions
	i.AppendInstruction(i.NewInstruction(Target, beforeCondition, UnusedDifference, NoAddress, NoAddress, NoAddress))

	// calculate the result of the conditional expression
	s.Condition.Accept(i)

	// jump behind the statement if the condition is false
	i.jumpConditional(s.Condition, false, behindStatement)

	// execute statement if the condition is true
	s.Statement.Accept(i)

	// append a target instruction behind the statement instructions
	i.AppendInstruction(i.NewInstruction(Target, behindStatement, UnusedDifference, NoAddress, NoAddress, NoAddress))

	// append a jump instruction to jump back to the conditional expression instructions
	beforeConditionAddress := NewAddress(Label, 0, beforeCondition)
	i.AppendInstruction(i.NewInstruction(Jump, NoLabel, UnusedDifference, beforeConditionAddress, NoAddress, NoAddress))
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
				jump = i.NewInstruction(JumpNotEqual, NoLabel, UnusedDifference, address, NoAddress, NoAddress)
			} else {
				jump = i.NewInstruction(JumpEqual, NoLabel, UnusedDifference, address, NoAddress, NoAddress)
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
				jump = i.NewInstruction(JumpEqual, NoLabel, UnusedDifference, address, NoAddress, NoAddress)

			case ast.NotEqual:
				jump = i.NewInstruction(JumpNotEqual, NoLabel, UnusedDifference, address, NoAddress, NoAddress)

			case ast.Less:
				jump = i.NewInstruction(JumpLess, NoLabel, UnusedDifference, address, NoAddress, NoAddress)

			case ast.LessEqual:
				jump = i.NewInstruction(JumpLessEqual, NoLabel, UnusedDifference, address, NoAddress, NoAddress)

			case ast.Greater:
				jump = i.NewInstruction(JumpGreater, NoLabel, UnusedDifference, address, NoAddress, NoAddress)

			case ast.GreaterEqual:
				jump = i.NewInstruction(JumpGreaterEqual, NoLabel, UnusedDifference, address, NoAddress, NoAddress)

			default:
				panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unknownConditionalOperation, nil, nil))
			}
		} else {
			// jump if the condition is false
			switch condition.Operation {
			case ast.Equal:
				jump = i.NewInstruction(JumpNotEqual, NoLabel, UnusedDifference, address, NoAddress, NoAddress)

			case ast.NotEqual:
				jump = i.NewInstruction(JumpEqual, NoLabel, UnusedDifference, address, NoAddress, NoAddress)

			case ast.Less:
				jump = i.NewInstruction(JumpGreaterEqual, NoLabel, UnusedDifference, address, NoAddress, NoAddress)

			case ast.LessEqual:
				jump = i.NewInstruction(JumpGreater, NoLabel, UnusedDifference, address, NoAddress, NoAddress)

			case ast.Greater:
				jump = i.NewInstruction(JumpLessEqual, NoLabel, UnusedDifference, address, NoAddress, NoAddress)

			case ast.GreaterEqual:
				jump = i.NewInstruction(JumpLess, NoLabel, UnusedDifference, address, NoAddress, NoAddress)

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
