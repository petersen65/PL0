// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package code

import (
	"container/list"
	"fmt"
	"io"

	"github.com/google/uuid"
	ast "github.com/petersen65/PL0/v2/ast"
	cor "github.com/petersen65/PL0/v2/core"
)

// Function name of the entrypoint.
const entryPoint = "@main"

// Abstract syntax extension type for intermediate code.
const scopeExtension ast.ExtensionType = 16

// Kind of supported symbol entry.
const (
	_ = entry(iota)
	variable
	function
)

type (
	// Intermediate code generation compiler pass. It implements the Visitor interface to traverse the AST and generate code.
	intermediateCode struct {
		abstractSyntax ast.Block // abstract syntax tree to generate code for
		module         *module   // module to store the generated intermediate code
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
		offsetCounter uint64     // offset counter for all variables of a scope
		results       *list.List // lifo stack holding temporary results from expressions
	}

	// Kind of symbol entries.
	entry int

	// Symbol represents a symbol in the intermediate code that maps its target to where it was defined.
	symbol struct {
		target     string        // target label in the intermediate code
		kind       entry         // kind of symbol entry
		dataType   DataType      // data type of the symbol
		offset     uint64        // variable offset in the logical memory space
		definition *list.Element // instruction where the symbol is defined
	}
)

// Create a new intermediate code generator.
func newIntermediateCode(abstractSyntax ast.Block) IntermediateCode {
	return &intermediateCode{abstractSyntax: abstractSyntax, module: NewModule().(*module)}
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

// Create metadata for a scope in the abstract syntax tree.
func newBlockMetaData(uniqueId int32) *scopeMetaData {
	return &scopeMetaData{results: list.New()}
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

// Push a result onto the stack of temporary results.
func (b *blockMetaData) pushResult(result *Address) {
	b.results.PushBack(result)
}

// Pop a result from the stack of temporary results.
func (b *blockMetaData) popResult() *Address {
	result := b.results.Back()

	if result == nil {
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unexpectedTemporaryResult, nil, nil))
	}

	b.results.Remove(result)
	return result.Value.(*Address)
}

// Generate intermediate code for the abstract syntax tree.
// The generator itself is performing a top down, left to right, and leftmost derivation walk on the abstract syntax tree.
func (i *intermediateCode) Generate() {
	// create intermediate code metadata for all blocks from the abstract syntax tree
	ast.Walk(i.abstractSyntax, ast.PreOrder, i, func(node ast.Node, code any) {
		if bn, ok := node.(*ast.BlockNode); ok {
			metadata := newBlockMetaData(bn.UniqueId)
			code.(*intermediateCode).metaData[bn.UniqueId] = metadata

			// name of entrypoint function
			name := entryPoint

			// only main block has no parent procedure declaration
			if bn.ParentNode != nil {
				name = bn.ParentNode.(*ast.ProcedureDeclarationNode).Name
			}

			// pre-create intermediate code function symbols for all abstract syntax procedures to enable forward calls
			i.module.insert(
				&symbol{
					name:       name,
					target:     metadata.newLabel(FunctionLabel),
					kind:       function,
					dataType:   Void,
					definition: nil, // emphasize that the intermediate code of the function body is not yet generated
				})
		}
	})

	// generate intermediate code for the abstract syntax tree using created metadata
	i.abstractSyntax.Accept(i)
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
	// name of entrypoint function
	name := entryPoint

	// only main block has no parent procedure declaration
	if bn.ParentNode != nil {
		name = bn.ParentNode.(*ast.ProcedureDeclarationNode).Name
	}

	// intermediate code function symbol
	funcSymbol := i.module.lookup(name, byName)

	// append a target instruction with a target-label to mark the beginning of the block
	// update intermediate code definition in the symbol table
	funcSymbol.definition = i.AppendInstruction(
		i.NewInstruction(Target, funcSymbol.target, UnusedDifference, NoAddress, NoAddress, NoAddress))

	// all declarations except blocks of nested procedures
	for _, declaration := range bn.Declarations {
		if declaration.Type() != ast.ProcedureDeclarationType {
			declaration.Accept(i)
		}
	}

	// statement of the block
	bn.Statement.Accept(i)

	// return from the block and mark the end of the block
	i.AppendInstruction(i.NewInstruction(Return, funcSymbol.target, UnusedDifference, NoAddress, NoAddress, NoAddress))

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
	// access metadata of the current block
	metaData := i.metaData[ast.SearchBlock(ast.CurrentBlock, vd).UniqueId]

	// set the location of the variable in its logical memory space
	vd.Offset = metaData.offsetCounter
	metaData.offsetCounter++

	// allocate memory for the variable in its logical memory space
	instruction := i.NewInstruction(
		Allocate,
		NoLabel,
		UnusedDifference,
		NewAddress(DataTypeMap[vd.DataType], vd.Offset, vd.Name),
		NoAddress,
		metaData.newTempVariable(DataTypeMap[vd.DataType], vd.Offset))

	// append instruction to the module
	element := i.AppendInstruction(instruction)

	// insert the variable into the symbol table of the module
	i.module.insert(
		&symbol{
			name:       vd.Name,
			target:     instruction.Code.Result.Variable,
			kind:       variable,
			dataType:   instruction.Code.Result.DataType,
			offset:     instruction.Code.Result.Offset,
			definition: element,
		})
}

// Generate code for a procedure declaration.
func (i *intermediateCode) VisitProcedureDeclaration(pd *ast.ProcedureDeclarationNode) {
	// generate code for the block of the procedure
	pd.Block.Accept(i)
}

// Generate code for a literal.
func (i *intermediateCode) VisitLiteral(ln *ast.LiteralNode) {
	// access metadata of the current block
	metaData := i.metaData[ast.SearchBlock(ast.CurrentBlock, ln).UniqueId]

	// create a value copy instruction to store the literal in a temporary variable
	instruction := i.NewInstruction(
		ValueCopy,
		NoLabel,
		UnusedDifference,
		NewAddress(DataTypeMap[ln.DataType], 0, ln.Value),
		NoAddress,
		metaData.newTempVariable(DataTypeMap[ln.DataType], 0))

	// push the temporary result onto the stack and append the instruction to the module
	metaData.pushResult(instruction.Code.Result)
	i.AppendInstruction(instruction)
}

// Generate code for an identifier use.
func (i *intermediateCode) VisitIdentifierUse(iu *ast.IdentifierUseNode) {
	// access metadata of the current block
	metaData := i.metaData[ast.SearchBlock(ast.CurrentBlock, iu).UniqueId]

	switch iu.Context {
	case ast.Constant:
		// get constant declaration of the constant to load
		constantDeclaration := iu.Scope.Lookup(iu.Name).Declaration.(*ast.ConstantDeclarationNode)

		// create a value copy instruction to store the constant value in a temporary variable
		instruction := i.NewInstruction(
			ValueCopy,
			NoLabel,
			UnusedDifference,
			NewAddress(DataTypeMap[constantDeclaration.DataType], 0, constantDeclaration.Value),
			NoAddress,
			metaData.newTempVariable(DataTypeMap[constantDeclaration.DataType], 0))

		// push the temporary result onto the stack and append the instruction to the module
		metaData.pushResult(instruction.Code.Result)
		i.AppendInstruction(instruction)

	case ast.Variable:
		// get variable declaration of the variable to load
		variableDeclaration := iu.Scope.Lookup(iu.Name).Declaration.(*ast.VariableDeclarationNode)

		// determine the block nesting depth of the variable declaration
		declarationDepth := ast.SearchBlock(ast.CurrentBlock, variableDeclaration).Depth

		// determine the block nesting depth of the variable use from inside an expression or statement
		useDepth := ast.SearchBlock(ast.CurrentBlock, iu).Depth

		// create a variable load instruction to load the variable value into a temporary variable
		instruction := i.NewInstruction(
			VariableLoad,
			NoLabel,
			useDepth-declarationDepth,
			NewAddress(DataTypeMap[variableDeclaration.DataType], variableDeclaration.Offset, variableDeclaration.Name),
			NoAddress,
			metaData.newTempVariable(DataTypeMap[variableDeclaration.DataType], variableDeclaration.Offset))

		// push the temporary result onto the stack and append the instruction to the module
		metaData.pushResult(instruction.Code.Result)
		i.AppendInstruction(instruction)

	case ast.Procedure:
		// not required for code generation

	default:
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, invalidContextInIdentifierUse, nil, nil))
	}
}

// Generate code for a unary operation.
func (i *intermediateCode) VisitUnaryOperation(uo *ast.UnaryOperationNode) {
	// access metadata of the current block
	metaData := i.metaData[ast.SearchBlock(ast.CurrentBlock, uo).UniqueId]

	// load the temporary result of the expression from the stack
	uo.Operand.Accept(i)
	result := metaData.popResult()

	// perform the unary operation on the temporary result
	switch uo.Operation {
	case ast.Odd:
		// create an odd instruction to check if the temporary result is odd
		instruction := i.NewInstruction(
			Odd,
			NoLabel,
			UnusedDifference,
			result,
			NoAddress,
			NoAddress)

		// append the instruction to the module
		i.AppendInstruction(instruction)

	case ast.Negate:
		// create a negate instruction to negate the temporary result
		instruction := i.NewInstruction(
			Negate,
			NoLabel,
			UnusedDifference,
			result,
			NoAddress,
			metaData.newTempVariable(result.DataType, result.Offset))

		// push the temporary result onto the stack and append the instruction to the module
		metaData.pushResult(instruction.Code.Result)
		i.AppendInstruction(instruction)

	default:
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unknownUnaryOperation, nil, nil))
	}
}

// Generate code for a binary arithmetic operation.
func (i *intermediateCode) VisitBinaryOperation(bo *ast.BinaryOperationNode) {
	// access metadata of the current block
	metaData := i.metaData[ast.SearchBlock(ast.CurrentBlock, bo).UniqueId]

	// load the temporary results of the left and right expressions from the stack
	bo.Left.Accept(i)
	bo.Right.Accept(i)
	right := metaData.popResult()
	left := metaData.popResult()

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

		// create a binary arithmetic operation instruction to perform the operation on the left- and right-hand-side temporary results
		instruction := i.NewInstruction(
			operation,
			NoLabel,
			UnusedDifference,
			left,
			right,
			metaData.newTempVariable(left.DataType, 0))

		// push the temporary result onto the stack and append the instruction to the module
		metaData.pushResult(instruction.Code.Result)
		i.AppendInstruction(instruction)

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownBinaryOperation, nil, nil))
	}
}

// Generate code for a binary relational operation.
func (i *intermediateCode) VisitConditionalOperation(co *ast.ConditionalOperationNode) {
	// access metadata of the current block
	metaData := i.metaData[ast.SearchBlock(ast.CurrentBlock, co).UniqueId]

	// load the temporary results of the left and right expressions from the stack
	co.Left.Accept(i)
	co.Right.Accept(i)
	right := metaData.popResult()
	left := metaData.popResult()

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

		// create a binary relational operation instruction to perform the operation on the left- and right-hand-side temporary results
		instruction := i.NewInstruction(
			operation,
			NoLabel,
			UnusedDifference,
			left,
			right,
			NoAddress)

		// append the instruction to the module
		i.AppendInstruction(instruction)

	default:
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unknownConditionalOperation, nil, nil))
	}
}

// Generate code for an assignment statement.
func (i *intermediateCode) VisitAssignmentStatement(s *ast.AssignmentStatementNode) {
	// access metadata of the current block
	metaData := i.metaData[ast.SearchBlock(ast.CurrentBlock, s).UniqueId]

	// load the value from the result of the right-hand-side expression of the assignment
	s.Expression.Accept(i)
	right := metaData.popResult()

	// get the variable declaration on the left-hand-side of the assignment
	variableUse := s.Variable.(*ast.IdentifierUseNode)
	variableDeclaration := variableUse.Scope.Lookup(variableUse.Name).Declaration.(*ast.VariableDeclarationNode)

	// determine the block nesting depth of the variable declaration
	declarationDepth := ast.SearchBlock(ast.CurrentBlock, variableDeclaration).Depth

	// determine the block nesting depth of the assignment statement where the variable is used
	assignmentDepth := ast.SearchBlock(ast.CurrentBlock, s).Depth

	// map the abstract syntax variable declaration to the intermediate code variable symbol
	varSymbol := i.module.lookup(variableDeclaration.Name, byName)

	// store the resultant value from the right-hand-side expression in the variable on the left-hand-side of the assignment
	insstruction := i.NewInstruction(
		VariableStore,
		NoLabel,
		assignmentDepth-declarationDepth,
		right,
		NoAddress,
		NewAddress(varSymbol.dataType, varSymbol.offset, varSymbol.target))

	// append the instruction to the module
	i.AppendInstruction(insstruction)
}

// Generate code for a read statement.
func (i *intermediateCode) VisitReadStatement(s *ast.ReadStatementNode) {
	// access metadata of the current block
	metaData := i.metaData[ast.SearchBlock(ast.CurrentBlock, s).UniqueId]

	// get the variable declaration of the variable to read into
	variableUse := s.Variable.(*ast.IdentifierUseNode)
	variableDeclaration := variableUse.Scope.Lookup(variableUse.Name).Declaration.(*ast.VariableDeclarationNode)

	// determine the block nesting depth of the variable declaration
	declarationDepth := ast.SearchBlock(ast.CurrentBlock, variableDeclaration).Depth

	// determine the block nesting depth of the read statement where the variable is used
	readDepth := ast.SearchBlock(ast.CurrentBlock, s).Depth

	// parameter 1 for the readln runtime function
	param := i.NewInstruction(
		Parameter,
		NoLabel,
		UnusedDifference,
		NoAddress,
		NoAddress,
		metaData.newTempVariable(Integer64, 0))

	// call the readln runtime function with 1 parameter
	readln := i.NewInstruction(
		Runtime,
		NoLabel,
		UnusedDifference,
		NewAddress(UnsignedInteger64, 0, uint64(1)),
		NewAddress(UnsignedInteger64, 0, uint64(ReadLn)),
		NoAddress)

	// map the abstract syntax variable declaration to the intermediate code variable symbol
	varSymbol := i.module.lookup(variableDeclaration.Name, byName)

	// store the resultant value into the variable used by the read statement
	store := i.NewInstruction(
		VariableStore,
		NoLabel,
		readDepth-declarationDepth,
		param.Code.Result,
		NoAddress,
		NewAddress(varSymbol.dataType, varSymbol.offset, varSymbol.target))

	// append the instructions to the module
	i.AppendInstruction(param)
	i.AppendInstruction(readln)
	i.AppendInstruction(store)
}

// Generate code for a write statement.
func (i *intermediateCode) VisitWriteStatement(s *ast.WriteStatementNode) {
	// access metadata of the current block
	metaData := i.metaData[ast.SearchBlock(ast.CurrentBlock, s).UniqueId]

	// load the value from the result of the expression on the right-hand-side of the write statement
	s.Expression.Accept(i)
	right := metaData.popResult()

	// parameter 1 for the writeln runtime function
	param := i.NewInstruction(
		Parameter,
		NoLabel,
		UnusedDifference,
		right,
		NoAddress,
		metaData.newTempVariable(right.DataType, 0))

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

	// map the abstract syntax procedure declaration to the intermediate code function symbol
	funcSymbol := i.module.lookup(procedureDeclaration.Name, byName)

	// call the function with 0 parameters
	call := i.NewInstruction(
		Call,
		NoLabel,
		callDepth-declarationDepth,
		NewAddress(UnsignedInteger64, 0, uint64(0)),
		NewAddress(Label, 0, funcSymbol.target),
		NoAddress)

	// append the instruction to the module
	i.AppendInstruction(call)
}

// Generate code for an if-then statement.
func (i *intermediateCode) VisitIfStatement(s *ast.IfStatementNode) {
	// access metadata of the current block
	metaData := i.metaData[ast.SearchBlock(ast.CurrentBlock, s).UniqueId]
	behindStatement := metaData.newLabel(BranchLabel)

	// calculate the result of the condition expression
	s.Condition.Accept(i)

	// jump behind the statement if the condition is false
	i.jumpConditional(s.Condition, false, behindStatement)

	// execute statement if the condition is true
	s.Statement.Accept(i)

	// append a label instruction behind the statement instructions
	i.AppendInstruction(i.NewInstruction(Target, behindStatement, UnusedDifference, NoAddress, NoAddress, NoAddress))
}

// Generate code for a while-do statement.
func (i *intermediateCode) VisitWhileStatement(s *ast.WhileStatementNode) {
	// access metadata of the current block
	metaData := i.metaData[ast.SearchBlock(ast.CurrentBlock, s).UniqueId]
	beforeCondition := metaData.newLabel(BranchLabel)
	behindStatement := metaData.newLabel(BranchLabel)

	// append a label instruction before the condition expression instructions
	i.AppendInstruction(i.NewInstruction(Target, beforeCondition, UnusedDifference, NoAddress, NoAddress, NoAddress))

	// calculate the result of the condition expression
	s.Condition.Accept(i)

	// jump behind the statement if the condition is false
	i.jumpConditional(s.Condition, false, behindStatement)

	// execute statement if the condition is true
	s.Statement.Accept(i)

	// append a label instruction behind the statement instructions
	i.AppendInstruction(i.NewInstruction(Target, behindStatement, UnusedDifference, NoAddress, NoAddress, NoAddress))

	// append a jump instruction to jump back to the condition expression instructions
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
func (i *intermediateCode) jumpConditional(expression ast.Expression, jumpIfCondition bool, label string) {
	var jump *Instruction
	address := NewAddress(Label, 0, label)

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
