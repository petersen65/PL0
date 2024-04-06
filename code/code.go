// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package code

import (
	"container/list"
	"fmt"
	"io"

	ast "github.com/petersen65/PL0/v2/ast"
	cor "github.com/petersen65/PL0/v2/core"
)

type (
	// Metadata for each block in the abstract syntax tree.
	blockMetaData struct {
		uniqueId            int32      // unique identifier of the block that this metadata belongs to
		stackOffset         uint64     // offset counter for all variables in the stack frame of the block
		tempVariableCounter uint64     // temporary variable counter for the block
		labelCounter        uint64     // label counter for the block
		results             *list.List // lifo stack holding temporary results from expressions
	}

	// Intermediate code generation compiler pass. It implements the Visitor interface to traverse the AST and generate code.
	intermediateCode struct {
		abstractSyntax ast.Block                // abstract syntax tree to generate code for
		metaData       map[int32]*blockMetaData // metadata for each block in the abstract syntax tree
		module         Module                   // module to store the generated intermediate code
	}
)

// Create a new intermediate code generator.
func newIntermediateCode(abstractSyntax ast.Block) IntermediateCode {
	return &intermediateCode{
		abstractSyntax: abstractSyntax,
		metaData:       make(map[int32]*blockMetaData),
		module:         make(Module, 0),
	}
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
	return fmt.Sprintf("%v:%v:%v", a.DataType, a.Offset, a.Variable)
}

// String representation of an intermediate code operation.
func (o Operation) String() string {
	return OperationNames[o]
}

// String representation of an intermediate code instruction.
func (i *Instruction) String() string {
	return fmt.Sprintf(
		"%-8v %4v    %-12v    %-16v    %-16v    %-16v",
		i.Label,
		i.DepthDifference,
		i.Code.Operation,
		i.Code.Arg1,
		i.Code.Arg2,
		i.Code.Result)
}

// Print the module to the specified writer.
func (m Module) Print(print io.Writer, args ...any) error {
	// enumerate all instructions in the module and print them to the writer
	for _, instruction := range m {
		_, err := fmt.Fprintf(print, "%v\n", instruction)

		if err != nil {
			return cor.NewGeneralError(cor.Intermediate, failureMap, cor.Error, intermediateCodeExportFailed, nil, err)
		}
	}

	return nil
}

// Export the module of the intermediate code generator.
func (m Module) Export(format cor.ExportFormat, print io.Writer) error {
	switch format {
	case cor.Text:
		// print is a convenience function to export the module as a string to the print writer
		return m.Print(print)

	default:
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unknownExportFormat, format, nil))
	}
}

// Create a new compiler-generated temporary variable for a block.
func (b *blockMetaData) newTempVariable(dataType DataType) *Address {
	b.tempVariableCounter++
	return NewAddress(dataType, 0, fmt.Sprintf("t%v.%v", b.uniqueId, b.tempVariableCounter))
}

// Create a new compiler-generated label for a block.
func (b *blockMetaData) newLabel(labelType LabelType) string {
	b.labelCounter++
	return fmt.Sprintf("%v%v.%v", LabelPrefix[labelType], b.uniqueId, b.labelCounter)
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
	i.abstractSyntax.Accept(i)
}

// Get the generated intermediate code module.
func (i *intermediateCode) GetModule() Module {
	return i.module
}

// Append an instruction to the intermediate code module.
func (i *intermediateCode) AppendInstruction(instruction *Instruction) {
	i.module = append(i.module, instruction)
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
	// create metadata for the block
	i.metaData[bn.UniqueId] = new(blockMetaData)
	i.metaData[bn.UniqueId].uniqueId = bn.UniqueId
	i.metaData[bn.UniqueId].results = list.New()

	// create a procedure label for the block to mark the beginning of the block
	procedure := i.metaData[bn.UniqueId].newLabel(Procedure)
	i.AppendInstruction(i.NewInstruction(Target, procedure, UnusedDifference, NoAddress, NoAddress, NoAddress))

	// all declarations and with that all blocks of nested procedures (calls generator recursively)
	for _, declaration := range bn.Declarations {
		declaration.Accept(i)
	}

	// statement of the block
	bn.Statement.Accept(i)

	// return from the block and mark the end of the block
	i.AppendInstruction(i.NewInstruction(Return, procedure, UnusedDifference, NoAddress, NoAddress, NoAddress))
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
	vd.Offset = metaData.stackOffset
	metaData.stackOffset++

	// allocate memory for the variable in its logical memory space
	Instruction := i.NewInstruction(
		Allocate,
		NoLabel,
		UnusedDifference,
		NoAddress,
		NoAddress,
		NewAddress(DataTypeMap[vd.DataType], vd.Offset, vd.Name))

	// append instruction to the module
	i.AppendInstruction(Instruction)
}

// Generate code for a procedure declaration.
func (i *intermediateCode) VisitProcedureDeclaration(pd *ast.ProcedureDeclarationNode) {
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
		metaData.newTempVariable(DataTypeMap[ln.DataType]))

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
			metaData.newTempVariable(DataTypeMap[constantDeclaration.DataType]))

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
			metaData.newTempVariable(DataTypeMap[variableDeclaration.DataType]))

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
			metaData.newTempVariable(result.DataType))

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
			metaData.newTempVariable(left.DataType))

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

	// store the resultant value from the right-hand-side expression in the variable on the left-hand-side of the assignment
	insstruction := i.NewInstruction(
		VariableStore,
		NoLabel,
		assignmentDepth-declarationDepth,
		right,
		NoAddress,
		NewAddress(DataTypeMap[variableDeclaration.DataType], variableDeclaration.Offset, variableDeclaration.Name))

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
		metaData.newTempVariable(Integer64))

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
		NewAddress(DataTypeMap[variableDeclaration.DataType], variableDeclaration.Offset, variableDeclaration.Name))

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
		metaData.newTempVariable(right.DataType))

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

	// call the procedure with 0 parameters
	call := i.NewInstruction(
		Call,
		NoLabel,
		callDepth-declarationDepth,
		NewAddress(UnsignedInteger64, 0, uint64(0)),
		NewAddress(Label, 0, procedureDeclaration.Block.(*ast.BlockNode).Label),
		NoAddress)

	// append the instruction to the module
	i.AppendInstruction(call)
}

// Generate code for an if-then statement.
func (i *intermediateCode) VisitIfStatement(s *ast.IfStatementNode) {
	// access metadata of the current block
	metaData := i.metaData[ast.SearchBlock(ast.CurrentBlock, s).UniqueId]
	behindStatement := metaData.newLabel(Branch)

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
	beforeCondition := metaData.newLabel(Branch)
	behindStatement := metaData.newLabel(Branch)

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
	i.AppendInstruction(i.NewInstruction(Jump, beforeCondition, UnusedDifference, NoAddress, NoAddress, NoAddress))
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

	// odd operation or conditional operations are valid for conditional jumps
	switch condition := expression.(type) {
	// unary operation node with the odd operation
	case *ast.UnaryOperationNode:
		if condition.Operation == ast.Odd {
			if jumpIfCondition {
				jump = i.NewInstruction(JumpNotEqual, target, UnusedDifference, NoAddress, NoAddress, NoAddress)
			} else {
				jump = i.NewInstruction(JumpEqual, target, UnusedDifference, NoAddress, NoAddress, NoAddress)
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
				jump = i.NewInstruction(JumpEqual, target, UnusedDifference, NoAddress, NoAddress, NoAddress)

			case ast.NotEqual:
				jump = i.NewInstruction(JumpNotEqual, target, UnusedDifference, NoAddress, NoAddress, NoAddress)

			case ast.Less:
				jump = i.NewInstruction(JumpLess, target, UnusedDifference, NoAddress, NoAddress, NoAddress)

			case ast.LessEqual:
				jump = i.NewInstruction(JumpLessEqual, target, UnusedDifference, NoAddress, NoAddress, NoAddress)

			case ast.Greater:
				jump = i.NewInstruction(JumpGreater, target, UnusedDifference, NoAddress, NoAddress, NoAddress)

			case ast.GreaterEqual:
				jump = i.NewInstruction(JumpGreaterEqual, target, UnusedDifference, NoAddress, NoAddress, NoAddress)

			default:
				panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unknownConditionalOperation, nil, nil))
			}
		} else {
			// jump if the condition is false
			switch condition.Operation {
			case ast.Equal:
				jump = i.NewInstruction(JumpNotEqual, target, UnusedDifference, NoAddress, NoAddress, NoAddress)

			case ast.NotEqual:
				jump = i.NewInstruction(JumpEqual, target, UnusedDifference, NoAddress, NoAddress, NoAddress)

			case ast.Less:
				jump = i.NewInstruction(JumpGreaterEqual, target, UnusedDifference, NoAddress, NoAddress, NoAddress)

			case ast.LessEqual:
				jump = i.NewInstruction(JumpGreater, target, UnusedDifference, NoAddress, NoAddress, NoAddress)

			case ast.Greater:
				jump = i.NewInstruction(JumpLessEqual, target, UnusedDifference, NoAddress, NoAddress, NoAddress)

			case ast.GreaterEqual:
				jump = i.NewInstruction(JumpLess, target, UnusedDifference, NoAddress, NoAddress, NoAddress)

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
