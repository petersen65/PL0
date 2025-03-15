// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package generator

import (
	"container/list"

	ast "github.com/petersen65/PL0/v2/ast"
	cor "github.com/petersen65/PL0/v2/core"
)

// Display name of entry point only used for informational purposes.
const entryPointDisplayName = "@main"

// Abstract syntax extension for the scope.
const scopeExtension ast.ExtensionType = 16

// Abstract syntax extension for the symbol.
const symbolExtension ast.ExtensionType = 17

type (
	// Intermediate code generation compiler phase. It implements the Visitor interface to traverse the AST and generates code.
	generator struct {
		abstractSyntax   ast.Block            // abstract syntax tree to generate intermediate code for
		intermediateCode IntermediateCodeUnit // intermediate code unit to store the generated intermediate code
		results          *list.List           // last-in-first-out results-list holding temporary results from expressions
	}

	// Metadata for each scope in the abstract syntax tree.
	scopeMetaData struct {
		offset int64 // offset for all variables in an abstract syntax scope
	}

	// Metadata for each symbol in the abstract syntax tree.
	symbolMetaData struct {
		name string // intermediate code flattened name created from a scoped abstract syntax symbol
	}
)

// Map abstract syntax datatypes to intermediate code datatypes (they have separate type systems)
var dataTypeMap = map[ast.DataType]DataType{
	ast.Integer64: Integer64,
	ast.Integer32: Integer32,
	ast.Integer16: Integer16,
	ast.Integer8:  Integer8,
	ast.Float64:   Float64,
	ast.Float32:   Float32,
	ast.Rune32:    Rune32,
	ast.Boolean8:  Boolean8,
}

// Create a new intermediate code generator.
func newGenerator(abstractSyntax ast.Block) Generator {
	return &generator{
		abstractSyntax:   abstractSyntax,
		intermediateCode: NewIntermediateCodeUnit(),
		results:          list.New(),
	}
}

// Create metadata for a scope in the abstract syntax tree.
func newScopeMetaData() *scopeMetaData {
	return &scopeMetaData{offset: 0}
}

// Create metadata for a symbol in the abstract syntax tree.
func newSymbolMetaData(name string) *symbolMetaData {
	return &symbolMetaData{name: name}
}

// Generate intermediate code for the abstract syntax tree.
// The generator itself is performing a top down, left to right, and leftmost derivation walk on the abstract syntax tree.
func (i *generator) Generate() {
	// pre-create symbol table for intermediate code by providing a visit function that is called for each node
	if err := ast.Walk(i.abstractSyntax, ast.PreOrder, i, configureSymbols); err != nil {
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, intermediateCodeGenerationFailed, nil, err))
	}

	// generate intermediate code for the abstract syntax tree by using the visitor pattern of the abstract syntax tree
	i.abstractSyntax.Accept(i)
}

// Get access to the generated intermediate code.
func (i *generator) GetIntermediateCodeUnit() IntermediateCodeUnit {
	return i.intermediateCode
}

// Generate code for a block, all nested procedure blocks, and its statement.
func (i *generator) VisitBlock(bn *ast.BlockNode) {
	// only main block has no parent procedure declaration
	if bn.ParentNode == nil {
		blockBegin := bn.Scope.NewIdentifier(prefix[FunctionPrefix])

		// append a target instruction with a branch-label to mark the beginning of the block
		instruction := newInstruction(
			Target, // target for any branching operation
			NewAddress(entryPointDisplayName, Diagnostic, Void, 0), // for diagnostic purposes only
			noAddress,
			noAddress,
			blockBegin) // branch-label as target for any branching operation

		i.intermediateCode.AppendInstruction(instruction)
	} else {
		astSymbol := bn.Scope.Lookup(bn.ParentNode.(*ast.ProcedureDeclarationNode).Name)
		blockBegin := astSymbol.Extension[symbolExtension].(*symbolMetaData).name

		// append a target instruction with a branch-label to mark the beginning of the block
		instruction := newInstruction(
			Target, // target for any branching operation
			NewAddress(astSymbol.Name, Diagnostic, Void, 0), // for diagnostic purposes only
			noAddress,
			noAddress,
			blockBegin) // branch-label as target for any branching operation

		element := i.intermediateCode.AppendInstruction((instruction))

		// update intermediate code function symbol with the instruction that marks the beginning of the block
		codeSymbol := i.intermediateCode.Lookup(blockBegin)
		codeSymbol.Definition = element
	}

	// create prelude for the block
	i.intermediateCode.AppendInstruction(newInstruction(Prelude, noAddress, noAddress, noAddress))

	// all declarations except blocks of nested procedures
	for _, declaration := range bn.Declarations {
		if declaration.Type() != ast.ProcedureDeclarationType {
			declaration.Accept(i)
		}
	}

	// statement of the block
	bn.Statement.Accept(i)

	// create epilog for the block
	i.intermediateCode.AppendInstruction(newInstruction(Epilog, noAddress, noAddress, noAddress))

	// return from the block and mark the end of the block
	i.intermediateCode.AppendInstruction(newInstruction(Return, noAddress, noAddress, noAddress))

	// all blocks of nested procedure declarations (makes a procedure declaration a top-level construct in intermediate code)
	for _, declaration := range bn.Declarations {
		if declaration.Type() == ast.ProcedureDeclarationType {
			declaration.Accept(i)
		}
	}
}

// Generate code for a constant declaration.
func (i *generator) VisitConstantDeclaration(declaration *ast.ConstantDeclarationNode) {
	// not required for code generation
}

// Generate code for a variable declaration.
func (i *generator) VisitVariableDeclaration(vd *ast.VariableDeclarationNode) {
	// access intermediate code metadata from abstract syntax scope
	scopeMetaData := vd.Scope.Extension[scopeExtension].(*scopeMetaData)

	// determine the intermediate code name of the abstract syntax variable declaration
	codeName := vd.Scope.LookupCurrent(vd.Name).Extension[symbolExtension].(*symbolMetaData).name

	// get the intermediate code symbol table entry of the abstract syntax variable declaration
	codeSymbol := i.intermediateCode.Lookup(codeName)

	// set the offset of the variable in its logical memory space
	// ensure that the offset in the logical memory space is aligned to a datatype depended boundary
	byteSize := int64(codeSymbol.DataType.BitSize() / 8)
	scopeMetaData.offset = codeSymbol.DataType.Alignment(scopeMetaData.offset + byteSize)
	codeSymbol.Offset = scopeMetaData.offset

	// allocate memory for the variable in its logical memory space
	instruction := newInstruction(
		Allocate, // allocate memory based on a offset
		NewAddress(vd.Name, Diagnostic, codeSymbol.DataType, codeSymbol.Offset), // for diagnostic purposes only
		noAddress,
		NewAddress(codeSymbol.Name, Variable, codeSymbol.DataType, codeSymbol.Offset), // offset for the variable
		vd.TokenStreamIndex) // variable declaration in the token stream

	// append allocate instruction to the unit and set it as definition for the intermediate code variable
	codeSymbol.Definition = i.intermediateCode.AppendInstruction(instruction)
}

// Generate code for a procedure declaration.
func (i *generator) VisitProcedureDeclaration(pd *ast.ProcedureDeclarationNode) {
	// generate code for the block of the procedure
	pd.Block.Accept(i)
}

// Generate code for a literal.
func (i *generator) VisitLiteral(ln *ast.LiteralNode) {
	// create a value copy instruction to store the literal in an temporary result
	instruction := newInstruction(
		ValueCopy, // copy the value of the literal to a temporary result
		NewAddress(ln.Value, Literal, dataTypeMap[ln.DataType], 0), // literal value
		noAddress,
		NewAddress(ln.Scope.NewIdentifier(prefix[ResultPrefix]), Temporary, dataTypeMap[ln.DataType], 0), // temporary result
		ln.TokenStreamIndex) // literal use in the token stream

	// push the temporary result onto the results-list and append the instruction to the intermediate code unit
	i.pushResult(instruction.Code.Result)
	i.intermediateCode.AppendInstruction(instruction)
}

// Generate code for an identifier use.
func (i *generator) VisitIdentifierUse(iu *ast.IdentifierUseNode) {
	switch iu.Context {
	case ast.Constant:
		// get constant declaration of the constant to load
		constantDeclaration := iu.Scope.Lookup(iu.Name).Declaration.(*ast.ConstantDeclarationNode)

		// determine the intermediate code name of the abstract syntax constant declaration
		codeName := iu.Scope.Lookup(iu.Name).Extension[symbolExtension].(*symbolMetaData).name

		// get the intermediate code symbol table entry of the abstract syntax constant declaration
		codeSymbol := i.intermediateCode.Lookup(codeName)

		// create a value copy instruction to store the constant value in an temporary result
		instruction := newInstruction(
			ValueCopy, // copy the value of the constant to a temporary result
			NewAddress(constantDeclaration.Value, Literal, dataTypeMap[constantDeclaration.DataType], 0), // literal value
			noAddress,
			NewAddress(iu.Scope.NewIdentifier(prefix[ResultPrefix]), Temporary, codeSymbol.DataType, 0), // temporary result
			iu.TokenStreamIndex) // constant use in the token stream

		// push the temporary result onto the results-list and append the instruction to the intermediate code unit
		i.pushResult(instruction.Code.Result)
		i.intermediateCode.AppendInstruction(instruction)

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
		codeSymbol := i.intermediateCode.Lookup(codeName)

		// create a variable load instruction to load the variable value into a temporary result
		instruction := newInstruction(
			VariableLoad, // load the value of the variable from its offset into a temporary result
			NewAddress(codeSymbol.Name, Variable, codeSymbol.DataType, codeSymbol.Offset), // variable offset
			noAddress,
			NewAddress(iu.Scope.NewIdentifier(prefix[ResultPrefix]), Temporary, codeSymbol.DataType, 0), // temporary result
			useDepth-declarationDepth, // block nesting depth difference between variable use and variable declaration
			iu.TokenStreamIndex)       // variable use in the token stream

		// push the temporary result onto the results-list and append the instruction to the intermediate code unit
		i.pushResult(instruction.Code.Result)
		i.intermediateCode.AppendInstruction(instruction)

	case ast.Procedure:
		// not required for code generation

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, invalidContextInIdentifierUse, nil, nil))
	}
}

// Generate code for a unary operation.
func (i *generator) VisitUnaryOperation(uo *ast.UnaryOperationNode) {
	// load the temporary result of the expression from the results-list
	uo.Operand.Accept(i)
	result := i.popResult()

	// perform the unary operation on the temporary result
	switch uo.Operation {
	case ast.Odd:
		// create an odd instruction to check if the temporary result is odd
		instruction := newInstruction(
			Odd,
			result, // consumed temporary result
			noAddress,
			noAddress,           // consumed temporary result is checked in-place, boolean result must be hold externally
			uo.TokenStreamIndex) // unary operation in the token stream

		// append the instruction to the intermediate code unit (boolean results are not stored on the results-list)
		i.intermediateCode.AppendInstruction(instruction)

	case ast.Negate:
		// create a negate instruction to negate the temporary result
		instruction := newInstruction(
			Negate,
			result, // consumed temporary result
			noAddress,
			result,              // consumed temporary result is negated in-place (read, negate, write back negated result)
			uo.TokenStreamIndex) // unary operation in the token stream

		// push the temporary result onto the results-list and append the instruction to the intermediate code unit
		i.pushResult(instruction.Code.Result)
		i.intermediateCode.AppendInstruction(instruction)

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownUnaryOperation, nil, nil))
	}
}

// Generate code for a binary arithmetic operation.
func (i *generator) VisitBinaryOperation(bo *ast.BinaryOperationNode) {
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
		instruction := newInstruction(
			operation,
			left,  // consumed left-hand-side temporary result
			right, // consumed right-hand-side temporary result
			NewAddress(scope.NewIdentifier(prefix[ResultPrefix]), Temporary, left.DataType, 0), // arithmetic operation result
			bo.TokenStreamIndex) // arithmetic operation in the token stream

		// push the temporary result result onto the results-list and append the instruction to the intermediate code unit
		i.pushResult(instruction.Code.Result)
		i.intermediateCode.AppendInstruction(instruction)

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownBinaryOperation, nil, nil))
	}
}

// Generate code for a binary relational operation.
func (i *generator) VisitConditionalOperation(co *ast.ConditionalOperationNode) {
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
		instruction := newInstruction(
			operation,
			left,                // consumed left-hand-side temporary result
			right,               // consumed right-hand-side temporary result
			noAddress,           // consumed temporary results are checked in-place, boolean result must be hold externally
			co.TokenStreamIndex) // conditional operation in the token stream

		// append the instruction to the intermediate code unit (boolean results are not stored on the results-list)
		i.intermediateCode.AppendInstruction(instruction)

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownConditionalOperation, nil, nil))
	}
}

// Generate code for an assignment statement.
func (i *generator) VisitAssignmentStatement(s *ast.AssignmentStatementNode) {
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
	codeSymbol := i.intermediateCode.Lookup(codeName)

	// store the resultant value from the right-hand-side expression in the variable on the left-hand-side of the assignment
	instruction := newInstruction(
		VariableStore, // store the value of the temporary result into the offset of a variable
		right,         // consumed right-hand-side temporary result
		noAddress,
		NewAddress(codeSymbol.Name, Variable, codeSymbol.DataType, codeSymbol.Offset),
		assignmentDepth-declarationDepth, // block nesting depth difference between variable use and variable declaration
		s.TokenStreamIndex)               // assignment statement in the token stream

	// append the instruction to the intermediate code unit
	i.intermediateCode.AppendInstruction(instruction)
}

// Generate code for a read statement.
func (i *generator) VisitReadStatement(s *ast.ReadStatementNode) {
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
	codeSymbol := i.intermediateCode.Lookup(codeName)

	// create a variable load instruction to load the variable value into a temporary result
	load := newInstruction(
		VariableLoad, // load the value of the variable from its offset into a temporary result
		NewAddress(codeSymbol.Name, Variable, codeSymbol.DataType, codeSymbol.Offset), // variable offset
		noAddress,
		NewAddress(scope.NewIdentifier(prefix[ResultPrefix]), Temporary, codeSymbol.DataType, 0), // temporary result
		readDepth-declarationDepth, // block nesting depth difference between variable use and variable declaration
		s.TokenStreamIndex)         // read statement in the token stream

	// parameter 1 for the readln standard function
	param := newInstruction(
		Parameter,        // parameter for a standard function
		load.Code.Result, // temporary result will be replaced by the standard function resultant value
		noAddress,
		noAddress,
		s.TokenStreamIndex) // read statement in the token stream

	// call the readln standard function with 1 parameter
	readln := newInstruction(
		Standard, // function call to the external standard library
		NewAddress(1, Count, UnsignedInteger64, 0), // number of parameters for the standard function
		NewAddress(ReadLn, Code, Integer64, 0),     // code of standard function to call
		noAddress,
		s.TokenStreamIndex) // read statement in the token stream

	// get the intermediate code symbol table entry of the abstract syntax variable declaration
	codeSymbol = i.intermediateCode.Lookup(codeName)

	// store the resultant value into the variable used by the read statement
	store := newInstruction(
		VariableStore,   // store the value of the standard function result into the offset of a variable
		param.Code.Arg1, // standard function resultant value
		noAddress,
		NewAddress(codeSymbol.Name, Variable, codeSymbol.DataType, codeSymbol.Offset), // variable offset
		readDepth-declarationDepth, // block nesting depth difference between variable use and variable declaration
		s.TokenStreamIndex)         // read statement in the token stream

	// append the instructions to the intermediate code unit
	i.intermediateCode.AppendInstruction(load)
	i.intermediateCode.AppendInstruction(param)
	i.intermediateCode.AppendInstruction(readln)
	i.intermediateCode.AppendInstruction(store)
}

// Generate code for a write statement.
func (i *generator) VisitWriteStatement(s *ast.WriteStatementNode) {
	// load the value from the result of the expression on the right-hand-side of the write statement
	s.Expression.Accept(i)
	right := i.popResult()

	// parameter 1 for the writeln standard function
	param := newInstruction(
		Parameter, // parameter for a standard function
		right,     // consumed right-hand-side temporary result
		noAddress,
		noAddress,
		s.TokenStreamIndex) // write statement in the token stream

	// call the writeln standard function with 1 parameter
	writeln := newInstruction(
		Standard, // function call to the external standard library
		NewAddress(1, Count, UnsignedInteger64, 0), // number of parameters for the standard function
		NewAddress(WriteLn, Code, Integer64, 0),    // code of standard function to call
		noAddress,
		s.TokenStreamIndex) // write statement in the token stream

	// append the instructions to the intermediate code unit
	i.intermediateCode.AppendInstruction(param)
	i.intermediateCode.AppendInstruction(writeln)
}

// Generate code for a call statement.
func (i *generator) VisitCallStatement(s *ast.CallStatementNode) {
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
	call := newInstruction(
		Call, // call to an intermediate code function
		NewAddress(0, Count, UnsignedInteger64, 0), // number of parameters for the function
		NewAddress(codeName, Label, String, 0),     // label of intermediate code function to call
		noAddress,
		callDepth-declarationDepth, // block nesting depth difference between procedure call and procedure declaration
		s.TokenStreamIndex)         // call statement in the token stream

	// append the instruction to the intermediate code unit
	i.intermediateCode.AppendInstruction(call)
}

// Generate code for an if-then statement.
func (i *generator) VisitIfStatement(s *ast.IfStatementNode) {
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
	i.intermediateCode.AppendInstruction(newInstruction(Target, noAddress, noAddress, noAddress, behindStatement, s.TokenStreamIndex))
}

// Generate code for a while-do statement.
func (i *generator) VisitWhileStatement(s *ast.WhileStatementNode) {
	// determine block and its scope where the while-do statement is located
	scope := ast.SearchBlock(ast.CurrentBlock, s).Scope
	beforeCondition := scope.NewIdentifier(prefix[LabelPrefix])
	behindStatement := scope.NewIdentifier(prefix[LabelPrefix])

	// append a target instruction before the conditional expression instructions
	i.intermediateCode.AppendInstruction(newInstruction(Target, noAddress, noAddress, noAddress, beforeCondition, s.TokenStreamIndex))

	// calculate the result of the conditional expression
	s.Condition.Accept(i)

	// jump behind the statement if the condition is false
	i.jumpConditional(s.Condition, false, behindStatement)

	// execute statement if the condition is true
	s.Statement.Accept(i)

	// append a jump instruction to jump back to the conditional expression instructions
	beforeConditionAddress := NewAddress(beforeCondition, Label, String, 0)
	i.intermediateCode.AppendInstruction(newInstruction(Jump, beforeConditionAddress, noAddress, noAddress, s.TokenStreamIndex))

	// append a target instruction behind the statement instructions
	i.intermediateCode.AppendInstruction(newInstruction(Target, noAddress, noAddress, noAddress, behindStatement, s.TokenStreamIndex))
}

// Generate code for a compound begin-end statement.
func (i *generator) VisitCompoundStatement(s *ast.CompoundStatementNode) {
	// generate code for all statements in the compound statement
	for _, statement := range s.Statements {
		statement.Accept(i)
	}
}

// Conditional jump instruction based on an expression that must be a unary or conditional operation node.
func (i *generator) jumpConditional(expression ast.Expression, jumpIfCondition bool, label string) {
	var jump *Instruction
	address := NewAddress(label, Label, String, 0)

	// odd operation or conditional operations are valid for conditional jumps
	switch condition := expression.(type) {
	// unary operation node with the odd operation
	case *ast.UnaryOperationNode:
		if condition.Operation == ast.Odd {
			if jumpIfCondition {
				jump = newInstruction(JumpNotEqual, address, noAddress, noAddress, condition.TokenStreamIndex)
			} else {
				jump = newInstruction(JumpEqual, address, noAddress, noAddress, condition.TokenStreamIndex)
			}
		} else {
			panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownUnaryOperation, nil, nil))
		}

	// conditional operation node with the equal, not equal, less, less equal, greater, or greater equal operation
	case *ast.ConditionalOperationNode:
		if jumpIfCondition {
			// jump if the condition is true
			switch condition.Operation {
			case ast.Equal:
				jump = newInstruction(JumpEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.NotEqual:
				jump = newInstruction(JumpNotEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.Less:
				jump = newInstruction(JumpLess, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.LessEqual:
				jump = newInstruction(JumpLessEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.Greater:
				jump = newInstruction(JumpGreater, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.GreaterEqual:
				jump = newInstruction(JumpGreaterEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			default:
				panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownConditionalOperation, nil, nil))
			}
		} else {
			// jump if the condition is false
			switch condition.Operation {
			case ast.Equal:
				jump = newInstruction(JumpNotEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.NotEqual:
				jump = newInstruction(JumpEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.Less:
				jump = newInstruction(JumpGreaterEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.LessEqual:
				jump = newInstruction(JumpGreater, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.Greater:
				jump = newInstruction(JumpLessEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.GreaterEqual:
				jump = newInstruction(JumpLess, address, noAddress, noAddress, condition.TokenStreamIndex)

			default:
				panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownConditionalOperation, nil, nil))
			}
		}

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownConditionalOperation, nil, nil))
	}

	// append the conditional jump instruction to the intermediate code unit
	i.intermediateCode.AppendInstruction(jump)
}

// Push a result onto the results-list of temporary results.
func (i *generator) pushResult(result *Address) {
	i.results.PushBack(result)
}

// Pop a result from the results-list of temporary results.
func (i *generator) popResult() *Address {
	result := i.results.Back()

	if result == nil {
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unexpectedIntermediateCodeResult, nil, nil))
	}

	i.results.Remove(result)
	return result.Value.(*Address)
}

// Configure abstract syntax extensions and fill the symbol table of the intermediate code unit. This is a visit function.
func configureSymbols(node ast.Node, code any) {
	unit := code.(*generator).intermediateCode

	switch n := node.(type) {
	case *ast.BlockNode:
		n.Scope.Extension[scopeExtension] = newScopeMetaData()

	case *ast.ConstantDeclarationNode:
		name := n.Scope.NewIdentifier(prefix[ConstantPrefix])
		n.Scope.LookupCurrent(n.Name).Extension[symbolExtension] = newSymbolMetaData(name)
		unit.Insert(NewSymbol(name, ConstantSymbol, dataTypeMap[n.DataType]))

		if !dataTypeMap[n.DataType].IsSupported() {
			panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unsupportedDataTypeInConstantDeclaration, n, nil))
		}

	case *ast.VariableDeclarationNode:
		name := n.Scope.NewIdentifier(prefix[VariablePrefix])
		n.Scope.LookupCurrent(n.Name).Extension[symbolExtension] = newSymbolMetaData(name)
		unit.Insert(NewSymbol(name, VariableSymbol, dataTypeMap[n.DataType]))

		if !dataTypeMap[n.DataType].IsSupported() {
			panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unsupportedDataTypeInVariableDeclaration, n, nil))
		}

	case *ast.ProcedureDeclarationNode:
		name := n.Block.(*ast.BlockNode).Scope.NewIdentifier(prefix[FunctionPrefix])
		n.Scope.LookupCurrent(n.Name).Extension[symbolExtension] = newSymbolMetaData(name)
		unit.Insert(NewSymbol(name, FunctionSymbol, Void))
	}
}
