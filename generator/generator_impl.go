// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package generator

import (
	"container/list"

	ast "github.com/petersen65/PL0/v2/ast"
	cor "github.com/petersen65/PL0/v2/core"
	ic "github.com/petersen65/PL0/v2/generator/intermediate"
)

// Display name of the internal static link variable used to hold the static link of the current block.
const staticLinkDisplayName = "@staticlink"

// Abstract syntax extension for the symbol.
const symbolExtension ast.ExtensionType = 16

// Prefixes for address names in the three-address code concept if a name does not contain a value.
const (
	labelPrefix    prefixType = iota // the label prefix is used for instruction labels and the address variant 'Label'
	registerPrefix                   // the register prefix is used virtual registers contained in the address variant 'Register'
	constantPrefix                   // the constant prefix is used for flattened constant names in the intermediate code
	variablePrefix                   // the variable prefix is used for flattened variable names in the intermediate code
	functionPrefix                   // the function prefix is used for flattened function names in the intermediate code
)

type (
	// Enumeration of prefixes used for names of addresses.
	prefixType int

	// Intermediate code generation compiler phase. It implements the Visitor interface to traverse the AST and generates code.
	generator struct {
		abstractSyntax   ast.Block               // abstract syntax tree to generate intermediate code for
		intermediateCode ic.IntermediateCodeUnit // intermediate code unit to store the generated intermediate code
		results          *list.List              // last-in-first-out results-list holding results from expressions
	}

	// Metadata for each symbol in the abstract syntax tree.
	symbolMetaData struct {
		name string // intermediate code flattened name created from a scoped abstract syntax symbol
	}
)

var (
	// Map abstract syntax datatypes to intermediate code datatypes (intentially their type systems are kept separate).
	dataTypeMap = map[ast.DataType]ic.DataType{
		ast.Integer64:  ic.Integer64,
		ast.Integer32:  ic.Integer32,
		ast.Integer16:  ic.Integer16,
		ast.Integer8:   ic.Integer8,
		ast.Float64:    ic.Float64,
		ast.Float32:    ic.Float32,
		ast.Unsigned64: ic.Unsigned64,
		ast.Unsigned32: ic.Unsigned32,
		ast.Unsigned16: ic.Unsigned16,
		ast.Unsigned8:  ic.Unsigned8,
		ast.Boolean:    ic.Boolean,
		ast.Character:  ic.Character,
		ast.String:     ic.String,
	}

	// Prefixes used for names of addresses.
	prefix = map[prefixType]rune{
		labelPrefix:    'l',
		registerPrefix: 'r',
		constantPrefix: 'c',
		variablePrefix: 'v',
		functionPrefix: 'f',
	}

	// NoAddress represents an unused address in the three-address code concept.
	noAddress = &ic.Address{Name: "-", Variant: ic.Empty, DataType: ic.Untyped}
)

// Create a new intermediate code generator.
func newGenerator(abstractSyntax ast.Block) Generator {
	return &generator{
		abstractSyntax:   abstractSyntax,
		intermediateCode: ic.NewIntermediateCodeUnit(),
		results:          list.New(),
	}
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
func (i *generator) GetIntermediateCodeUnit() ic.IntermediateCodeUnit {
	return i.intermediateCode
}

// Generate code for a block, all nested procedure blocks, and its statement.
func (i *generator) VisitBlock(bn *ast.BlockNode) {
	// only the main block has no parent procedure declaration
	if bn.ParentNode == nil {
		blockBegin := cor.EntryPointLabel

		// append a branch-target instruction with a branch-label to mark the beginning of the block
		instruction := ic.NewInstruction(
			ic.BranchTarget, // target for any branching operation
			noAddress,
			noAddress,
			ic.NewLiteralAddress(ic.String, blockBegin), // beginning of the block
			0)

		i.intermediateCode.AppendInstruction(instruction)
	} else {
		astSymbol := bn.Scope.Lookup(bn.ParentNode.(*ast.ProcedureDeclarationNode).Name)
		blockBegin := astSymbol.Extension[symbolExtension].(*symbolMetaData).name

		// append a branch-target instruction with a branch-label to mark the beginning of the block
		instruction := ic.NewInstruction(
			ic.BranchTarget, // target for any branching operation
			noAddress,
			noAddress,
			ic.NewLiteralAddress(ic.String, blockBegin), // beginning of the block
			0)

		element := i.intermediateCode.AppendInstruction((instruction))

		// update intermediate code function symbol with the instruction that marks the beginning of the block
		codeSymbol := i.intermediateCode.Lookup(blockBegin)
		codeSymbol.Definition = element
	}

	// create entry sequence for the block
	i.intermediateCode.AppendInstruction(ic.NewInstruction(ic.Prologue, noAddress, noAddress, noAddress, 0))

	// create a hidden first local variable for the block that holds internal data structures
	i.intermediateCode.AppendInstruction(ic.NewInstruction(
		ic.AllocateVariable, // allocate memory for the static link variable
		noAddress,
		noAddress,
		ic.NewVariableAddress(ic.Unsigned64, staticLinkDisplayName),
		0))

	// all declarations except blocks of nested procedures
	for _, declaration := range bn.Declarations {
		if declaration.Type() != ast.ProcedureDeclarationType {
			declaration.Accept(i)
		}
	}

	// initialize logical memory space and internal data structures for the block
	i.intermediateCode.AppendInstruction(ic.NewInstruction(
		ic.Setup, // setup of function call before the statement of the block is generated
		ic.NewLiteralAddress(ic.Integer32, bn.Depth), // block nesting depth
		noAddress,
		noAddress,
		0))

	// statement of the block
	bn.Statement.Accept(i)

	// create exit sequence for the block
	i.intermediateCode.AppendInstruction(ic.NewInstruction(ic.Epilogue, noAddress, noAddress, noAddress, 0))

	// only the main block has no parent procedure declaration
	if bn.ParentNode == nil {
		// return value of the main block, which is always 0
		returnValue := int32(0)

		// create a copy-literal instruction to store the return value in a virtual register
		copyLiteral := ic.NewInstruction(
			ic.CopyLiteral, // copy the value of the literal to a virtual register
			ic.NewLiteralAddress(ic.Integer32, returnValue), // literal value for the return value
			noAddress,
			ic.NewRegisterAddress(ic.Integer32, bn.Scope.NewIdentifier(prefix[registerPrefix])), // virtual register
			0)

		// return from the main block with a final result
		returnFromMain := ic.NewInstruction(
			ic.Return,
			ic.NewRegisterAddress(ic.Integer32, copyLiteral.Quadruple.Result.Name),
			noAddress,
			noAddress,
			0)

		// append the instructions to the intermediate code unit
		i.intermediateCode.AppendInstruction(copyLiteral)
		i.intermediateCode.AppendInstruction(returnFromMain)
	} else {
		// return from other blocks
		i.intermediateCode.AppendInstruction(ic.NewInstruction(ic.Return, noAddress, noAddress, noAddress, 0))
	}

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
	// determine the intermediate code name of the abstract syntax variable declaration
	codeName := vd.Scope.LookupCurrent(vd.Name).Extension[symbolExtension].(*symbolMetaData).name

	// get the intermediate code symbol table entry of the abstract syntax variable declaration
	codeSymbol := i.intermediateCode.Lookup(codeName)

	// allocate memory for the variable in its logical memory space
	instruction := ic.NewInstruction(
		ic.AllocateVariable, // allocate memory for the variable
		noAddress,
		noAddress,
		ic.NewVariableAddress(codeSymbol.DataType, codeSymbol.Name), // variable with flat unique name
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
	// create a copy-literal instruction to store the literal in a virtual register
	instruction := ic.NewInstruction(
		ic.CopyLiteral, // copy the value of the literal to a virtual register
		ic.NewLiteralAddress(dataTypeMap[ln.DataType], ln.Value), // literal value
		noAddress,
		ic.NewRegisterAddress(dataTypeMap[ln.DataType], ln.Scope.NewIdentifier(prefix[registerPrefix])), // virtual register
		ln.TokenStreamIndex) // literal use in the token stream

	// push the virtual register onto the results-list and append the instruction to the intermediate code unit
	i.pushResult(instruction.Quadruple.Result)
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

		// create a copy-literal instruction to store the constant value in a virtual register
		instruction := ic.NewInstruction(
			ic.CopyLiteral, // copy the value of the constant to a virtual register
			ic.NewLiteralAddress(dataTypeMap[constantDeclaration.DataType], constantDeclaration.Value), // literal value
			noAddress,
			ic.NewRegisterAddress(codeSymbol.DataType, iu.Scope.NewIdentifier(prefix[registerPrefix])), // virtual register
			iu.TokenStreamIndex) // constant use in the token stream

		// push the virtual register onto the results-list and append the instruction to the intermediate code unit
		i.pushResult(instruction.Quadruple.Result)
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

		// block nesting depth difference between variable use and variable declaration
		depthDifference := useDepth - declarationDepth

		// create a load-variable instruction to load the variable value into a virtual register
		instruction := ic.NewInstruction(
			ic.LoadVariable, // load the value of the variable into a virtual register
			ic.NewVariableAddress(codeSymbol.DataType, codeSymbol.Name),                                // variable with flat unique name
			ic.NewLiteralAddress(ic.Integer32, depthDifference),                                        // block nesting depth difference
			ic.NewRegisterAddress(codeSymbol.DataType, iu.Scope.NewIdentifier(prefix[registerPrefix])), // virtual register
			iu.TokenStreamIndex) // variable use in the token stream

		// push the virtual register onto the results-list and append the instruction to the intermediate code unit
		i.pushResult(instruction.Quadruple.Result)
		i.intermediateCode.AppendInstruction(instruction)

	case ast.Procedure:
		// not required for code generation

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, invalidContextInIdentifierUse, nil, nil))
	}
}

// Generate code for a unary operation.
func (i *generator) VisitUnaryOperation(uo *ast.UnaryOperationNode) {
	// load the result of the expression from the results-list
	uo.Operand.Accept(i)
	result := i.popResult()

	// perform the unary operation on the result
	switch uo.Operation {
	case ast.Odd:
		// create an odd instruction to check if the result is odd
		instruction := ic.NewInstruction(
			ic.Odd,
			result, // consumed result
			noAddress,
			noAddress,           // consumed result is checked in-place, boolean result must be hold externally
			uo.TokenStreamIndex) // unary operation in the token stream

		// append the instruction to the intermediate code unit (boolean results are not stored on the results-list)
		i.intermediateCode.AppendInstruction(instruction)

	case ast.Negate:
		// create a negate instruction to negate the result
		instruction := ic.NewInstruction(
			ic.Negate,
			result, // consumed result
			noAddress,
			result,              // consumed result is negated in-place (read, negate, write back negated result)
			uo.TokenStreamIndex) // unary operation in the token stream

		// push the result onto the results-list and append the instruction to the intermediate code unit
		i.pushResult(instruction.Quadruple.Result)
		i.intermediateCode.AppendInstruction(instruction)

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownUnaryOperation, nil, nil))
	}
}

// Generate code for a binary arithmetic operation.
func (i *generator) VisitBinaryOperation(bo *ast.BinaryOperationNode) {
	// determine block and its scope where the binary operation is located
	scope := ast.SearchBlock(ast.CurrentBlock, bo).Scope

	// load the results of the left and right expressions from the results-list
	bo.Left.Accept(i)
	bo.Right.Accept(i)
	right := i.popResult()
	left := i.popResult()

	// perform the binary arithmetic operation on the left- and right-hand-side results
	switch bo.Operation {
	case ast.Plus, ast.Minus, ast.Times, ast.Divide:
		var operation ic.Operation

		// map the AST binary operation to the corresponding three-address code binary arithmetic operation
		switch bo.Operation {
		case ast.Plus:
			operation = ic.Plus

		case ast.Minus:
			operation = ic.Minus

		case ast.Times:
			operation = ic.Times

		case ast.Divide:
			operation = ic.Divide
		}

		// create a binary arithmetic operation instruction to perform the operation on the left- and right-hand-side results
		instruction := ic.NewInstruction(
			operation,
			left,  // consumed left-hand-side result
			right, // consumed right-hand-side result
			ic.NewRegisterAddress(left.DataType, scope.NewIdentifier(prefix[registerPrefix])), // arithmetic operation result
			bo.TokenStreamIndex) // arithmetic operation in the token stream

		// push the result onto the results-list and append the instruction to the intermediate code unit
		i.pushResult(instruction.Quadruple.Result)
		i.intermediateCode.AppendInstruction(instruction)

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownBinaryOperation, nil, nil))
	}
}

// Generate code for a binary relational operation.
func (i *generator) VisitConditionalOperation(co *ast.ConditionalOperationNode) {
	// load the results of the left and right expressions from the results-list
	co.Left.Accept(i)
	co.Right.Accept(i)
	right := i.popResult()
	left := i.popResult()

	// perform the binary relational operation on the left- and right-hand-side results
	switch co.Operation {
	case ast.Equal, ast.NotEqual, ast.Less, ast.LessEqual, ast.Greater, ast.GreaterEqual:
		var operation ic.Operation

		// map the AST binary operation to the corresponding three-address code binary relational operation
		switch co.Operation {
		case ast.Equal:
			operation = ic.Equal

		case ast.NotEqual:
			operation = ic.NotEqual

		case ast.Less:
			operation = ic.Less

		case ast.LessEqual:
			operation = ic.LessEqual

		case ast.Greater:
			operation = ic.Greater

		case ast.GreaterEqual:
			operation = ic.GreaterEqual
		}

		// create a binary relational operation instruction to perform the operation on the left- and right-hand-side results
		instruction := ic.NewInstruction(
			operation,
			left,                // consumed left-hand-side result
			right,               // consumed right-hand-side result
			noAddress,           // consumed results are checked in-place, boolean result must be hold externally
			co.TokenStreamIndex) // conditional operation in the token stream

		// append the instruction to the intermediate code unit (boolean results are not stored on the results-list)
		i.intermediateCode.AppendInstruction(instruction)

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownConditionalOperation, nil, nil))
	}
}

// Generate code for an assignment statement.
func (i *generator) VisitAssignmentStatement(s *ast.AssignmentStatementNode) {
	// load the value from the result of the right-hand-side expression of the assignment
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

	// block nesting depth difference between variable use and variable declaration
	depthDifference := assignmentDepth - declarationDepth

	// store the resultant value from the right-hand-side expression in the variable on the left-hand-side of the assignment
	instruction := ic.NewInstruction(
		ic.StoreVariable, // store the value of the result into a variable
		right,            // consumed right-hand-side result
		ic.NewLiteralAddress(ic.Integer32, depthDifference),         // block nesting depth difference
		ic.NewVariableAddress(codeSymbol.DataType, codeSymbol.Name), // variable with flat unique name
		s.TokenStreamIndex) // assignment statement in the token stream

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

	// block nesting depth difference between variable use and variable declaration
	depthDifference := readDepth - declarationDepth

	// create a load-variable instruction to load the variable value into a virtual register
	load := ic.NewInstruction(
		ic.LoadVariable, // load the value of the variable into a virtual register
		ic.NewVariableAddress(codeSymbol.DataType, codeSymbol.Name),                             // variable with flat unique name
		ic.NewLiteralAddress(ic.Integer32, depthDifference),                                     // block nesting depth difference
		ic.NewRegisterAddress(codeSymbol.DataType, scope.NewIdentifier(prefix[registerPrefix])), // virtual register
		s.TokenStreamIndex) // read statement in the token stream

	// parameter 1 for the readln standard function
	param := ic.NewInstruction(
		ic.Parameter,          // parameter for a standard function
		load.Quadruple.Result, // result will be replaced by the standard function resultant value
		noAddress,
		noAddress,
		s.TokenStreamIndex) // read statement in the token stream

	// call the readln standard function with 1 parameter
	// readln := ic.NewInstruction(
	// 	ic.Standard, // function call to the external standard library
	// 	ic.NewAddress(1, ic.Count, ic.Unsigned64),       // number of parameters for the standard function
	// 	ic.NewAddress(ic.ReadLn, ic.Code, ic.Integer64), // code of standard function to call
	// 	noAddress,
	// 	s.TokenStreamIndex) // read statement in the token stream

	// get the intermediate code symbol table entry of the abstract syntax variable declaration
	codeSymbol = i.intermediateCode.Lookup(codeName)

	// store the resultant value into the variable used by the read statement
	store := ic.NewInstruction(
		ic.StoreVariable,     // store the value of the standard function result into a variable
		param.Quadruple.Arg1, // standard function resultant value
		ic.NewLiteralAddress(ic.Integer32, depthDifference),         // block nesting depth difference
		ic.NewVariableAddress(codeSymbol.DataType, codeSymbol.Name), // variable with flat unique name
		s.TokenStreamIndex) // read statement in the token stream

	// append the instructions to the intermediate code unit
	i.intermediateCode.AppendInstruction(load)
	i.intermediateCode.AppendInstruction(param)
	// i.intermediateCode.AppendInstruction(readln)
	i.intermediateCode.AppendInstruction(store)
}

// Generate code for a write statement.
func (i *generator) VisitWriteStatement(s *ast.WriteStatementNode) {
	// load the value from the result of the expression on the right-hand-side of the write statement
	s.Expression.Accept(i)
	right := i.popResult()

	// parameter 1 for the writeln standard function
	param := ic.NewInstruction(
		ic.Parameter, // parameter for a standard function
		right,        // consumed right-hand-side result
		noAddress,
		noAddress,
		s.TokenStreamIndex) // write statement in the token stream

	// call the writeln standard function with 1 parameter
	// writeln := ic.NewInstruction(
	// 	ic.Standard, // function call to the external standard library
	// 	ic.NewAddress(1, ic.Count, ic.Unsigned64),        // number of parameters for the standard function
	// 	ic.NewAddress(ic.WriteLn, ic.Code, ic.Integer64), // code of standard function to call
	// 	noAddress,
	// 	s.TokenStreamIndex) // write statement in the token stream

	// append the instructions to the intermediate code unit
	i.intermediateCode.AppendInstruction(param)
	// i.intermediateCode.AppendInstruction(writeln)
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

	// block nesting depth difference between procedure call and procedure declaration
	depthDifference := callDepth - declarationDepth

	// call the intermediate code function
	call := ic.NewInstruction(
		ic.Call, // call to an intermediate code function
		ic.NewLiteralAddress(ic.String, codeName),           // label of intermediate code function to call
		ic.NewLiteralAddress(ic.Integer32, depthDifference), // block nesting depth difference
		noAddress,
		s.TokenStreamIndex) // call statement in the token stream

	// append the instruction to the intermediate code unit
	i.intermediateCode.AppendInstruction(call)
}

// Generate code for an if-then statement.
func (i *generator) VisitIfStatement(s *ast.IfStatementNode) {
	// determine block and its scope where the if-then statement is located
	scope := ast.SearchBlock(ast.CurrentBlock, s).Scope
	behindStatement := scope.NewIdentifier(prefix[labelPrefix])

	// calculate the result of the condition expression
	s.Condition.Accept(i)

	// jump behind the statement if the condition is false
	i.jumpConditional(s.Condition, false, behindStatement)

	// execute statement if the condition is true
	s.Statement.Accept(i)

	// append a branch-target instruction behind the statement instructions
	i.intermediateCode.AppendInstruction(ic.NewInstruction(
		ic.BranchTarget, // target for any branching operation
		noAddress,
		noAddress,
		ic.NewLiteralAddress(ic.String, behindStatement), // behind if-then statement
		s.TokenStreamIndex)) // if-then statement in the token stream
}

// Generate code for a while-do statement.
func (i *generator) VisitWhileStatement(s *ast.WhileStatementNode) {
	// determine block and its scope where the while-do statement is located
	scope := ast.SearchBlock(ast.CurrentBlock, s).Scope
	beforeCondition := scope.NewIdentifier(prefix[labelPrefix])
	behindStatement := scope.NewIdentifier(prefix[labelPrefix])

	// append a branch-target instruction before the conditional expression instructions
	i.intermediateCode.AppendInstruction(ic.NewInstruction(
		ic.BranchTarget, // target for any branching operation
		noAddress,
		noAddress,
		ic.NewLiteralAddress(ic.String, beforeCondition), // before while-do conditional expression
		s.TokenStreamIndex)) // before while-do statement conditional expression in the token stream

	// calculate the result of the conditional expression
	s.Condition.Accept(i)

	// jump behind the statement if the condition is false
	i.jumpConditional(s.Condition, false, behindStatement)

	// execute statement if the condition is true
	s.Statement.Accept(i)

	// append a jump instruction to jump back to the conditional expression instructions
	i.intermediateCode.AppendInstruction(ic.NewInstruction(
		ic.Jump, // jump back to the conditional expression
		ic.NewLiteralAddress(ic.String, beforeCondition), // before while-do conditional expression
		noAddress,
		noAddress,
		s.TokenStreamIndex)) // end of while-do statement in the token stream

	// append a branch-target instruction behind the statement instructions
	i.intermediateCode.AppendInstruction(ic.NewInstruction(
		ic.BranchTarget, // target for any branching operation
		noAddress,
		noAddress,
		ic.NewLiteralAddress(ic.String, behindStatement), // behind while-do statement
		s.TokenStreamIndex)) // behind while-do statement in the token stream
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
	var jump *ic.Instruction
	address := ic.NewLiteralAddress(ic.String, label)

	// odd operation or conditional operations are valid for conditional jumps
	switch condition := expression.(type) {
	// unary operation node with the odd operation
	case *ast.UnaryOperationNode:
		if condition.Operation == ast.Odd {
			if jumpIfCondition {
				jump = ic.NewInstruction(ic.JumpNotEqual, address, noAddress, noAddress, condition.TokenStreamIndex)
			} else {
				jump = ic.NewInstruction(ic.JumpEqual, address, noAddress, noAddress, condition.TokenStreamIndex)
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
				jump = ic.NewInstruction(ic.JumpEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.NotEqual:
				jump = ic.NewInstruction(ic.JumpNotEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.Less:
				jump = ic.NewInstruction(ic.JumpLess, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.LessEqual:
				jump = ic.NewInstruction(ic.JumpLessEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.Greater:
				jump = ic.NewInstruction(ic.JumpGreater, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.GreaterEqual:
				jump = ic.NewInstruction(ic.JumpGreaterEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			default:
				panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownConditionalOperation, nil, nil))
			}
		} else {
			// jump if the condition is false
			switch condition.Operation {
			case ast.Equal:
				jump = ic.NewInstruction(ic.JumpNotEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.NotEqual:
				jump = ic.NewInstruction(ic.JumpEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.Less:
				jump = ic.NewInstruction(ic.JumpGreaterEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.LessEqual:
				jump = ic.NewInstruction(ic.JumpGreater, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.Greater:
				jump = ic.NewInstruction(ic.JumpLessEqual, address, noAddress, noAddress, condition.TokenStreamIndex)

			case ast.GreaterEqual:
				jump = ic.NewInstruction(ic.JumpLess, address, noAddress, noAddress, condition.TokenStreamIndex)

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

// Push an expression result onto the results-list.
func (i *generator) pushResult(result *ic.Address) {
	i.results.PushBack(result)
}

// Pop an expression result from the results-list.
func (i *generator) popResult() *ic.Address {
	result := i.results.Back()

	if result == nil {
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unexpectedIntermediateCodeResult, nil, nil))
	}

	i.results.Remove(result)
	return result.Value.(*ic.Address)
}

// Configure abstract syntax extensions and fill the symbol table of the intermediate code unit. This is a visit function.
func configureSymbols(node ast.Node, code any) {
	unit := code.(*generator).intermediateCode

	switch n := node.(type) {
	case *ast.ConstantDeclarationNode:
		name := n.Scope.NewIdentifier(prefix[constantPrefix])
		n.Scope.LookupCurrent(n.Name).Extension[symbolExtension] = newSymbolMetaData(name)
		unit.Insert(ic.NewSymbol(name, ic.ConstantSymbol, dataTypeMap[n.DataType]))

		if !dataTypeMap[n.DataType].IsSupported() {
			panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unsupportedDataTypeInConstantDeclaration, n, nil))
		}

	case *ast.VariableDeclarationNode:
		name := n.Scope.NewIdentifier(prefix[variablePrefix])
		n.Scope.LookupCurrent(n.Name).Extension[symbolExtension] = newSymbolMetaData(name)
		unit.Insert(ic.NewSymbol(name, ic.VariableSymbol, dataTypeMap[n.DataType]))

		if !dataTypeMap[n.DataType].IsSupported() {
			panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unsupportedDataTypeInVariableDeclaration, n, nil))
		}

	case *ast.ProcedureDeclarationNode:
		name := n.Block.(*ast.BlockNode).Scope.NewIdentifier(prefix[functionPrefix])
		n.Scope.LookupCurrent(n.Name).Extension[symbolExtension] = newSymbolMetaData(name)
		unit.Insert(ic.NewSymbol(name, ic.FunctionSymbol, ic.Untyped))
	}
}
