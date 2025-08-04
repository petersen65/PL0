// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package generator

import (
	"container/list"

	ast "github.com/petersen65/PL0/v2/ast"
	cor "github.com/petersen65/PL0/v2/core"
	ic "github.com/petersen65/PL0/v2/generator/intermediate"
)

// Abstract syntax extension for the symbol.
const symbolExtension ast.ExtensionType = 16

// Prefixes for address names in the three-address code concept.
const (
	labelPrefix     prefixType = iota // the label prefix is used for branch target labels or literal data labels
	temporaryPrefix                   // the temporary prefix is used for flattened temporary names in the intermediate code
	constantPrefix                    // the constant prefix is used for flattened constant names in the intermediate code
	variablePrefix                    // the variable prefix is used for flattened variable names in the intermediate code
	functionPrefix                    // the function prefix is used for flattened function names in the intermediate code
)

// Standard library and runtime library symbols used in the intermediate code generation.
const (
	staticLinkSymbol     = "@staticlink" // static link symbol holds a pointer to the lexical parent block
	readStatementSymbol  = "@read"       // read statement symbol is used to call the read function from the standard library
	writeStatementSymbol = "@write"      // write statement symbol is used to call the write function from the standard library
)

type (
	// Enumeration of prefixes used for names of addresses.
	prefixType int

	// Intermediate code generation compiler phase. It implements the Visitor interface to traverse the AST and generates code.
	generator struct {
		abstractSyntax   ast.Block               // abstract syntax tree to generate intermediate code for
		intermediateCode ic.IntermediateCodeUnit // intermediate code unit to store the generated intermediate code
		debugStringTable *cor.DebugStringTable   // collect debug information that requires knowledge of the abstract syntax tree
		results          *list.List              // last-in-first-out results-list holding results from expressions
	}

	// Metadata for each symbol in the abstract syntax tree.
	symbolMetaData struct {
		name string // intermediate code flattened name created from a scoped abstract syntax symbol
	}
)

var (
	// Map abstract syntax data types to intermediate code data types (intentially their type systems are kept separate).
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
		labelPrefix:     'l',
		temporaryPrefix: 't',
		constantPrefix:  'c',
		variablePrefix:  'v',
		functionPrefix:  'f',
	}

	// NoAddress represents an unused address in the three-address code concept.
	noAddress = &ic.Address{Variant: ic.Empty, DataType: ic.Untyped}
)

// Create a new intermediate code generator.
func newGenerator(abstractSyntax ast.Block) Generator {
	return &generator{
		abstractSyntax:   abstractSyntax,
		intermediateCode: ic.NewIntermediateCodeUnit(),
		debugStringTable: &cor.DebugStringTable{Functions: make([]cor.FunctionDescription, 0)},
		results:          list.New(),
	}
}

// Create metadata for a symbol in the abstract syntax tree.
func newSymbolMetaData(name string) *symbolMetaData {
	return &symbolMetaData{name: name}
}

// Generate intermediate code for the abstract syntax tree.
// The generator itself is performing a top down, left to right, and leftmost derivation walk on the abstract syntax tree.
func (g *generator) Generate() {
	// pre-create symbol table for intermediate code by providing a visit function that is called for each node
	if err := ast.Walk(g.abstractSyntax, ast.PreOrder, g, configureSymbols); err != nil {
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, intermediateCodeGenerationFailed, nil, err))
	}

	// generate intermediate code for the abstract syntax tree by using the visitor pattern of the abstract syntax tree
	g.abstractSyntax.Accept(g)
}

// Get access to the generated intermediate code.
func (g *generator) GetIntermediateCodeUnit() ic.IntermediateCodeUnit {
	return g.intermediateCode
}

// Generate code for a block, all nested procedure blocks, and its statement.
func (g *generator) VisitBlock(bn *ast.BlockNode) {
	// branch target label for the beginning of the block
	var blockBegin string

	// only the main block has no parent procedure declaration
	if bn.ParentNode == nil {
		// take the entry point label as the branch target label
		blockBegin = cor.EntryPointLabel

		// append a branch-target instruction with a branch-label to mark the beginning of the block
		g.intermediateCode.AppendInstruction(
			ic.BranchTarget, // target for any branching operation
			noAddress,
			noAddress,
			ic.NewLiteralAddress(ic.String, blockBegin), // branch target label
			bn.Index()) // block node in the token stream (first declaration or statement in the block)
	} else {
		// take the flattened name of the procedure declaration as the branch target label
		astSymbol := bn.Scope.Lookup(bn.ParentNode.(*ast.ProcedureDeclarationNode).Name)
		blockBegin = astSymbol.Extension[symbolExtension].(*symbolMetaData).name

		// append a branch-target instruction with a branch-label to mark the beginning of the block
		element := g.intermediateCode.AppendInstruction(
			ic.BranchTarget, // target for any branching operation
			noAddress,
			noAddress,
			ic.NewLiteralAddress(ic.String, blockBegin), // branch target label
			bn.Index()) // block node in the token stream (first declaration or statement in the block)

		// update intermediate code function symbol with the instruction that marks the beginning of the block
		codeSymbol := g.intermediateCode.Lookup(blockBegin)
		codeSymbol.Definition = element
	}

	// create entry sequence for the block
	g.intermediateCode.AppendInstruction(
		ic.Prologue, // function entry sequence
		ic.NewLiteralAddress(ic.String, blockBegin), // branch target label
		noAddress,
		noAddress,
		bn.Index()) // block node in the token stream (first declaration or statement in the block)

	// create a hidden first local variable for the block that holds internal data structures
	g.intermediateCode.AppendInstruction(
		ic.AllocateVariable, // allocate memory for the static link variable
		noAddress,
		noAddress,
		ic.NewVariableAddress(ic.Unsigned64, staticLinkSymbol),
		bn.Index()) // block node in the token stream (first declaration or statement in the block)

	// all declarations except blocks of nested procedures
	for _, declaration := range bn.Declarations {
		if declaration.Type() != ast.ProcedureDeclarationType {
			declaration.Accept(g)
		}
	}

	// initialize logical memory space and internal data structures for the block
	g.intermediateCode.AppendInstruction(
		ic.Setup, // setup of function call before the statement of the block is generated
		ic.NewLiteralAddress(ic.Integer32, bn.Depth), // block nesting depth
		noAddress,
		noAddress,
		bn.Statement.Index()) // block node in the token stream (first statement in the block)

	// statement of the block
	bn.Statement.Accept(g)

	// determine the token stream index after the last statement of the block
	indexEnd := cor.NoTokenStreamIndex

	if bn.Statement.Type() == ast.CompoundStatementType {
		indexEnd = bn.Statement.(*ast.CompoundStatementNode).TokenStreamIndexEnd
	}

	// create exit sequence for the block
	g.intermediateCode.AppendInstruction(
		ic.Epilogue,
		noAddress,
		noAddress,
		noAddress,
		indexEnd) // block node in the token stream (only applicable if the statement is a compound statement)

	// only the main block has no parent procedure declaration
	if bn.ParentNode == nil {
		// return value of the main block, which is always 0
		returnValue := int64(0)

		// create a copy-literal instruction to store the return value in a temporary
		copyLiteral := ic.NewInstruction(
			ic.CopyLiteral, // copy the value of the literal to a temporary
			ic.NewLiteralAddress(ic.Integer64, returnValue),                                       // literal value for the return value
			ic.NewLiteralAddress(ic.String, bn.Scope.NewIdentifier(prefix[labelPrefix])),          // new literal data label as source
			ic.NewTemporaryAddress(ic.Integer64, bn.Scope.NewIdentifier(prefix[temporaryPrefix])), // temporary as destination
			indexEnd) // block node in the token stream (only applicable if the statement is a compound statement)

		// return from the main block with a final result
		returnFromMain := ic.NewInstruction(
			ic.Return, // return from the main block
			ic.NewLiteralAddress(ic.String, blockBegin), // branch target label
			noAddress,
			copyLiteral.Quadruple.Result, // temporary with the return value
			indexEnd)                     // block node in the token stream (only applicable if the statement is a compound statement)

		// append the existing instructions to the intermediate code unit
		g.intermediateCode.AppendExistingInstruction(copyLiteral)
		g.intermediateCode.AppendExistingInstruction(returnFromMain)
	} else {
		// return from other blocks
		g.intermediateCode.AppendInstruction(
			ic.Return, // return from the block
			ic.NewLiteralAddress(ic.String, blockBegin), // branch target label
			noAddress,
			noAddress, // no return value
			indexEnd)  // block node in the token stream (only applicable if the statement is a compound statement)
	}

	// all blocks of nested procedure declarations (makes a procedure declaration a top-level construct in intermediate code)
	for _, declaration := range bn.Declarations {
		if declaration.Type() == ast.ProcedureDeclarationType {
			declaration.Accept(g)
		}
	}
}

// Generate code for a constant declaration.
func (g *generator) VisitConstantDeclaration(declaration *ast.ConstantDeclarationNode) {
	// not required for code generation
}

// Generate code for a variable declaration.
func (g *generator) VisitVariableDeclaration(vd *ast.VariableDeclarationNode) {
	// determine the intermediate code name of the abstract syntax variable declaration
	codeName := vd.Scope.LookupCurrent(vd.Name).Extension[symbolExtension].(*symbolMetaData).name

	// get the intermediate code symbol table entry of the abstract syntax variable declaration
	codeSymbol := g.intermediateCode.Lookup(codeName)

	// append allocate instruction to the unit and set it as definition for the intermediate code variable
	codeSymbol.Definition = g.intermediateCode.AppendInstruction(
		ic.AllocateVariable, // allocate memory for the variable in its logical memory space
		noAddress,
		noAddress,
		ic.NewVariableAddress(codeSymbol.DataType, codeSymbol.Name), // variable with flat unique name
		vd.TokenStreamIndex) // variable declaration in the token stream
}

// Generate code for a procedure declaration.
func (g *generator) VisitProcedureDeclaration(pd *ast.ProcedureDeclarationNode) {
	// generate code for the block of the procedure
	pd.Block.Accept(g)
}

// Generate code for a literal.
func (g *generator) VisitLiteral(ln *ast.LiteralNode) {
	// create a copy-literal instruction to store the literal in a temporary
	instruction := ic.NewInstruction(
		ic.CopyLiteral, // copy the value of the literal to a temporary
		ic.NewLiteralAddress(dataTypeMap[ln.DataType], ln.Value),                                          // literal value
		ic.NewLiteralAddress(ic.String, ln.Scope.NewIdentifier(prefix[labelPrefix])),                      // new literal data label as source
		ic.NewTemporaryAddress(dataTypeMap[ln.DataType], ln.Scope.NewIdentifier(prefix[temporaryPrefix])), // temporary as destination
		ln.TokenStreamIndex) // literal use in the token stream

	// push the temporary onto the results-list and append the instruction to the intermediate code unit
	g.pushResult(instruction.Quadruple.Result)
	g.intermediateCode.AppendExistingInstruction(instruction)
}

// Generate code for an identifier use.
func (g *generator) VisitIdentifierUse(iu *ast.IdentifierUseNode) {
	switch iu.Context {
	case ast.ConstantEntry:
		// get constant declaration of the constant to load
		constantDeclaration := iu.Scope.Lookup(iu.Name).Declaration.(*ast.ConstantDeclarationNode)

		// determine the intermediate code name of the abstract syntax constant declaration
		codeName := iu.Scope.Lookup(iu.Name).Extension[symbolExtension].(*symbolMetaData).name

		// get the intermediate code symbol table entry of the abstract syntax constant declaration
		codeSymbol := g.intermediateCode.Lookup(codeName)

		// create a copy-literal instruction to store the constant value in a temporary
		instruction := ic.NewInstruction(
			ic.CopyLiteral, // copy the value of the constant to a temporary
			ic.NewLiteralAddress(dataTypeMap[constantDeclaration.DataType], constantDeclaration.Value),   // literal value
			ic.NewLiteralAddress(ic.String, iu.Scope.NewIdentifier(prefix[labelPrefix])),                 // new literal data label as source
			ic.NewTemporaryAddress(codeSymbol.DataType, iu.Scope.NewIdentifier(prefix[temporaryPrefix])), // temporary as destination
			iu.TokenStreamIndex) // constant use in the token stream

		// push the temporary onto the results-list and append the instruction to the intermediate code unit
		g.pushResult(instruction.Quadruple.Result)
		g.intermediateCode.AppendExistingInstruction(instruction)

	case ast.VariableEntry:
		// get variable declaration of the variable to load
		variableDeclaration := iu.Scope.Lookup(iu.Name).Declaration.(*ast.VariableDeclarationNode)

		// determine the block nesting depth of the variable declaration
		declarationDepth := ast.SearchBlock(ast.CurrentBlock, variableDeclaration).Depth

		// determine the block nesting depth of the variable use from inside an expression or statement
		useDepth := ast.SearchBlock(ast.CurrentBlock, iu).Depth

		// determine the intermediate code name of the abstract syntax variable declaration
		codeName := iu.Scope.Lookup(iu.Name).Extension[symbolExtension].(*symbolMetaData).name

		// get the intermediate code symbol table entry of the abstract syntax variable declaration
		codeSymbol := g.intermediateCode.Lookup(codeName)

		// block nesting depth difference between variable use and variable declaration
		depthDifference := useDepth - declarationDepth

		// create a load-variable instruction to load the variable value into a temporary
		instruction := ic.NewInstruction(
			ic.LoadVariable, // load the value of the variable into a temporary
			ic.NewVariableAddress(codeSymbol.DataType, codeSymbol.Name),                                  // variable with flat unique name
			ic.NewLiteralAddress(ic.Integer32, depthDifference),                                          // block nesting depth difference
			ic.NewTemporaryAddress(codeSymbol.DataType, iu.Scope.NewIdentifier(prefix[temporaryPrefix])), // the resulting temporary
			iu.TokenStreamIndex) // variable use in the token stream

		// push the temporary onto the results-list and append the instruction to the intermediate code unit
		g.pushResult(instruction.Quadruple.Result)
		g.intermediateCode.AppendExistingInstruction(instruction)

	case ast.ProcedureEntry:
		// not required for code generation

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, invalidContextInIdentifierUse, nil, nil))
	}
}

// Generate code for a unary operation.
func (g *generator) VisitUnaryOperation(uo *ast.UnaryOperationNode) {
	// load the result of the expression from the results-list
	uo.Operand.Accept(g)
	result := g.popResult()

	// perform the unary operation on the result
	switch uo.Operation {
	case ast.Odd:
		// append the instruction to the intermediate code unit (boolean results are not stored on the results-list)
		g.intermediateCode.AppendInstruction(
			ic.Odd, // create an odd instruction to check if the result is odd
			result, // consumed result
			noAddress,
			noAddress,           // consumed result is checked in-place, boolean result must be hold externally
			uo.TokenStreamIndex) // unary operation in the token stream

	case ast.Negate:
		// create a negate instruction to negate the result
		instruction := ic.NewInstruction(
			ic.Negate,
			result, // consumed result
			noAddress,
			result,              // consumed result is negated in-place (read, negate, write back negated result)
			uo.TokenStreamIndex) // unary operation in the token stream

		// push the result onto the results-list and append the instruction to the intermediate code unit
		g.pushResult(instruction.Quadruple.Result)
		g.intermediateCode.AppendExistingInstruction(instruction)

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownUnaryOperation, nil, nil))
	}
}

// Generate code for a binary arithmetic operation.
func (g *generator) VisitBinaryOperation(bo *ast.BinaryOperationNode) {
	// determine block and its scope where the binary operation is located
	scope := ast.SearchBlock(ast.CurrentBlock, bo).Scope

	// load the results of the left and right expressions from the results-list
	bo.Left.Accept(g)
	bo.Right.Accept(g)
	right := g.popResult()
	left := g.popResult()

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
			ic.NewTemporaryAddress(left.DataType, scope.NewIdentifier(prefix[temporaryPrefix])), // arithmetic operation result
			bo.TokenStreamIndex) // arithmetic operation in the token stream

		// push the result onto the results-list and append the instruction to the intermediate code unit
		g.pushResult(instruction.Quadruple.Result)
		g.intermediateCode.AppendExistingInstruction(instruction)

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownBinaryOperation, nil, nil))
	}
}

// Generate code for a binary comparison operation.
func (g *generator) VisitComparisonOperation(co *ast.ComparisonOperationNode) {
	// load the results of the left and right expressions from the results-list
	co.Left.Accept(g)
	co.Right.Accept(g)
	right := g.popResult()
	left := g.popResult()

	// perform the binary comparison operation on the left- and right-hand-side results
	switch co.Operation {
	case ast.Equal, ast.NotEqual, ast.Less, ast.LessEqual, ast.Greater, ast.GreaterEqual:
		var operation ic.Operation

		// map the AST binary operation to the corresponding three-address code binary comparison operation
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

		// append the instruction to the intermediate code unit (boolean results are not stored on the results-list)
		g.intermediateCode.AppendInstruction(
			operation,           // create a binary comparison operation instruction to perform the operation on the left- and right-hand-side results
			left,                // consumed left-hand-side result
			right,               // consumed right-hand-side result
			noAddress,           // consumed results are checked in-place, boolean result must be hold externally
			co.TokenStreamIndex) // comparison operation in the token stream

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownComparisonOperation, nil, nil))
	}
}

// Generate code for an assignment statement.
func (g *generator) VisitAssignmentStatement(s *ast.AssignmentStatementNode) {
	// load the value from the result of the right-hand-side expression of the assignment
	s.Expression.Accept(g)
	right := g.popResult()

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
	codeSymbol := g.intermediateCode.Lookup(codeName)

	// block nesting depth difference between variable use and variable declaration
	depthDifference := assignmentDepth - declarationDepth

	// append the instruction to the intermediate code unit
	g.intermediateCode.AppendInstruction(
		ic.StoreVariable, // store the resultant value from the right-hand-side expression in the variable on the left-hand-side of the assignment
		right,            // consumed right-hand-side result
		ic.NewLiteralAddress(ic.Integer32, depthDifference),         // block nesting depth difference
		ic.NewVariableAddress(codeSymbol.DataType, codeSymbol.Name), // variable with flat unique name
		s.TokenStreamIndex) // assignment statement in the token stream
}

// Generate code for a read statement.
func (g *generator) VisitReadStatement(s *ast.ReadStatementNode) {
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
	codeSymbol := g.intermediateCode.Lookup(codeName)

	// block nesting depth difference between variable use and variable declaration
	depthDifference := readDepth - declarationDepth

	// append the instruction to the intermediate code unit
	call := ic.NewInstruction(
		ic.Call, // call the read standard function with one return value
		ic.NewLiteralAddress(ic.String, readStatementSymbol),                                      // label of standard function to call
		ic.NewLiteralAddress(ic.Integer32, int32(0)),                                              // block nesting depth difference ignored for standard functions
		ic.NewTemporaryAddress(codeSymbol.DataType, scope.NewIdentifier(prefix[temporaryPrefix])), // the standard function result
		s.TokenStreamIndex) // call statement in the token stream

	// store the resultant value into the variable used by the read statement
	store := ic.NewInstruction(
		ic.StoreVariable,      // store the value of the standard function result into a variable
		call.Quadruple.Result, // standard function resultant value
		ic.NewLiteralAddress(ic.Integer32, depthDifference),         // block nesting depth difference
		ic.NewVariableAddress(codeSymbol.DataType, codeSymbol.Name), // variable with flat unique name
		s.TokenStreamIndex) // read statement in the token stream

	// append the instructions to the intermediate code unit
	g.intermediateCode.AppendExistingInstruction(call)
	g.intermediateCode.AppendExistingInstruction(store)
}

// Generate code for a write statement.
func (g *generator) VisitWriteStatement(s *ast.WriteStatementNode) {
	// load the value from the result of the expression on the right-hand-side of the write statement
	s.Expression.Accept(g)
	right := g.popResult()

	// append the instruction to the intermediate code unit
	g.intermediateCode.AppendInstruction(
		ic.Parameter, // one parameter for the write standard function
		right,        // consumed right-hand-side result
		noAddress,
		noAddress,
		s.TokenStreamIndex) // write statement in the token stream

	// append the instruction to the intermediate code unit
	g.intermediateCode.AppendInstruction(
		ic.Call, // call the write standard function with one parameter
		ic.NewLiteralAddress(ic.String, writeStatementSymbol), // label of standard function to call
		ic.NewLiteralAddress(ic.Integer32, int32(0)),          // block nesting depth difference ignored for standard functions
		noAddress,
		s.TokenStreamIndex) // call statement in the token stream
}

// Generate code for a call statement.
func (g *generator) VisitCallStatement(s *ast.CallStatementNode) {
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

	// append the instruction to the intermediate code unit
	g.intermediateCode.AppendInstruction(
		ic.Call, // call to an intermediate code function
		ic.NewLiteralAddress(ic.String, codeName),           // label of intermediate code function to call
		ic.NewLiteralAddress(ic.Integer32, depthDifference), // block nesting depth difference
		noAddress,
		s.TokenStreamIndex) // call statement in the token stream
}

// Generate code for an if-then statement.
func (g *generator) VisitIfStatement(s *ast.IfStatementNode) {
	// create a new branch target label in the scope of the block where the if-then statement is located
	scope := ast.SearchBlock(ast.CurrentBlock, s).Scope
	behindStatement := scope.NewIdentifier(prefix[labelPrefix])

	// calculate the result of the condition expression
	s.Condition.Accept(g)

	// jump behind the statement if the condition is false
	g.jumpConditional(s.Condition, false, behindStatement)

	// execute statement if the condition is true
	s.Statement.Accept(g)

	// append a branch-target instruction behind the statement instructions
	g.intermediateCode.AppendInstruction(
		ic.BranchTarget, // target for any branching operation
		noAddress,
		noAddress,
		ic.NewLiteralAddress(ic.String, behindStatement), // behind if-then statement
		s.TokenStreamIndex) // if-then statement in the token stream
}

// Generate code for a while-do statement.
func (g *generator) VisitWhileStatement(s *ast.WhileStatementNode) {
	// create two new branch target labels in the scope of the block where the while-do statement is located
	scope := ast.SearchBlock(ast.CurrentBlock, s).Scope
	beforeCondition := scope.NewIdentifier(prefix[labelPrefix])
	behindStatement := scope.NewIdentifier(prefix[labelPrefix])

	// append a branch-target instruction before the condition expression instructions
	g.intermediateCode.AppendInstruction(
		ic.BranchTarget, // target for any branching operation
		noAddress,
		noAddress,
		ic.NewLiteralAddress(ic.String, beforeCondition), // before while-do condition expression
		s.TokenStreamIndex) // before while-do statement condition expression in the token stream

	// calculate the result of the condition expression
	s.Condition.Accept(g)

	// jump behind the statement if the condition is false
	g.jumpConditional(s.Condition, false, behindStatement)

	// execute statement if the condition is true
	s.Statement.Accept(g)

	// append a jump instruction to jump back to the condition expression instructions
	g.intermediateCode.AppendInstruction(
		ic.Jump, // jump back to the condition expression
		noAddress,
		noAddress,
		ic.NewLiteralAddress(ic.String, beforeCondition), // before while-do condition expression
		s.TokenStreamIndex) // end of while-do statement in the token stream

	// append a branch-target instruction behind the statement instructions
	g.intermediateCode.AppendInstruction(
		ic.BranchTarget, // target for any branching operation
		noAddress,
		noAddress,
		ic.NewLiteralAddress(ic.String, behindStatement), // behind while-do statement
		s.TokenStreamIndex) // behind while-do statement in the token stream
}

// Generate code for a compound begin-end statement.
func (g *generator) VisitCompoundStatement(s *ast.CompoundStatementNode) {
	// generate code for all statements in the compound statement
	for _, statement := range s.Statements {
		statement.Accept(g)
	}
}

// Conditional jump instruction based on an expression that must be a unary or comparison operation node.
func (g *generator) jumpConditional(expression ast.Expression, jumpIfCondition bool, label string) {
	var jump *ic.Instruction
	address := ic.NewLiteralAddress(ic.String, label)

	// odd operation or comparison operations are valid for conditional jumps
	switch condition := expression.(type) {
	// unary operation node with the odd operation
	case *ast.UnaryOperationNode:
		if condition.Operation == ast.Odd {
			if jumpIfCondition {
				jump = ic.NewInstruction(ic.JumpNotEqual, noAddress, noAddress, address, condition.TokenStreamIndex)
			} else {
				jump = ic.NewInstruction(ic.JumpEqual, noAddress, noAddress, address, condition.TokenStreamIndex)
			}
		} else {
			panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownUnaryOperation, nil, nil))
		}

	// comparison operation node with the equal, not equal, less, less equal, greater, or greater equal operation
	case *ast.ComparisonOperationNode:
		if jumpIfCondition {
			// jump if the condition is true
			switch condition.Operation {
			case ast.Equal:
				jump = ic.NewInstruction(ic.JumpEqual, noAddress, noAddress, address, condition.TokenStreamIndex)

			case ast.NotEqual:
				jump = ic.NewInstruction(ic.JumpNotEqual, noAddress, noAddress, address, condition.TokenStreamIndex)

			case ast.Less:
				jump = ic.NewInstruction(ic.JumpLess, noAddress, noAddress, address, condition.TokenStreamIndex)

			case ast.LessEqual:
				jump = ic.NewInstruction(ic.JumpLessEqual, noAddress, noAddress, address, condition.TokenStreamIndex)

			case ast.Greater:
				jump = ic.NewInstruction(ic.JumpGreater, noAddress, noAddress, address, condition.TokenStreamIndex)

			case ast.GreaterEqual:
				jump = ic.NewInstruction(ic.JumpGreaterEqual, noAddress, noAddress, address, condition.TokenStreamIndex)

			default:
				panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownComparisonOperation, nil, nil))
			}
		} else {
			// jump if the condition is false
			switch condition.Operation {
			case ast.Equal:
				jump = ic.NewInstruction(ic.JumpNotEqual, noAddress, noAddress, address, condition.TokenStreamIndex)

			case ast.NotEqual:
				jump = ic.NewInstruction(ic.JumpEqual, noAddress, noAddress, address, condition.TokenStreamIndex)

			case ast.Less:
				jump = ic.NewInstruction(ic.JumpGreaterEqual, noAddress, noAddress, address, condition.TokenStreamIndex)

			case ast.LessEqual:
				jump = ic.NewInstruction(ic.JumpGreater, noAddress, noAddress, address, condition.TokenStreamIndex)

			case ast.Greater:
				jump = ic.NewInstruction(ic.JumpLessEqual, noAddress, noAddress, address, condition.TokenStreamIndex)

			case ast.GreaterEqual:
				jump = ic.NewInstruction(ic.JumpLess, noAddress, noAddress, address, condition.TokenStreamIndex)

			default:
				panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownComparisonOperation, nil, nil))
			}
		}

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownComparisonOperation, nil, nil))
	}

	// append the conditional jump instruction to the intermediate code unit
	g.intermediateCode.AppendExistingInstruction(jump)
}

// Push an expression result onto the results-list.
func (g *generator) pushResult(result *ic.Address) {
	g.results.PushBack(result)
}

// Pop an expression result from the results-list.
func (g *generator) popResult() *ic.Address {
	result := g.results.Back()

	if result == nil {
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unexpectedIntermediateCodeResult, nil, nil))
	}

	g.results.Remove(result)
	return result.Value.(*ic.Address)
}

// Configure abstract syntax extensions, create debug information, and fill the symbol table of the intermediate code unit.
// Note: this is a visit function.
func configureSymbols(node ast.Node, code any) {
	table := code.(*generator).debugStringTable
	unit := code.(*generator).intermediateCode
	functions := map[string]*cor.FunctionDescription{}

	switch n := node.(type) {
	case *ast.ConstantDeclarationNode:
		// create a new symbol with a flattened name in the intermediate code unit for the constant declaration
		name := n.Scope.NewIdentifier(prefix[constantPrefix])
		n.Scope.LookupCurrent(n.Name).Extension[symbolExtension] = newSymbolMetaData(name)
		unit.Insert(ic.NewSymbol(name, ic.ConstantEntry, dataTypeMap[n.DataType]))

		if !dataTypeMap[n.DataType].IsSupported() {
			panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unsupportedDataTypeInConstantDeclaration, n, nil))
		}

	case *ast.VariableDeclarationNode:
		// create a new symbol with a flattened name in the intermediate code unit for the variable declaration
		name := n.Scope.NewIdentifier(prefix[variablePrefix])
		n.Scope.LookupCurrent(n.Name).Extension[symbolExtension] = newSymbolMetaData(name)
		unit.Insert(ic.NewSymbol(name, ic.VariableEntry, dataTypeMap[n.DataType]))

		if !dataTypeMap[n.DataType].IsSupported() {
			panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unsupportedDataTypeInVariableDeclaration, n, nil))
		}

		// create a new variable description used to collect debug information
		variable := cor.VariableDescription{
			Name:             n.Name,
			TokenStreamIndex: n.Index(),
		}

		// determine the function name where the variable is declared
		if n.Parent().Parent() != nil {
			variable.Function = n.Parent().Parent().(*ast.ProcedureDeclarationNode).Name
		} else {
			variable.Function = cor.EntryPointLabel
		}

		// check if the function of the variable is already known and create its function description if not
		if function, ok := functions[variable.Function]; ok {
			function.Variables = append(function.Variables, variable)
		} else {
			functions[variable.Function] = &cor.FunctionDescription{
				Name:      variable.Function,
				Variables: []cor.VariableDescription{variable},
			}

			table.Functions = append(table.Functions, *functions[variable.Function])
		}

	case *ast.ProcedureDeclarationNode:
		// create a new symbol with a flattened name in the intermediate code unit for the procedure declaration
		name := n.Block.(*ast.BlockNode).Scope.NewIdentifier(prefix[functionPrefix])
		n.Scope.LookupCurrent(n.Name).Extension[symbolExtension] = newSymbolMetaData(name)
		unit.Insert(ic.NewSymbol(name, ic.FunctionEntry, ic.Untyped))
	}
}
