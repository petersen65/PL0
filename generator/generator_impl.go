// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package generator

import (
	"container/list"
	"path/filepath"
	"strings"

	ast "github.com/petersen65/pl0/v3/ast"
	dbg "github.com/petersen65/pl0/v3/debugging"
	eh "github.com/petersen65/pl0/v3/errors"
	ic "github.com/petersen65/pl0/v3/generator/intermediate"
	plt "github.com/petersen65/pl0/v3/platform"
	sym "github.com/petersen65/pl0/v3/symbol"
	tok "github.com/petersen65/pl0/v3/token"
	ts "github.com/petersen65/pl0/v3/typesystem"
)

// Intermediate code extension for each symbol from the abstract syntax tree.
const intermediateCodeExtension sym.ExtensionType = 16

// Generator extension for each symbol from the intermediate code unit.
const generatorExtension sym.ExtensionType = 32

// Prefixes for address names in the three-address code concept.
const (
	labelPrefix     prefixType = iota // the label prefix is used for branch target labels or literal data labels
	temporaryPrefix                   // the temporary prefix is used for flattened temporary names in the intermediate code
	constantPrefix                    // the constant prefix is used for flattened constant names in the intermediate code
	variablePrefix                    // the variable prefix is used for flattened variable names in the intermediate code
	functionPrefix                    // the function prefix is used for flattened function names in the intermediate code
)

// Standard library and runtime symbols used in the intermediate code generation.
const (
	staticLinkSymbol     = "@staticlink" // static link symbol holds a pointer to the lexical parent block
	readStatementSymbol  = "@read"       // read statement symbol is used to call the read function from the standard library
	writeStatementSymbol = "@write"      // write statement symbol is used to call the write function from the standard library
)

// String has to be represented as a composite data type with a length and a data pointer.
const (
	stringLengthMemberName = "length" // unsigned integer based length
	stringDataMemberName   = "data"   // pointer to UTF encoding-dependent string data
)

type (
	// Enumeration of prefixes used for names of addresses.
	prefixType int

	// Intermediate code generation compiler phase. It implements the Visitor interface to traverse the AST and generates code.
	generator struct {
		abstractSyntax   ast.Block               // abstract syntax tree to generate intermediate code for
		intermediateCode ic.IntermediateCodeUnit // intermediate code unit to store the generated intermediate code
		debugInformation dbg.DebugInformation    // debug information collected during the code generation
		results          *list.List              // last-in-first-out results-list holding results from expressions
	}

	// Metadata for each symbol in the abstract syntax tree.
	symbolMetaData struct {
		name string // intermediate code flattened name created from a scoped abstract syntax symbol
	}
)

var (
	// Map UTF string encodings to their type system string base data type.
	stringBaseTypeMap = map[plt.StringEncoding]ts.PrimitiveDataType{
		plt.UTF8:  ts.Unsigned8,
		plt.UTF16: ts.Unsigned16,
		plt.UTF32: ts.Character,
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
	noAddress = &ic.Address{Variant: ic.Empty}
)

// Create a new intermediate code generator.
func newGenerator(abstractSyntax ast.Block, buildConfiguration plt.BuildConfiguration, tokenHandler tok.TokenHandler) Generator {
	compilationUnit := buildConfiguration.SourcePath
	compilationDirectory := filepath.ToSlash(filepath.Clean(strings.TrimSuffix(buildConfiguration.SourceAbsolutePath, compilationUnit)))
	producer := buildConfiguration.DriverDisplayName
	optimized := buildConfiguration.Optimization&plt.Debug == 0
	debugInformation := dbg.NewDebugInformation(compilationUnit, compilationDirectory, producer, ts.String.String(), optimized, tokenHandler)

	// predefine the structure of the "string" composite data type and add it to debugging information
	if !optimized {
		appendStringDataType(buildConfiguration.TargetPlatform.StringEncoding, debugInformation)
	}

	return &generator{
		abstractSyntax:   abstractSyntax,
		intermediateCode: ic.NewIntermediateCodeUnit(),
		debugInformation: debugInformation,
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
	if err := ast.Walk(g.abstractSyntax, ast.PreOrder, g.intermediateCode, configureSymbols); err != nil {
		panic(eh.NewGeneralError(eh.Generator, failureMap, eh.Fatal, intermediateCodeGenerationFailed, nil, err))
	}

	// collect the debug string table by providing a visit function that is called for each node
	if err := ast.Walk(g.abstractSyntax, ast.PreOrder, g.debugInformation, collectDebugStringTable); err != nil {
		panic(eh.NewGeneralError(eh.Generator, failureMap, eh.Fatal, intermediateCodeGenerationFailed, nil, err))
	}

	// generate intermediate code for the abstract syntax tree by using the visitor pattern of the abstract syntax tree
	g.abstractSyntax.Accept(g)
}

// Get access to the generated intermediate code.
func (g *generator) GetIntermediateCodeUnit() ic.IntermediateCodeUnit {
	return g.intermediateCode
}

// Provide access to the collected debug information.
func (g *generator) GetDebugInformation() dbg.DebugInformation {
	return g.debugInformation
}

// Generate code for a block, all nested function blocks, and its statement.
func (g *generator) VisitBlock(b ast.Block) {
	// branch target label for the beginning of the block
	var blockBegin string

	// only the main block has no parent function declaration
	if b.IsRootBlock() {
		// take the entry point label as the branch target label
		blockBegin = plt.EntryPointLabel

		// append a branch-target instruction with a branch-label to mark the beginning of the block
		g.intermediateCode.AppendInstruction(
			ic.BranchTarget, // target for any branching operation
			noAddress,
			noAddress,
			ic.NewLiteralAddress(ts.String.String(), blockBegin), // branch target label
			b.Index()) // block node in the token stream (first declaration or statement in the block)
	} else {
		// take the flattened name of the function declaration as the branch target label
		astSymbol := b.Function().Symbol()
		blockBegin = getIntermediateCodeName(astSymbol)

		// append a branch-target instruction with a branch-label to mark the beginning of the block
		element := g.intermediateCode.AppendInstruction(
			ic.BranchTarget, // target for any branching operation
			noAddress,
			noAddress,
			ic.NewLiteralAddress(ts.String.String(), blockBegin), // branch target label
			b.Index()) // block node in the token stream (first declaration or statement in the block)

		// update intermediate code function symbol with the instruction that marks the beginning of the block
		codeSymbol := g.intermediateCode.Lookup(blockBegin)
		codeSymbol.Extension[generatorExtension] = element
	}

	// create entry sequence for the block
	g.intermediateCode.AppendInstruction(
		ic.Prologue, // function entry sequence
		ic.NewLiteralAddress(ts.String.String(), blockBegin), // branch target label
		noAddress,
		noAddress,
		b.Index()) // block node in the token stream (first declaration or statement in the block)

	// create a hidden first local variable for the block that holds internal data structures
	g.intermediateCode.AppendInstruction(
		ic.AllocateVariable, // allocate memory for the static link variable
		noAddress,
		noAddress,
		ic.NewVariableAddress(ts.Unsigned64.String(), staticLinkSymbol),
		b.Index()) // block node in the token stream (first declaration or statement in the block)

	// all declarations except blocks of nested function declarations
	for _, declaration := range b.Declarations() {
		if declaration.Kind() != ast.KindFunctionDeclaration {
			declaration.Accept(g)
		}
	}

	// initialize the memory space and internal data structures for the block
	g.intermediateCode.AppendInstruction(
		ic.Setup, // setup of function call before the statement of the block is generated
		ic.NewLiteralAddress(ts.Integer32.String(), b.Depth), // block nesting depth
		noAddress,
		noAddress,
		b.Statement().Index()) // block node in the token stream (first statement in the block)

	// statement of the block
	b.Statement().Accept(g)

	// determine the token stream index after the last statement of the block
	_, indexEnd := b.Statement().IndexPair()

	// create exit sequence for the block
	g.intermediateCode.AppendInstruction(
		ic.Epilogue,
		noAddress,
		noAddress,
		noAddress,
		indexEnd) // block node in the token stream (only applicable if the statement is a compound statement)

	// only the root block has no parent function declaration
	if b.IsRootBlock() {
		// return value of the main block, which is always 0
		returnValue := int64(0)

		// create a copy-literal instruction to store the return value in a temporary
		copyLiteral := ic.NewInstruction(
			ic.CopyLiteral, // copy the value of the literal to a temporary
			ic.NewLiteralAddress(ts.Integer64.String(), returnValue),                             // literal value for the return value
			ic.NewLiteralAddress(ts.String.String(), b.UniqueName(prefix[labelPrefix])),          // new literal data label as source
			ic.NewTemporaryAddress(ts.Integer64.String(), b.UniqueName(prefix[temporaryPrefix])), // temporary as destination
			indexEnd) // block node in the token stream (only applicable if the statement is a compound statement)

		// return from the main block with a final result
		returnFromMain := ic.NewInstruction(
			ic.Return, // return from the main block
			ic.NewLiteralAddress(ts.String.String(), blockBegin), // branch target label
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
			ic.NewLiteralAddress(ts.String.String(), blockBegin), // branch target label
			noAddress,
			noAddress, // no return value
			indexEnd)  // block node in the token stream (only applicable if the statement is a compound statement)
	}

	// all blocks of nested function declarations (makes any function declaration a top-level construct in intermediate code)
	for _, declaration := range b.Declarations() {
		if declaration.Kind() == ast.KindFunctionDeclaration {
			declaration.Accept(g)
		}
	}
}

// Generate code for a constant declaration.
func (g *generator) VisitConstantDeclaration(_ ast.ConstantDeclaration) {
	// not required for code generation
}

// Generate code for a variable declaration.
func (g *generator) VisitVariableDeclaration(vd ast.VariableDeclaration) {
	// determine the intermediate code name of the abstract syntax variable declaration
	codeName := getIntermediateCodeName(vd.Symbol())

	// get the intermediate code symbol table entry of the abstract syntax variable declaration
	codeSymbol := g.intermediateCode.Lookup(codeName)

	// append allocate instruction to the unit and set it as definition for the intermediate code variable
	codeSymbol.Extension[generatorExtension] = g.intermediateCode.AppendInstruction(
		ic.AllocateVariable, // allocate memory for the variable in its memory space
		noAddress,
		noAddress,
		ic.NewVariableAddress(codeSymbol.DataType.String(), codeSymbol.Name), // variable with flat unique name
		vd.Index()) // variable declaration in the token stream
}

// Generate code for a function declaration.
func (g *generator) VisitFunctionDeclaration(fd ast.FunctionDeclaration) {
	// generate code for the block of the function
	fd.Block().Accept(g)
}

// Generate code for a literal.
func (g *generator) VisitLiteralUse(ln ast.LiteralUse) {
	// determine block where the literal use is located
	cb := ln.CurrentBlock()

	// create a copy-literal instruction to store the literal in a temporary
	instruction := ic.NewInstruction(
		ic.CopyLiteral, // copy the value of the literal to a temporary
		ic.NewLiteralAddress(dataTypeMap[ln.DataType], ln.Value),                                 // literal value
		ic.NewLiteralAddress(ts.String.String(), cb.UniqueName(prefix[labelPrefix])),             // new literal data label as source
		ic.NewTemporaryAddress(dataTypeMap[ln.DataType], cb.UniqueName(prefix[temporaryPrefix])), // temporary as destination
		ln.Index()) // literal use in the token stream

	// push the temporary onto the results-list and append the instruction to the intermediate code unit
	g.pushResult(instruction.Quadruple.Result)
	g.intermediateCode.AppendExistingInstruction(instruction)
}

// Generate code for an identifier use.
func (g *generator) VisitIdentifierUse(iu ast.IdentifierUse) {
	switch iu.Context() {
	case ast.Constant:
		// determine block where the identifier use is located
		cb := iu.CurrentBlock()

		// get constant declaration of the constant to load
		constantDeclaration := iu.Declaration().(ast.ConstantDeclaration)

		// determine the intermediate code name of the abstract syntax constant declaration
		codeName := getIntermediateCodeName(constantDeclaration.Symbol())

		// get the intermediate code symbol table entry of the abstract syntax constant declaration
		codeSymbol := g.intermediateCode.Lookup(codeName)

		// create a copy-literal instruction to store the constant value in a temporary
		instruction := ic.NewInstruction(
			ic.CopyLiteral, // copy the value of the constant to a temporary
			ic.NewLiteralAddress(codeSymbol.DataType.String(), constantDeclaration.Value),                // literal value
			ic.NewLiteralAddress(ts.String.String(), cb.UniqueName(prefix[labelPrefix])),                 // new literal data label as source
			ic.NewTemporaryAddress(codeSymbol.DataType.String(), cb.UniqueName(prefix[temporaryPrefix])), // temporary as destination
			iu.Index()) // constant use in the token stream

		// push the temporary onto the results-list and append the instruction to the intermediate code unit
		g.pushResult(instruction.Quadruple.Result)
		g.intermediateCode.AppendExistingInstruction(instruction)

	case ast.Variable:
		// determine block where the identifier use is located
		cb := iu.CurrentBlock()

		// get variable declaration of the variable to load
		variableDeclaration := iu.Declaration().(ast.VariableDeclaration)

		// determine the block nesting depth of the variable declaration
		declarationDepth := variableDeclaration.Depth()

		// determine the block nesting depth of the variable use from inside an expression or statement
		useDepth := iu.Depth()

		// determine the intermediate code name of the abstract syntax variable declaration
		codeName := getIntermediateCodeName(variableDeclaration.Symbol())

		// get the intermediate code symbol table entry of the abstract syntax variable declaration
		codeSymbol := g.intermediateCode.Lookup(codeName)

		// block nesting depth difference between variable use and variable declaration
		depthDifference := useDepth - declarationDepth

		// create a load-variable instruction to load the variable value into a temporary
		instruction := ic.NewInstruction(
			ic.LoadVariable, // load the value of the variable into a temporary
			ic.NewVariableAddress(codeSymbol.DataType.String(), codeSymbol.Name),                         // variable with flat unique name
			ic.NewLiteralAddress(ts.Integer32.String(), depthDifference),                                 // block nesting depth difference
			ic.NewTemporaryAddress(codeSymbol.DataType.String(), cb.UniqueName(prefix[temporaryPrefix])), // the resulting temporary
			iu.Index()) // variable use in the token stream

		// push the temporary onto the results-list and append the instruction to the intermediate code unit
		g.pushResult(instruction.Quadruple.Result)
		g.intermediateCode.AppendExistingInstruction(instruction)

	case ast.Function, ast.Procedure:
		// not required for code generation

	default:
		panic(eh.NewGeneralError(eh.Generator, failureMap, eh.Fatal, invalidContextInIdentifierUse, nil, nil))
	}
}

// Generate code for a unary operation.
func (g *generator) VisitUnaryOperation(uo ast.UnaryOperation) {
	// load the result of the expression from the results-list
	uo.Operand().Accept(g)
	result := g.popResult()

	// perform the unary operation on the result
	switch uo.Operation() {
	case ast.Odd:
		// append the instruction to the intermediate code unit (boolean results are not stored on the results-list)
		g.intermediateCode.AppendInstruction(
			ic.Odd, // create an odd instruction to check if the result is odd
			result, // consumed result
			noAddress,
			noAddress,  // consumed result is checked in-place, boolean result must be hold externally
			uo.Index()) // unary operation in the token stream

	case ast.Negate:
		// create a negate instruction to negate the result
		instruction := ic.NewInstruction(
			ic.Negate,
			result, // consumed result
			noAddress,
			result,     // consumed result is negated in-place (read, negate, write back negated result)
			uo.Index()) // unary operation in the token stream

		// push the result onto the results-list and append the instruction to the intermediate code unit
		g.pushResult(instruction.Quadruple.Result)
		g.intermediateCode.AppendExistingInstruction(instruction)

	default:
		panic(eh.NewGeneralError(eh.Generator, failureMap, eh.Fatal, unknownUnaryOperation, nil, nil))
	}
}

// Generate code for an arithmetic operation.
func (g *generator) VisitArithmeticOperation(bo ast.ArithmeticOperation) {
	// determine block where the arithmetic operation is located
	cb := bo.CurrentBlock()

	// load the results of the left and right expressions from the results-list
	bo.Left().Accept(g)
	bo.Right().Accept(g)
	right := g.popResult()
	left := g.popResult()

	// perform the arithmetic operation on the left- and right-hand-side results
	switch bo.Operation() {
	case ast.Plus, ast.Minus, ast.Times, ast.Divide:
		var operation ic.Operation

		// map the AST arithmetic operation to the corresponding three-address code arithmetic operation
		switch bo.Operation() {
		case ast.Plus:
			operation = ic.Plus

		case ast.Minus:
			operation = ic.Minus

		case ast.Times:
			operation = ic.Times

		case ast.Divide:
			operation = ic.Divide
		}

		// create an arithmetic operation instruction to perform the operation on the left- and right-hand-side results
		instruction := ic.NewInstruction(
			operation,
			left,  // consumed left-hand-side result
			right, // consumed right-hand-side result
			ic.NewTemporaryAddress(left.DataType, cb.UniqueName(prefix[temporaryPrefix])), // arithmetic operation result
			bo.Index()) // arithmetic operation in the token stream

		// push the result onto the results-list and append the instruction to the intermediate code unit
		g.pushResult(instruction.Quadruple.Result)
		g.intermediateCode.AppendExistingInstruction(instruction)

	default:
		panic(eh.NewGeneralError(eh.Generator, failureMap, eh.Fatal, unknownArithmeticOperation, nil, nil))
	}
}

// Generate code for a comparison operation.
func (g *generator) VisitComparisonOperation(co ast.ComparisonOperation) {
	// load the results of the left and right expressions from the results-list
	co.Left().Accept(g)
	co.Right().Accept(g)
	right := g.popResult()
	left := g.popResult()

	// perform the comparison operation on the left- and right-hand-side results
	switch co.Operation() {
	case ast.Equal, ast.NotEqual, ast.Less, ast.LessEqual, ast.Greater, ast.GreaterEqual:
		var operation ic.Operation

		// map the AST comparison operation to the corresponding three-address code comparison operation
		switch co.Operation() {
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
			operation,  // create a comparison operation instruction to perform the operation on the left- and right-hand-side results
			left,       // consumed left-hand-side result
			right,      // consumed right-hand-side result
			noAddress,  // consumed results are checked in-place, boolean result must be hold externally
			co.Index()) // comparison operation in the token stream

	default:
		panic(eh.NewGeneralError(eh.Generator, failureMap, eh.Fatal, unknownComparisonOperation, nil, nil))
	}
}

// Generate code for an assignment statement.
func (g *generator) VisitAssignmentStatement(s ast.AssignmentStatement) {
	// load the value from the result of the right-hand-side expression of the assignment
	s.Expression().Accept(g)
	right := g.popResult()

	// get the variable declaration on the left-hand-side of the assignment
	variableUse := s.Variable()
	variableDeclaration := variableUse.Declaration().(ast.VariableDeclaration)

	// determine the block nesting depth of the variable declaration
	declarationDepth := variableDeclaration.Depth()

	// determine the block nesting depth of the assignment statement where the variable is used
	assignmentDepth := s.CurrentBlock().Depth()

	// determine the intermediate code name of the abstract syntax variable declaration
	codeName := getIntermediateCodeName(variableDeclaration.Symbol())

	// get the intermediate code symbol table entry of the abstract syntax variable declaration
	codeSymbol := g.intermediateCode.Lookup(codeName)

	// block nesting depth difference between variable use and variable declaration
	depthDifference := assignmentDepth - declarationDepth

	// append the instruction to the intermediate code unit
	g.intermediateCode.AppendInstruction(
		ic.StoreVariable, // store the resultant value from the right-hand-side expression in the variable on the left-hand-side of the assignment
		right,            // consumed right-hand-side result
		ic.NewLiteralAddress(ts.Integer32.String(), depthDifference),         // block nesting depth difference
		ic.NewVariableAddress(codeSymbol.DataType.String(), codeSymbol.Name), // variable with flat unique name
		s.Index()) // assignment statement in the token stream
}

// Generate code for a read statement.
func (g *generator) VisitReadStatement(s ast.ReadStatement) {
	// determine block where the read statement is located
	cb := s.CurrentBlock()

	// get the variable declaration of the variable to read into
	variableUse := s.Variable()
	variableDeclaration := variableUse.Declaration().(ast.VariableDeclaration)

	// determine the block nesting depth of the variable declaration
	declarationDepth := variableDeclaration.Depth()

	// determine the block nesting depth of the read statement where the variable is used
	readDepth := cb.Depth()

	// determine the intermediate code name of the abstract syntax variable declaration
	codeName := getIntermediateCodeName(variableDeclaration.Symbol())

	// get the intermediate code symbol table entry of the abstract syntax variable declaration
	codeSymbol := g.intermediateCode.Lookup(codeName)

	// block nesting depth difference between variable use and variable declaration
	depthDifference := readDepth - declarationDepth

	// append the instruction to the intermediate code unit
	call := ic.NewInstruction(
		ic.Call, // call the read standard function with one return value
		ic.NewLiteralAddress(ts.String.String(), readStatementSymbol),                                // label of standard function to call
		ic.NewLiteralAddress(ts.Integer32.String(), int32(0)),                                        // block nesting depth difference ignored for standard functions
		ic.NewTemporaryAddress(codeSymbol.DataType.String(), cb.UniqueName(prefix[temporaryPrefix])), // the standard function result
		s.Index()) // call statement in the token stream

	// store the resultant value into the variable used by the read statement
	store := ic.NewInstruction(
		ic.StoreVariable,      // store the value of the standard function result into a variable
		call.Quadruple.Result, // standard function resultant value
		ic.NewLiteralAddress(ts.Integer32.String(), depthDifference),         // block nesting depth difference
		ic.NewVariableAddress(codeSymbol.DataType.String(), codeSymbol.Name), // variable with flat unique name
		s.Index()) // read statement in the token stream

	// append the instructions to the intermediate code unit
	g.intermediateCode.AppendExistingInstruction(call)
	g.intermediateCode.AppendExistingInstruction(store)
}

// Generate code for a write statement.
func (g *generator) VisitWriteStatement(s ast.WriteStatement) {
	// load the value from the result of the expression on the right-hand-side of the write statement
	s.Expression().Accept(g)
	right := g.popResult()

	// append the instruction to the intermediate code unit
	g.intermediateCode.AppendInstruction(
		ic.Parameter, // one parameter for the write standard function
		right,        // consumed right-hand-side result
		noAddress,
		noAddress,
		s.Index()) // write statement in the token stream

	// append the instruction to the intermediate code unit
	g.intermediateCode.AppendInstruction(
		ic.Call, // call the write standard function with one parameter
		ic.NewLiteralAddress(ts.String.String(), writeStatementSymbol), // label of standard function to call
		ic.NewLiteralAddress(ts.Integer32.String(), int32(0)),          // block nesting depth difference ignored for standard functions
		noAddress,
		s.Index()) // call statement in the token stream
}

// Generate code for a call statement.
func (g *generator) VisitCallStatement(s ast.CallStatement) {
	// get the declaration of the procedure to call
	identifierUse := s.Function()
	procedureDeclaration := identifierUse.Declaration().(ast.FunctionDeclaration)

	// determine the block nesting depth of the procedure declaration
	declarationDepth := procedureDeclaration.Depth()

	// determine the block nesting depth of the call statement where the procedure is called
	callDepth := s.CurrentBlock().Depth()

	// determine the intermediate code name of the abstract syntax procedure declaration
	codeName := getIntermediateCodeName(procedureDeclaration.Symbol())

	// block nesting depth difference between procedure call and procedure declaration
	depthDifference := callDepth - declarationDepth

	// append the instruction to the intermediate code unit
	g.intermediateCode.AppendInstruction(
		ic.Call, // call to an intermediate code function
		ic.NewLiteralAddress(ts.String.String(), codeName),           // label of intermediate code function to call
		ic.NewLiteralAddress(ts.Integer32.String(), depthDifference), // block nesting depth difference
		noAddress,
		s.Index()) // call statement in the token stream
}

// Generate code for an if-then statement.
func (g *generator) VisitIfStatement(s ast.IfStatement) {
	// create a new branch target label in the block where the if-then statement is located
	behindStatement := s.CurrentBlock().UniqueName(prefix[labelPrefix])

	// calculate the result of the condition expression
	s.Condition().Accept(g)

	// jump behind the statement if the condition is false
	g.jumpConditional(s.Condition(), false, behindStatement)

	// execute statement if the condition is true
	s.Statement().Accept(g)

	// append a branch-target instruction behind the statement instructions
	g.intermediateCode.AppendInstruction(
		ic.BranchTarget, // target for any branching operation
		noAddress,
		noAddress,
		ic.NewLiteralAddress(ts.String.String(), behindStatement), // behind if-then statement
		s.Index()) // if-then statement in the token stream
}

// Generate code for a while-do statement.
func (g *generator) VisitWhileStatement(s ast.WhileStatement) {
	// create two new branch target labels in the block where the while-do statement is located
	cb := s.CurrentBlock()
	beforeCondition := cb.UniqueName(prefix[labelPrefix])
	behindStatement := cb.UniqueName(prefix[labelPrefix])

	// append a branch-target instruction before the condition expression instructions
	g.intermediateCode.AppendInstruction(
		ic.BranchTarget, // target for any branching operation
		noAddress,
		noAddress,
		ic.NewLiteralAddress(ts.String.String(), beforeCondition), // before while-do condition expression
		s.Index()) // before while-do statement condition expression in the token stream

	// calculate the result of the condition expression
	s.Condition().Accept(g)

	// jump behind the statement if the condition is false
	g.jumpConditional(s.Condition(), false, behindStatement)

	// execute statement if the condition is true
	s.Statement().Accept(g)

	// append a jump instruction to jump back to the condition expression instructions
	g.intermediateCode.AppendInstruction(
		ic.Jump, // jump back to the condition expression
		noAddress,
		noAddress,
		ic.NewLiteralAddress(ts.String.String(), beforeCondition), // before while-do condition expression
		s.Index()) // end of while-do statement in the token stream

	// append a branch-target instruction behind the statement instructions
	g.intermediateCode.AppendInstruction(
		ic.BranchTarget, // target for any branching operation
		noAddress,
		noAddress,
		ic.NewLiteralAddress(ts.String.String(), behindStatement), // behind while-do statement
		s.Index()) // behind while-do statement in the token stream
}

// Generate code for a compound begin-end statement.
func (g *generator) VisitCompoundStatement(s ast.CompoundStatement) {
	// generate code for all statements in the compound statement
	for _, statement := range s.Statements() {
		statement.Accept(g)
	}
}

// Conditional jump instruction based on an expression that must be a unary or comparison operation node.
func (g *generator) jumpConditional(expression ast.Expression, jumpIfCondition bool, label string) {
	var jump *ic.Instruction
	address := ic.NewLiteralAddress(ts.String.String(), label)

	// odd operation or comparison operations are valid for conditional jumps
	switch condition := expression.(type) {
	// unary operation node with the odd operation
	case ast.UnaryOperation:
		if condition.Operation() == ast.Odd {
			if jumpIfCondition {
				jump = ic.NewInstruction(ic.JumpNotEqual, noAddress, noAddress, address, condition.Index())
			} else {
				jump = ic.NewInstruction(ic.JumpEqual, noAddress, noAddress, address, condition.Index())
			}
		} else {
			panic(eh.NewGeneralError(eh.Generator, failureMap, eh.Fatal, unknownUnaryOperation, nil, nil))
		}

	// comparison operation node with the equal, not equal, less, less equal, greater, or greater equal operation
	case ast.ComparisonOperation:
		if jumpIfCondition {
			// jump if the condition is true
			switch condition.Operation() {
			case ast.Equal:
				jump = ic.NewInstruction(ic.JumpEqual, noAddress, noAddress, address, condition.Index())

			case ast.NotEqual:
				jump = ic.NewInstruction(ic.JumpNotEqual, noAddress, noAddress, address, condition.Index())

			case ast.Less:
				jump = ic.NewInstruction(ic.JumpLess, noAddress, noAddress, address, condition.Index())

			case ast.LessEqual:
				jump = ic.NewInstruction(ic.JumpLessEqual, noAddress, noAddress, address, condition.Index())

			case ast.Greater:
				jump = ic.NewInstruction(ic.JumpGreater, noAddress, noAddress, address, condition.Index())

			case ast.GreaterEqual:
				jump = ic.NewInstruction(ic.JumpGreaterEqual, noAddress, noAddress, address, condition.Index())

			default:
				panic(eh.NewGeneralError(eh.Generator, failureMap, eh.Fatal, unknownComparisonOperation, nil, nil))
			}
		} else {
			// jump if the condition is false
			switch condition.Operation() {
			case ast.Equal:
				jump = ic.NewInstruction(ic.JumpNotEqual, noAddress, noAddress, address, condition.Index())

			case ast.NotEqual:
				jump = ic.NewInstruction(ic.JumpEqual, noAddress, noAddress, address, condition.Index())

			case ast.Less:
				jump = ic.NewInstruction(ic.JumpGreaterEqual, noAddress, noAddress, address, condition.Index())

			case ast.LessEqual:
				jump = ic.NewInstruction(ic.JumpGreater, noAddress, noAddress, address, condition.Index())

			case ast.Greater:
				jump = ic.NewInstruction(ic.JumpLessEqual, noAddress, noAddress, address, condition.Index())

			case ast.GreaterEqual:
				jump = ic.NewInstruction(ic.JumpLess, noAddress, noAddress, address, condition.Index())

			default:
				panic(eh.NewGeneralError(eh.Generator, failureMap, eh.Fatal, unknownComparisonOperation, nil, nil))
			}
		}

	default:
		panic(eh.NewGeneralError(eh.Generator, failureMap, eh.Fatal, unknownComparisonOperation, nil, nil))
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
		panic(eh.NewGeneralError(eh.Generator, failureMap, eh.Fatal, unexpectedIntermediateCodeResult, nil, nil))
	}

	g.results.Remove(result)
	return result.Value.(*ic.Address)
}

// This is a visit function. Configure intermediate code extensions for abstract syntax declarations and fill the symbol table of the intermediate code unit.
func configureSymbols(node ast.Node, code any) {
	unit := code.(ic.IntermediateCodeUnit)

	// safely switch on the kind of the node and then cast it to the appropriate declaration kind
	switch node.Kind() {
	case ast.KindConstantDeclaration:
		cd := node.(ast.ConstantDeclaration)

		// create the intermediate code name of the abstract syntax constant declaration
		name := cd.CurrentBlock().UniqueName(prefix[constantPrefix])

		// store the intermediate code name in the abstract syntax constant declaration symbol extension
		cd.Symbol().Extension[intermediateCodeExtension] = newSymbolMetaData(name)

		// insert a new constant symbol for each abstract syntax declaration into the intermediate code symbol table
		unit.Insert(sym.NewSymbol(name, sym.ConstantEntry, cd.Symbol().DataType, nil))

	case ast.KindVariableDeclaration:
		vd := node.(ast.VariableDeclaration)

		// create the intermediate code name of the abstract syntax variable declaration
		name := vd.CurrentBlock().UniqueName(prefix[variablePrefix])

		// store the intermediate code name in the abstract syntax variable declaration symbol extension
		vd.Symbol().Extension[intermediateCodeExtension] = newSymbolMetaData(name)

		// insert a new variable symbol for each abstract syntax declaration into the intermediate code symbol table
		unit.Insert(sym.NewSymbol(name, sym.VariableEntry, vd.Symbol().DataType, nil))

	case ast.KindFunctionDeclaration:
		fd := node.(ast.FunctionDeclaration)

		// create the intermediate code name of the abstract syntax function declaration
		name := fd.CurrentBlock().UniqueName(prefix[functionPrefix])

		// store the intermediate code name in the abstract syntax function declaration symbol extension
		fd.Symbol().Extension[intermediateCodeExtension] = newSymbolMetaData(name)

		// insert a new function symbol for each abstract syntax declaration into the intermediate code symbol table
		if fd.IsFunction() {
			unit.Insert(sym.NewSymbol(name, sym.FunctionEntry, fd.Symbol().DataType, nil))
		} else {
			unit.Insert(sym.NewSymbol(name, sym.ProcedureEntry, fd.Symbol().DataType, nil))
		}
	}
}

// This is a visit function. Collect the debug string table from the abstract syntax tree and store it as debug information.
func collectDebugStringTable(node ast.Node, code any) {
	var function, functionSource string
	var global, entryPoint bool
	var tokenStreamIndex int
	info := code.(dbg.DebugInformation)

	// only process block nodes from the abstract syntax tree
	if node.Kind() != ast.KindBlock {
		return
	}

	// now safely cast the node to a block node
	block := node.(ast.Block)

	// only the main block has no parent function declaration
	if block.IsRootBlock() {
		// take the entry point label as the function name and the function source name
		function = plt.EntryPointLabel
		functionSource = plt.EntryPointLabel

		// only the entry point is marked as global
		global = true
		entryPoint = true

		// token stream index of the main block
		tokenStreamIndex = block.Index()
	} else {
		// treat the parent node as a function declaration
		fd := block.Function()

		// take the flattened name from intermediate code as the function name
		function = getIntermediateCodeName(fd.Symbol())

		// take the abstract syntax function declaration name as the function source name
		functionSource = fd.Name()

		// token stream index of the function declaration
		tokenStreamIndex = fd.Index()
	}

	// append the function to the debug information
	if info.AppendFunction(function, functionSource, global, entryPoint, tokenStreamIndex) {
		// append all constant declarations and variable declarations of the function to the debug information
		for _, declaration := range block.Declarations() {
			// safely switch on the kind of the node and then cast it to the appropriate declaration kind
			switch declaration.Kind() {
			case ast.KindConstantDeclaration:
				cd := declaration.(ast.ConstantDeclaration)

				// determine the intermediate code name of the abstract syntax constant declaration
				name := getIntermediateCodeName(cd.Symbol())

				// take the abstract syntax constant declaration name as the constant source name
				nameSource := cd.Name()

				// extract the data type names of the constant from abstract syntax
				dataTypeName := cd.DataTypeName()
				dataTypeNameSource := cd.DataTypeName()

				// append the local constant of the function to the debug information
				constantType := dbg.NewSimpleDataType(dataTypeName, dataTypeNameSource)
				info.AppendConstant(function, functionSource, name, nameSource, constantType, cd.Value, cd.Index())

			case ast.KindVariableDeclaration:
				vd := declaration.(ast.VariableDeclaration)

				// determine the intermediate code name of the abstract syntax variable declaration
				name := getIntermediateCodeName(vd.Symbol())

				// take the abstract syntax variable declaration name as the variable source name
				nameSource := vd.Name()

				// extract the data type names of the variable from abstract syntax
				dataTypeName := vd.DataTypeName()
				dataTypeNameSource := vd.DataTypeName()

				// append the local variable of the function to the debug information
				variableType := dbg.NewSimpleDataType(dataTypeName, dataTypeNameSource)
				info.AppendVariable(function, functionSource, name, nameSource, variableType, vd.Index())
			}
		}
	}
}

// Define the string composite data type structure with length and data pointer members for debug information.
func appendStringDataType(stringEncoding plt.StringEncoding, debugInformation dbg.DebugInformation) {
	// string target encoding data type names that depend on the target platform's string encoding
	stringEncodingTypeNameSource := stringBaseTypeMap[stringEncoding]
	stringEncodingTypeName := stringEncodingTypeNameSource

	// create the string member data type for length
	uint64Type := dbg.NewSimpleDataType(
		ic.Unsigned64.String(),
		ts.Unsigned64.String(),
	)

	// create the string member data type for data
	stringEncodingType := dbg.NewSimpleDataType(
		stringEncodingTypeName.String(),
		stringEncodingTypeNameSource.String(),
	)

	// create the string member data type for pointer to data
	stringEncodingPointerType := dbg.NewPointerDataType(
		stringEncodingTypeName.PointerString(),
		stringEncodingTypeNameSource.PointerString(),
		stringEncodingType,
	)

	// create the composite data type for the string that has to use the name provided by the debug information
	stringType := dbg.NewCompositeDataType(
		debugInformation.GetDebugStringTable().String,
		ts.String.String(),
	)

	// append the string composite data type and its member data types to debugging information
	debugInformation.AppendDataType(uint64Type)
	debugInformation.AppendDataType(stringEncodingType)
	debugInformation.AppendDataType(stringEncodingPointerType)
	debugInformation.AppendDataType(stringType)
	debugInformation.AppendMember(stringType.Name(), stringLengthMemberName, stringLengthMemberName, uint64Type)
	debugInformation.AppendMember(stringType.Name(), stringDataMemberName, stringDataMemberName, stringEncodingPointerType)
}

// Set the intermediate code name for an abstract syntax symbol. The name is flattened and unique within the intermediate code unit.
func setIntermediateCodeName(symbol *sym.Symbol, name string) {
	symbol.Extension[intermediateCodeExtension] = newSymbolMetaData(name)
}

// Get the intermediate code name from an abstract syntax symbol. The name is flattened and unique within the intermediate code unit.
func getIntermediateCodeName(symbol *sym.Symbol) string {
	return symbol.Extension[intermediateCodeExtension].(*symbolMetaData).name
}
