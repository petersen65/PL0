// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package generator

import (
	asm "github.com/petersen65/PL0/v2/assembler"
	ast "github.com/petersen65/PL0/v2/ast"
	cor "github.com/petersen65/PL0/v2/core"
	emt "github.com/petersen65/PL0/v2/emitter"
)

// Walks of the generator to emit IL/0 or assembly code.
const (
	_ = walk(iota)
	emitterWalk
	assemblerWalk
)

type (
	// Generator walk for code generation.
	walk int

	// Generator is a parser pass for code generation. It implements the Visitor interface to traverse the AST and generate code.
	generator struct {
		walk           walk          // current walk of the generator
		abstractSyntax ast.Block     // abstract syntax tree to generate code for
		emitter        emt.Emitter   // emitter that emits IL/0 code
		assembler      asm.Assembler // assembler that emits assembly code
	}
)

// Map data types from the abstract syntax tree to generator data types.
var generatorType = map[ast.DataType]string{
	ast.Void:      "void",
	ast.Integer64: "i64",
}

// Create new code generator with the given source and abstract syntax tree.
func newGenerator(source string, abstractSyntax ast.Block) Generator {
	return &generator{walk: emitterWalk, abstractSyntax: abstractSyntax, emitter: emt.NewEmitter(), assembler: asm.NewAssembler(source)}
}

// Generate code for the given abstract syntax tree and return the emitter and assembler for their generated code.
// The generator itself is performing a top down, left to right, and leftmost derivation walk on the abstract syntax tree.
func (g *generator) Generate() (emt.Emitter, asm.Assembler) {
	g.walk = emitterWalk
	//g.abstractSyntax.Accept(g)
	g.walk = assemblerWalk
	g.abstractSyntax.Accept(g)
	return g.emitter, g.assembler
}

// Generate code for a block, all nested procedure blocks, and its statement.
func (g *generator) VisitBlock(bn *ast.BlockNode) {
	switch g.walk {
	case emitterWalk:
		// take the first offset for block variables from the emitter
		bn.Offset = emt.VariableOffsetStart

		// emit a jump to the first instruction of the block whose address is not yet known
		// the address of the jump instruction itself was already stored in the procedure declaration as part of the block's procedure declaration visit function execution
		//
		// for block nesting depth
		// 	 0: jump is always used to start the program
		//   1 and above: jump is only used for forward calls to procedures with a lower block nesting depth (block not emitted yet)
		firstInstruction := g.emitter.Jump(emt.NullAddress)

		// emit all declarations and with that all blocks of nested procedures (calls generator recursively)
		for _, declaration := range bn.Declarations {
			declaration.Accept(g)
		}

		// update the jump instruction address to the first instruction of the block
		g.emitter.Update(firstInstruction, g.emitter.GetNextAddress(), nil)

		// update the code address of the block's procedure declaration to the first instruction of the block
		if bn.ParentNode != nil {
			bn.ParentNode.(*ast.ProcedureDeclarationNode).Address = uint64(g.emitter.GetNextAddress())
		}

		// allocating stack space for block variables is the first code instruction of the block
		g.emitter.AllocateStackSpace(emt.Offset(bn.Offset))

		// emit all statement instructions which are defining the code logic of the block
		bn.Statement.Accept(g)

		// emit a return instruction to return from the block
		g.emitter.Return()

	case assemblerWalk:
		// emit all blocks of nested procedure declarations first (calls generator recursively but creates only top-level function definitions)
		for _, declaration := range bn.Declarations {
			if _, ok := declaration.(*ast.ProcedureDeclarationNode); ok {
				declaration.Accept(g)
			}
		}

		if bn.Depth == 0 {
			// emit all global variable declarations of the entrypoint block (main function)
			for _, declaration := range bn.Declarations {
				if variableDeclaration, ok := declaration.(*ast.VariableDeclarationNode); ok {
					variableDeclaration.Accept(g)
				}
			}

			// create a function definition for the entrypoint block (main function)
			g.assembler.Function(bn.Name, generatorType[ast.Integer64])

			// emit all statement instructions which are defining the code logic of the main function
			bn.Statement.Accept(g)

			// end the program by returning from the main function
			g.assembler.Return(int64(0), generatorType[ast.Integer64])

			// end the function definition
			g.assembler.EndFunction()
		} else {
			// create a function definition for the block
			g.assembler.Function(bn.Name, generatorType[ast.Void])

			// emit all local variable declarations of the block
			for _, declaration := range bn.Declarations {
				if variableDeclaration, ok := declaration.(*ast.VariableDeclarationNode); ok {
					variableDeclaration.Accept(g)
				}
			}

			// emit all statement instructions which are defining the code logic of the block
			bn.Statement.Accept(g)

			// return from the function
			g.assembler.Return(nil, generatorType[ast.Void])

			// end the function definition
			g.assembler.EndFunction()
		}

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, invalidGeneratorWalk, nil, nil))
	}
}

// Generate code for a constant declaration.
func (g *generator) VisitConstantDeclaration(cd *ast.ConstantDeclarationNode) {
	// not required for code generation
}

// Generate code for a variable declaration.
func (g *generator) VisitVariableDeclaration(vd *ast.VariableDeclarationNode) {
	switch g.walk {
	case emitterWalk:
		// calculate storage location for the variable on the block stack frame
		block := ast.SearchBlock(ast.CurrentBlock, vd)
		vd.Offset = block.Offset
		block.Offset++

	case assemblerWalk:
		// determine the block nesting depth of the variable declaration
		blockNestingDepth := ast.SearchBlock(ast.CurrentBlock, vd).Depth

		if blockNestingDepth == 0 {
			// emit a global variable declaration for the variable
			g.assembler.VariableDeclaration(vd.Name, generatorType[vd.DataType], vd.Ssa, true)
		} else {
			// emit a local variable declaration for the variable
			g.assembler.VariableDeclaration(vd.Name, generatorType[vd.DataType], vd.Ssa, false)
		}

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, invalidGeneratorWalk, nil, nil))
	}
}

// Generate code for a procedure declaration.
func (g *generator) VisitProcedureDeclaration(pd *ast.ProcedureDeclarationNode) {
	switch g.walk {
	case emitterWalk:
		// calculate the absolute address of the procedure block in the text section
		pd.Address = uint64(g.emitter.GetNextAddress())

		// generate code for the block of the procedure
		pd.Block.Accept(g)

	case assemblerWalk:
		// generate code for the block of the procedure
		pd.Block.Accept(g)

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, invalidGeneratorWalk, nil, nil))
	}
}

// Generate code for a literal (load a constant value).
func (g *generator) VisitLiteral(ln *ast.LiteralNode) {
	switch g.walk {
	case emitterWalk:
		g.emitter.Constant(ln.Value)

	case assemblerWalk:
		g.assembler.Constant(ln.Value, generatorType[ln.DataType])

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, invalidGeneratorWalk, nil, nil))
	}
}

// Generate code for an identifier use.
func (g *generator) VisitIdentifierUse(iu *ast.IdentifierUseNode) {
	switch g.walk {
	case emitterWalk:
		switch iu.Context {
		case ast.Constant:
			g.emitter.Constant(iu.Scope.Lookup(iu.Name).Declaration.(*ast.ConstantDeclarationNode).Value)

		case ast.Variable:
			// get variable declaration of the variable to load
			variableDeclaration := iu.Scope.Lookup(iu.Name).Declaration.(*ast.VariableDeclarationNode)

			// determine the block nesting depth of the variable declaration
			blockNestingDepth := ast.SearchBlock(ast.CurrentBlock, variableDeclaration).Depth

			// determine the block nesting depth of the variable use from inside an expression or statement
			useDepth := ast.SearchBlock(ast.CurrentBlock, iu).Depth

			// calculate the offset of the variable on the block stack frame
			offset := emt.Offset(variableDeclaration.Offset)

			// load the content of the variable
			g.emitter.LoadVariable(offset, useDepth-blockNestingDepth)

		case ast.Procedure:
			// not required for code generation

		default:
			panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, invalidContextInIdentifierUse, nil, nil))
		}

	case assemblerWalk:
		switch iu.Context {
		case ast.Constant:
			// get constant declaration of the constant to load
			constantDeclaration := iu.Scope.Lookup(iu.Name).Declaration.(*ast.ConstantDeclarationNode)

			// load the constant value that has a specific data type
			g.assembler.Constant(constantDeclaration.Value, generatorType[constantDeclaration.DataType])

		case ast.Variable:
			// get variable declaration of the variable to load
			variableDeclaration := iu.Scope.Lookup(iu.Name).Declaration.(*ast.VariableDeclarationNode)

			// determine the block nesting depth of the variable declaration
			blockNestingDepth := ast.SearchBlock(ast.CurrentBlock, variableDeclaration).Depth

			// load the content of the variable and increase its SSA counter
			_, variableDeclaration.Ssa = g.assembler.LoadVariable(
				iu.Name,
				generatorType[variableDeclaration.DataType],
				variableDeclaration.Ssa,
				blockNestingDepth == 0)

		case ast.Procedure:
			// not required for code generation

		default:
			panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, invalidContextInIdentifierUse, nil, nil))
		}

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, invalidGeneratorWalk, nil, nil))
	}
}

// Generate code for a unary operation.
func (g *generator) VisitUnaryOperation(uo *ast.UnaryOperationNode) {
	// load the operand value from the result of the expression
	uo.Operand.Accept(g)

	// perform the unary operation on the operand value
	switch uo.Operation {
	case ast.Odd:
		g.emitter.Odd()

	case ast.Negate:
		g.emitter.Negate()

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownUnaryOperation, nil, nil))

	}
}

// Generate code for a binary operation.
func (g *generator) VisitBinaryOperation(bo *ast.BinaryOperationNode) {
	// load the left-hand-side value from the result of the left expression
	bo.Left.Accept(g)

	// load the right-hand-side value from the result of the right expression
	bo.Right.Accept(g)

	// perform the binary operation on the left and right-hand-side values
	switch g.walk {
	case emitterWalk:
		switch bo.Operation {
		case ast.Plus:
			g.emitter.Add()

		case ast.Minus:
			g.emitter.Subtract()

		case ast.Times:
			g.emitter.Multiply()

		case ast.Divide:
			g.emitter.Divide()

		default:
			panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownBinaryOperation, nil, nil))
		}

	case assemblerWalk:
		switch bo.Operation {
		case ast.Plus:
			g.assembler.Add()

		case ast.Minus:
			g.assembler.Subtract()

		case ast.Times:
			g.assembler.Multiply()

		case ast.Divide:
			g.assembler.Divide()

		default:
			panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownBinaryOperation, nil, nil))
		}

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, invalidGeneratorWalk, nil, nil))
	}
}

// Generate code for a conditional operation.
func (g *generator) VisitConditionalOperation(co *ast.ConditionalOperationNode) {
	// load the left-hand-side value from the result of the left expression
	co.Left.Accept(g)

	// load the right-hand-side value from the result of the right expression
	co.Right.Accept(g)

	// perform the conditional operation on the left and right-hand-side values
	switch co.Operation {
	case ast.Equal:
		g.emitter.Equal()

	case ast.NotEqual:
		g.emitter.NotEqual()

	case ast.Less:
		g.emitter.Less()

	case ast.LessEqual:
		g.emitter.LessEqual()

	case ast.Greater:
		g.emitter.Greater()

	case ast.GreaterEqual:
		g.emitter.GreaterEqual()

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownConditionalOperation, nil, nil))
	}
}

// Generate code for an assignment statement.
func (g *generator) VisitAssignmentStatement(as *ast.AssignmentStatementNode) {
	switch g.walk {
	case emitterWalk:
		// load the value from the result of the right-hand-side expression of the assignment
		as.Expression.Accept(g)

		// get the variable declaration on the left-hand-side of the assignment
		variableUse := as.Variable.(*ast.IdentifierUseNode)
		variableDeclaration := variableUse.Scope.Lookup(variableUse.Name).Declaration.(*ast.VariableDeclarationNode)

		// determine the block nesting depth of the variable declaration
		blockNestingDepth := ast.SearchBlock(ast.CurrentBlock, variableDeclaration).Depth

		// determine the block nesting depth of the assignment statement where the variable is used
		assignmentDepth := ast.SearchBlock(ast.CurrentBlock, as).Depth

		// calculate the offset of the variable on the block stack frame
		offset := emt.Offset(variableDeclaration.Offset)

		// store the resultant value from the right-hand-side expression in the variable on the left-hand-side of the assignment
		g.emitter.StoreVariable(offset, assignmentDepth-blockNestingDepth)

	case assemblerWalk:
		// load the value from the result of the right-hand-side expression of the assignment
		as.Expression.Accept(g)

		// get the variable declaration on the left-hand-side of the assignment
		variableUse := as.Variable.(*ast.IdentifierUseNode)
		variableDeclaration := variableUse.Scope.Lookup(variableUse.Name).Declaration.(*ast.VariableDeclarationNode)

		// determine the block nesting depth of the variable declaration
		blockNestingDepth := ast.SearchBlock(ast.CurrentBlock, variableDeclaration).Depth

		// store the resultant value from the right-hand-side expression in the variable on the left-hand-side of the assignment
		g.assembler.StoreVariable(
			variableUse.Name,
			generatorType[variableDeclaration.DataType],
			variableDeclaration.Ssa,
			blockNestingDepth == 0)

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, invalidGeneratorWalk, nil, nil))
	}
}

// Generate code for a read statement.
func (g *generator) VisitReadStatement(rs *ast.ReadStatementNode) {
	// get the variable declaration of the variable to read into
	variableUse := rs.Variable.(*ast.IdentifierUseNode)
	variableDeclaration := variableUse.Scope.Lookup(variableUse.Name).Declaration.(*ast.VariableDeclarationNode)

	// determine the block nesting depth of the variable declaration
	blockNestingDepth := ast.SearchBlock(ast.CurrentBlock, variableDeclaration).Depth

	// determine the block nesting depth of the read statement where the variable is used
	readDepth := ast.SearchBlock(ast.CurrentBlock, rs).Depth

	// calculate the offset of the variable on the block stack frame
	offset := emt.Offset(variableDeclaration.Offset)

	// read the value from the input by calling the system-call read function
	g.emitter.System(emt.Read)

	// store the read value in the variable on the right-hand-side of the read statement
	g.emitter.StoreVariable(offset, readDepth-blockNestingDepth)
}

// Generate code for a write statement.
func (g *generator) VisitWriteStatement(ws *ast.WriteStatementNode) {
	// load the value from the result of the expression on the right-hand-side of the write statement
	ws.Expression.Accept(g)

	// write the value to the output by calling the system-call write function
	g.emitter.System(emt.Write)
}

// Generate code for a call statement.
func (g *generator) VisitCallStatement(cs *ast.CallStatementNode) {
	// get the declaration of the procedure to call
	procedureUse := cs.Procedure.(*ast.IdentifierUseNode)
	procedureDeclaration := procedureUse.Scope.Lookup(procedureUse.Name).Declaration.(*ast.ProcedureDeclarationNode)

	// determine the block nesting depth of the procedure declaration
	blockNestingDepth := ast.SearchBlock(ast.CurrentBlock, procedureDeclaration).Depth

	// determine the block nesting depth of the call statement where the procedure is called
	callDepth := ast.SearchBlock(ast.CurrentBlock, cs).Depth

	// calculate the address of the procedure to call
	address := emt.Address(procedureDeclaration.Address)

	// call the procedure on the given address
	g.emitter.Call(address, callDepth-blockNestingDepth)
}

// Generate code for an if-then statement.
func (g *generator) VisitIfStatement(is *ast.IfStatementNode) {
	// calculate the result of the condition expression
	is.Condition.Accept(g)

	// jump behind the statement if the condition is false
	ifDecision := g.jumpConditional(is.Condition, false)

	// execute statement if the condition is true
	is.Statement.Accept(g)

	// update the jump instruction address to the next instruction after the statement
	g.emitter.Update(ifDecision, g.emitter.GetNextAddress(), nil)
}

// Generate code for a while-do statement.
func (g *generator) VisitWhileStatement(ws *ast.WhileStatementNode) {
	// get the address at the beginning of the condition expression instructions
	conditionAddress := g.emitter.GetNextAddress()

	// calculate the result of the condition expression
	ws.Condition.Accept(g)

	// jump behind the statement if the condition is false
	whileDecision := g.jumpConditional(ws.Condition, false)

	// execute statement if the condition is true
	ws.Statement.Accept(g)

	// jump back to the condition expression instructions
	g.emitter.Jump(conditionAddress)

	// update the jump instruction address to the next instruction after the statement
	g.emitter.Update(whileDecision, g.emitter.GetNextAddress(), nil)
}

// Generate code for a compound begin-end statement.
func (g *generator) VisitCompoundStatement(cs *ast.CompoundStatementNode) {
	// generate code for all statements in the compound statement
	for _, statement := range cs.Statements {
		statement.Accept(g)
	}
}

// Emit a conditional jump instruction based on an expression that must be a unary or conditional operation node.
func (g *generator) jumpConditional(expression ast.Expression, jumpIfCondition bool) emt.Address {
	var address emt.Address

	// odd operation or conditional operations are valid for conditional jumps
	switch condition := expression.(type) {
	// unary operation node with the odd operation
	case *ast.UnaryOperationNode:
		if condition.Operation == ast.Odd {
			if jumpIfCondition {
				address = g.emitter.JumpNotEqual(emt.NullAddress)
			} else {
				address = g.emitter.JumpEqual(emt.NullAddress)
			}
		} else {
			panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownUnaryOperation, nil, nil))
		}

	// conditional operation node with the equal, not equal, less, less equal, greater, or greater equal operation
	case *ast.ConditionalOperationNode:
		if jumpIfCondition {
			// jump if the condition is true and remember the address of the jump instruction
			switch condition.Operation {
			case ast.Equal:
				address = g.emitter.JumpEqual(emt.NullAddress)

			case ast.NotEqual:
				address = g.emitter.JumpNotEqual(emt.NullAddress)

			case ast.Less:
				address = g.emitter.JumpLess(emt.NullAddress)

			case ast.LessEqual:
				address = g.emitter.JumpLessEqual(emt.NullAddress)

			case ast.Greater:
				address = g.emitter.JumpGreater(emt.NullAddress)

			case ast.GreaterEqual:
				address = g.emitter.JumpGreaterEqual(emt.NullAddress)

			default:
				panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownConditionalOperation, nil, nil))
			}
		} else {
			// jump if the condition is false and remember the address of the jump instruction
			switch condition.Operation {
			case ast.Equal:
				address = g.emitter.JumpNotEqual(emt.NullAddress)

			case ast.NotEqual:
				address = g.emitter.JumpEqual(emt.NullAddress)

			case ast.Less:
				address = g.emitter.JumpGreaterEqual(emt.NullAddress)

			case ast.LessEqual:
				address = g.emitter.JumpGreater(emt.NullAddress)

			case ast.Greater:
				address = g.emitter.JumpLessEqual(emt.NullAddress)

			case ast.GreaterEqual:
				address = g.emitter.JumpLess(emt.NullAddress)

			default:
				panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownConditionalOperation, nil, nil))
			}
		}

	default:
		panic(cor.NewGeneralError(cor.Generator, failureMap, cor.Fatal, unknownConditionalOperation, nil, nil))
	}

	return address
}
