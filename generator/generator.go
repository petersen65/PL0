// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package generator

import (
	ast "github.com/petersen65/PL0/ast"
	emt "github.com/petersen65/PL0/emitter"
)

// Generator is a parser pass for code generation. It implements the Visitor interface to traverse the AST and generate code.
type generator struct {
	abstractSyntax ast.Block   // abstract syntax tree to generate code for
	emitter        emt.Emitter // emitter that emits the code
}

// Create new code generator with the given abstract syntax tree.
func newGenerator(abstractSyntax ast.Block) Generator {
	return &generator{abstractSyntax: abstractSyntax, emitter: emt.NewEmitter()}
}

// Generate code for the given abstract syntax tree and return the emitter with the generated code.
// The generator itself is performing a top down, left to right, and leftmost derivation walk on the abstract syntax tree.
func (g *generator) Generate() emt.Emitter {
	g.abstractSyntax.Accept(g)
	return g.emitter
}

// Generate code for a block, all nested procedure blocks, and its statement.
func (g *generator) VisitBlock(bn *ast.BlockNode) {
	// take the first offset for block variables from the emitter
	bn.Offset = emt.VariableOffsetStart

	// emit a jump to the first instruction of the block whose address is not yet known
	// the address of the jump instruction itself was already stored in the symbol table as part of the block's procedure symbol
	//
	// for block declaration depth
	// 	 0: jump is always used to start the program
	//   1 and above: jump is only used for forward calls to procedures with a lower declaration depth (block not emitted yet)
	firstInstruction := g.emitter.Jump(emt.NullAddress)

	// emit all declarations and with that all blocks of nested procedures (calls generator recursively)
	for _, declaration := range bn.Declarations {
		declaration.Accept(g)
	}

	// update the jump instruction address to the first instruction of the block
	g.emitter.Update(firstInstruction, g.emitter.GetNextAddress(), nil)

	// update the code address of the block's procedure symbol to the first instruction of the block
	procedureSymbol := bn.Scope.Lookup(bn.Name)
	procedureSymbol.Address = uint64(g.emitter.GetNextAddress())

	// allocating stack space for block variables is the first code instruction of the block
	g.emitter.AllocateStackSpace(emt.Offset(bn.Offset))

	// emit all statement instructions which are defining the code logic of the block
	bn.Statement.Accept(g)

	// emit a return instruction to return from the block
	g.emitter.Return()
}

// Generate code for a constant declaration.
func (g *generator) VisitConstantDeclaration(cd *ast.ConstantDeclarationNode) {
	// not required for code generation
}

// Generate code for a variable declaration.
func (g *generator) VisitVariableDeclaration(vd *ast.VariableDeclarationNode) {
	// calculate storage location for the variable on the block stack frame
	block := ast.SearchBlock(ast.CurrentBlock, vd)
	vd.Offset = block.Offset
	block.Offset++
}

// Generate code for a procedure declaration.
func (g *generator) VisitProcedureDeclaration(pd *ast.ProcedureDeclarationNode) {
	// calculate the absolute address of the procedure block in the text section
	pd.Address = uint64(g.emitter.GetNextAddress())

	// generate code for the block of the procedure
	pd.Block.Accept(g)
}

// Generate code for a literal.
func (g *generator) VisitLiteral(l *ast.LiteralNode) {
	g.emitter.Constant(l.Value)
}

// Generate code for a constant reference.
func (g *generator) VisitConstantReference(cr *ast.ConstantReferenceNode) {
	g.emitter.Constant(cr.Declaration.(*ast.ConstantDeclarationNode).Value)
}

// Generate code for a variable reference.
func (g *generator) VisitVariableReference(vr *ast.VariableReferenceNode) {
	declarationDepth := ast.SearchBlock(ast.CurrentBlock, vr.Declaration).Depth
	referenceDepth := ast.SearchBlock(ast.CurrentBlock, vr).Depth
	offset := emt.Offset(vr.Declaration.(*ast.VariableDeclarationNode).Offset)
	g.emitter.LoadVariable(offset, referenceDepth-declarationDepth)
}

// Generate code for a procedure reference.
func (g *generator) VisitProcedureReference(pr *ast.ProcedureReferenceNode) {
	// not required for code generation
}

// Generate code for a unary operation.
func (g *generator) VisitUnaryOperation(uo *ast.UnaryOperationNode) {
	uo.Operand.Accept(g)

	switch uo.Operation {
	case ast.Odd:
		g.emitter.Odd()

	case ast.Negate:
		g.emitter.Negate()
	}
}

// Generate code for a binary operation.
func (g *generator) VisitBinaryOperation(bo *ast.BinaryOperationNode) {
	bo.Left.Accept(g)
	bo.Right.Accept(g)

	switch bo.Operation {
	case ast.Plus:
		g.emitter.Add()

	case ast.Minus:
		g.emitter.Subtract()

	case ast.Times:
		g.emitter.Multiply()

	case ast.Divide:
		g.emitter.Divide()
	}
}

// Generate code for a conditional operation.
func (g *generator) VisitConditionalOperation(co *ast.ConditionalOperationNode) {
	co.Left.Accept(g)
	co.Right.Accept(g)

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
	}
}

// Generate code for an assignment statement.
func (g *generator) VisitAssignmentStatement(as *ast.AssignmentStatementNode) {
	variableDeclaration := as.Variable.(*ast.VariableReferenceNode).Declaration.(*ast.VariableDeclarationNode)
	declarationDepth := ast.SearchBlock(ast.CurrentBlock, variableDeclaration).Depth
	referenceDepth := ast.SearchBlock(ast.CurrentBlock, as).Depth
	offset := emt.Offset(variableDeclaration.Offset)
	as.Expression.Accept(g)
	g.emitter.StoreVariable(offset, referenceDepth-declarationDepth)
}

// Generate code for a read statement.
func (g *generator) VisitReadStatement(rs *ast.ReadStatementNode) {
	variableDeclaration := rs.Variable.(*ast.VariableReferenceNode).Declaration.(*ast.VariableDeclarationNode)
	declarationDepth := ast.SearchBlock(ast.CurrentBlock, variableDeclaration).Depth
	referenceDepth := ast.SearchBlock(ast.CurrentBlock, rs).Depth
	offset := emt.Offset(variableDeclaration.Offset)
	g.emitter.System(emt.Read)
	g.emitter.StoreVariable(offset, referenceDepth-declarationDepth)
}

// Generate code for a write statement.
func (g *generator) VisitWriteStatement(ws *ast.WriteStatementNode) {
	ws.Expression.Accept(g)
	g.emitter.System(emt.Write)
}

// Generate code for a call statement.
func (g *generator) VisitCallStatement(cs *ast.CallStatementNode) {
	procedureDeclaration := cs.Procedure.(*ast.ProcedureReferenceNode).Declaration.(*ast.ProcedureDeclarationNode)
	declarationDepth := ast.SearchBlock(ast.CurrentBlock, procedureDeclaration).Depth
	referenceDepth := ast.SearchBlock(ast.CurrentBlock, cs).Depth
	address := emt.Address(procedureDeclaration.Address)
	g.emitter.Call(address, referenceDepth-declarationDepth)
}

// Generate code for an if-then statement.
func (g *generator) VisitIfStatement(is *ast.IfStatementNode) {
	is.Condition.Accept(g)
	ifDecision := g.jumpConditional(is.Condition, false)
	is.Statement.Accept(g)
	g.emitter.Update(ifDecision, g.emitter.GetNextAddress(), nil)
}

// Generate code for a while-do statement.
func (g *generator) VisitWhileStatement(ws *ast.WhileStatementNode) {
	conditionAddress := g.emitter.GetNextAddress()
	ws.Condition.Accept(g)
	whileDecision := g.jumpConditional(ws.Condition, false)
	ws.Statement.Accept(g)
	g.emitter.Jump(conditionAddress)
	g.emitter.Update(whileDecision, g.emitter.GetNextAddress(), nil)
}

// Generate code for a compound begin-end statement.
func (g *generator) VisitCompoundStatement(cs *ast.CompoundStatementNode) {
	for _, statement := range cs.Statements {
		statement.Accept(g)
	}
}

// Emit a conditional jump instruction based on an expression that must be a unary or conditional operation node.
func (g *generator) jumpConditional(expression ast.Expression, jumpIfCondition bool) emt.Address {
	var address emt.Address

	switch condition := expression.(type) {
	case *ast.UnaryOperationNode:
		if condition.Operation == ast.Odd {
			if jumpIfCondition {
				address = g.emitter.JumpNotEqual(emt.NullAddress)
			} else {
				address = g.emitter.JumpEqual(emt.NullAddress)
			}
		} else {
			panic("generator error: invalid unary operation")
		}

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
			}
		}

	default:
		panic("generator error: invalid conditional operation")
	}

	return address
}
