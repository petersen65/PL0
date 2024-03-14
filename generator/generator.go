// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package generator

import (
	ast "github.com/petersen65/PL0/ast"
	emt "github.com/petersen65/PL0/emitter"
)

// Generator is the parser pass for code generation. It implements the Visitor interface to traverse the AST and generate code.
type generator struct {
	emitter        emt.Emitter // emitter that emits the code
	abstractSyntax ast.Block   // abstract syntax tree to generate code for
}

// Create new code generator with the given abstract syntax tree.
func newGenerator(abstractSyntax ast.Block) Generator {
	return &generator{emitter: emt.NewEmitter(), abstractSyntax: abstractSyntax}
}

// Generate code for the given abstract syntax tree and return the emitter with the generated code.
// The generator itself is performing a top down, left to right, and leftmost derivation walk on the abstract syntax tree.
func (g *generator) Generate() emt.Emitter {
	g.abstractSyntax.Accept(g)
	return g.emitter
}

// Generate code for each symbol (not required for code generation).
func (g *generator) VisitSymbol(symbol *ast.Symbol) {
}

// Generate code for a block, all nested procedure blocks, and its statement.
func (g *generator) VisitBlock(bn *ast.BlockNode) {
	var varOffset uint64 = emt.VariableOffsetStart // take the first offset for block variables from the emitter

	// calculate the offset of the block's variables on the stack
	for symbol := range bn.Scope.IterateCurrent() {
		// only variables are allocated on the stack
		if symbol.Kind == ast.Variable {
			symbol.Offset = varOffset
			varOffset++
		}
	}

	// emit a jump to the first instruction of the block whose address is not yet known
	// the address of the jump instruction itself was already stored in the symbol table as part of the block's procedure symbol
	//
	// for block declaration depth
	// 	 0: jump is always used to start the program
	//   1 and above: jump is only used for forward calls to procedures with a lower declaration depth (block not emitted yet)
	firstInstruction := g.emitter.Jump(emt.NullAddress)

	// emit all blocks of nested procedures which calls the generator recursively
	for _, procedure := range bn.Procedures {
		procedure.Accept(g)
	}

	// update the jump instruction address to the first instruction of the block
	g.emitter.Update(firstInstruction, g.emitter.GetNextAddress(), nil)

	// update the code address of the block's procedure symbol to the first instruction of the block
	procedureSymbol := bn.Scope.Lookup(bn.Name)
	procedureSymbol.Address = uint64(g.emitter.GetNextAddress())

	// allocating stack space for block variables is the first code instruction of the block
	g.emitter.AllocateStackSpace(emt.Offset(varOffset))

	// emit all statement instructions which are defining the code logic of the block
	bn.Statement.Accept(g)

	// emit a return instruction to return from the block
	g.emitter.Return()
}

// Generate code for a literal.
func (g *generator) VisitLiteral(l *ast.LiteralNode) {
	g.emitter.Constant(l.Value)
}

// Generate code for a constant reference.
func (g *generator) VisitConstantReference(cr *ast.ConstantReferenceNode) {
	g.emitter.Constant(cr.Symbol.Value)
}

// Generate code for a variable reference.
func (g *generator) VisitVariableReference(vr *ast.VariableReferenceNode) {
	referenceDeclarationDepth := ast.SearchBlock(ast.CurrentBlock, vr).Depth
	g.emitter.LoadVariable(emt.Offset(vr.Symbol.Offset), referenceDeclarationDepth-vr.Symbol.Depth)
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
	referenceDeclarationDepth := ast.SearchBlock(ast.CurrentBlock, as).Depth
	as.Expression.Accept(g)
	g.emitter.StoreVariable(emt.Offset(as.Symbol.Offset), referenceDeclarationDepth-as.Symbol.Depth)
}

// Generate code for a read statement.
func (g *generator) VisitReadStatement(rs *ast.ReadStatementNode) {
	referenceDeclarationDepth := ast.SearchBlock(ast.CurrentBlock, rs).Depth
	g.emitter.System(emt.Read)
	g.emitter.StoreVariable(emt.Offset(rs.Symbol.Offset), referenceDeclarationDepth-rs.Symbol.Depth)
}

// Generate code for a write statement.
func (g *generator) VisitWriteStatement(ws *ast.WriteStatementNode) {
	ws.Expression.Accept(g)
	g.emitter.System(emt.Write)
}

// Generate code for a call statement.
func (g *generator) VisitCallStatement(cs *ast.CallStatementNode) {
	referenceDeclarationDepth := ast.SearchBlock(ast.CurrentBlock, cs).Depth
	g.emitter.Call(emt.Address(cs.Symbol.Address), referenceDeclarationDepth-cs.Symbol.Depth)
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
