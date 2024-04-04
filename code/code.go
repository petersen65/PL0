// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package code

import (
	"container/list"
	"fmt"

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

// Create a new three-address code argument or result address.
func newAddress(dataType ast.DataType, value any) string {
	return fmt.Sprintf("%v %v", dataType, value)
}

// Create a new compiler-generated temporary variable for a block.
func (b *blockMetaData) newTempVariable(datatype ast.DataType) string {
	b.tempVariableCounter++
	return fmt.Sprintf("%v t%v.%v", datatype, b.uniqueId, b.tempVariableCounter)
}

// Create a new compiler-generated label for a block.
func (b *blockMetaData) newLabel() string {
	b.labelCounter++
	return fmt.Sprintf("L%v.%v", b.uniqueId, b.labelCounter)
}

// Push a result onto the stack of temporary results.
func (b *blockMetaData) pushResult(result string) {
	b.results.PushBack(result)
}

// Pop a result from the stack of temporary results.
func (b *blockMetaData) popResult() string {
	result := b.results.Back()

	if result == nil {
		panic(cor.NewGeneralError(cor.Intermediate, failureMap, cor.Fatal, unexpectedTemporaryResult, nil, nil))
	}

	b.results.Remove(result)
	return result.Value.(string)
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
func (i *intermediateCode) NewInstruction(operatiom Operation, label string, difference int32, arg1, arg2, result string) *Instruction {
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

	// all declarations and with that all blocks of nested procedures (calls generator recursively)
	for _, declaration := range bn.Declarations {
		declaration.Accept(i)
	}

	// label of the block marking the start of the block' statement
	bn.Label = i.metaData[bn.UniqueId].newLabel()
	i.AppendInstruction(i.NewInstruction(NullOperation, bn.Label, UnusedDifference, NoAddress, NoAddress, NoAddress))

	// statement of the block
	bn.Statement.Accept(i)

	// return from the block
	i.AppendInstruction(i.NewInstruction(Return, NoLabel, UnusedDifference, NoAddress, NoAddress, NoAddress))
}

// Generate code for a constant declaration.
func (i *intermediateCode) VisitConstantDeclaration(declaration *ast.ConstantDeclarationNode) {
	// not required for code generation
}

// Generate code for a variable declaration.
func (i *intermediateCode) VisitVariableDeclaration(vd *ast.VariableDeclarationNode) {
	block := ast.SearchBlock(ast.CurrentBlock, vd)
	vd.Offset = i.metaData[block.UniqueId].stackOffset
	i.metaData[block.UniqueId].stackOffset++
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
		newAddress(ln.DataType, ln.Value),
		NoAddress,
		metaData.newTempVariable(ln.DataType))

	// push the temporary result onto the stack and append the instruction to the module
	metaData.pushResult(instruction.Code.Result)
	i.AppendInstruction(instruction)
}

func (i *intermediateCode) VisitIdentifierUse(use *ast.IdentifierUseNode) {
}

func (i *intermediateCode) VisitUnaryOperation(operation *ast.UnaryOperationNode) {
}

func (i *intermediateCode) VisitBinaryOperation(operation *ast.BinaryOperationNode) {
}

func (i *intermediateCode) VisitConditionalOperation(operation *ast.ConditionalOperationNode) {
}

func (i *intermediateCode) VisitAssignmentStatement(assignment *ast.AssignmentStatementNode) {
}

func (i *intermediateCode) VisitReadStatement(read *ast.ReadStatementNode) {
}

func (i *intermediateCode) VisitWriteStatement(write *ast.WriteStatementNode) {
}

func (i *intermediateCode) VisitCallStatement(call *ast.CallStatementNode) {
}

func (i *intermediateCode) VisitIfStatement(ifStmt *ast.IfStatementNode) {
}

func (i *intermediateCode) VisitWhileStatement(whileStmt *ast.WhileStatementNode) {
}

func (i *intermediateCode) VisitCompoundStatement(compound *ast.CompoundStatementNode) {
}
