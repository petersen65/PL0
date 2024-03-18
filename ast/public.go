// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package ast implements the abstract syntax tree (AST) for the PL/0 parser.
package ast

import (
	"fmt"
	"io"
)

// Traverse the abstract syntax tree in specific orders.
const (
	_ = TraversalOrder(iota)
	PreOrder
	InOrder
	PostOrder
	LevelOrder
)

// Search parent block nodes in the abstract syntax tree.
const (
	_ = BlockSearchMode(iota)
	CurrentBlock
	RootBlock
)

// Operators with one operand.
const (
	_ = UnaryOperator(iota)
	Odd
	Negate
)

// Operators with two operands.
const (
	_ = BinaryOperator(iota)
	Plus
	Minus
	Times
	Divide
)

// Operators for comparison.
const (
	_ = RelationalOperator(iota)
	Equal
	NotEqual
	Less
	LessEqual
	Greater
	GreaterEqual
)

// Data types of literals and symbols.
const (
	_ = DataType(iota)
	Integer64
)

// Kind of supported symbol entry.
const (
	Constant = Entry(iota)
	Variable
	Procedure
)

type (
	// Traversal order for the abstract syntax tree.
	TraversalOrder int

	// Search mode for block nodes in the abstract syntax tree.
	BlockSearchMode int

	// Take one operand and perform an operation on it.
	UnaryOperator int

	// Take two operands and perform an operation on them.
	BinaryOperator int

	// Take two operands and perform a comparison on them.
	RelationalOperator int

	// The data type of a symbol.
	DataType int

	// Kind of symbol entries.
	Entry int

	// A symbol is a data structure that stores all the necessary information related to a declared identifier that the compiler must know.
	Symbol struct {
		ParentNode Node   // parent node in the abstract syntax tree
		Name       string // name of constant, variable, or procedure
		Kind       Entry  // constant, variable, or procedure
		Depth      int32  // declaration nesting depth of constant, variable, or procedure
		Value      int64  // value of constant
		Offset     uint64 // offset of variable in its runtime procedure stack frame
		Address    uint64 // absolute address of procedure in text section
	}

	// A symbol table is a data structure that stores a mapping from symbol name (string) to the symbol.
	SymbolTable map[string]*Symbol

	// A scope is a data structure that stores information about declared identifiers. Scopes are nested from the outermost scope to the innermost scope.
	Scope struct {
		Outer       *Scope       // outer scope or nil if this is the outermost scope
		names       []string     // enable deterministic iteration over the symbol table
		symbolTable *SymbolTable // symbol table of the scope
	}

	// A node in the abstract syntax tree.
	Node interface {
		SetParent(node Node)
		Parent() Node
		Children() []Node
		String() string
		Accept(visitor Visitor)
	}

	// A block represented as an abstract syntax tree.
	Block interface {
		Node
		BlockString() string
	}

	// A declaration represented as an abstract syntax tree.
	Declaration interface {
		Node
		DeclarationString() string
	}

	// An expression represented as an abstract syntax tree.
	Expression interface {
		Node
		ExpressionString() string
	}

	// A statement represented as an abstract syntax tree.
	Statement interface {
		Node
		StatementString() string
	}

	// Block node represents a block in the AST.
	BlockNode struct {
		ParentNode Node      // parent node of the block
		Name       string    // name of the block that can be used for lookup in the symbol table
		Depth      int32     // declaration nesting depth
		Scope      *Scope    // scope with symbol table of the block that has its own outer scope chain
		Procedures []Block   // nested procedures of the block
		Statement  Statement // statement of the block
	}

	// ConstantDeclaration node represents a constant declaration in the AST.
	ConstantDeclarationNode struct {
		ParentNode       Node     // parent node of the constant declaration
		Name             string   // name of the constant
		Value            any      // value of constant
		DataType         DataType // data type of the constant
		TokenStreamIndex int      // index of the token in the token stream
	}

	// VariableDeclaration node represents a variable declaration in the AST.
	VariableDeclarationNode struct {
		ParentNode       Node     // parent node of the variable declaration
		Name             string   // name of the variable
		DataType         DataType // data type of the variable
		TokenStreamIndex int      // index of the token in the token stream
	}

	// ProcedureDeclaration node represents a procedure declaration in the AST.
	ProcedureDeclarationNode struct {
		ParentNode       Node   // parent node of the procedure declaration
		Name             string // name of the procedure
		Block            Block  // block of the procedure
		TokenStreamIndex int    // index of the token in the token stream
	}

	// Literal node represents the usage of a literal value in the AST.
	LiteralNode struct {
		ParentNode Node     // parent node of the literal
		Value      any      // literal value
		DataType   DataType // data type of the literal
	}

	// ConstantReferenceNode represents the usage of a constant in the AST.
	ConstantReferenceNode struct {
		ParentNode Node    // parent node of the constant reference
		Symbol     *Symbol // constant symbol entry
	}

	// VariableReferenceNode represents the usage of a variable in the AST.
	VariableReferenceNode struct {
		ParentNode Node    // parent node of the variable reference
		Symbol     *Symbol // variable symbol entry
	}

	// UnaryOperation node represents a unary operation in the AST.
	UnaryOperationNode struct {
		ParentNode Node          // parent node of the unary operation
		Operation  UnaryOperator // unary operation
		Operand    Expression    // operand of the unary operation
	}

	// BinaryOperation node represents a binary operation in the AST.
	BinaryOperationNode struct {
		ParentNode Node           // parent node of the binary operation
		Operation  BinaryOperator // binary operation
		Left       Expression     // left operand of the binary operation
		Right      Expression     // right operand of the binary operation
	}

	// ConditionalOperation node represents a conditional operation in the AST.
	ConditionalOperationNode struct {
		ParentNode Node               // parent node of the conditional
		Operation  RelationalOperator // conditional operation
		Left       Expression         // left operand of the conditional operation
		Right      Expression         // right operand of the conditional operation
	}

	// AssignmentStatement node represents an assignment statement in the AST.
	AssignmentStatementNode struct {
		ParentNode Node       // parent node of the assignment statement
		Symbol     *Symbol    // variable symbol entry on the left side of the assignment statement
		Expression Expression // expression on the right side of the assignment statement
	}

	// ReadStatement node represents a read statement in the AST.
	ReadStatementNode struct {
		ParentNode Node    // parent node of the read statement
		Symbol     *Symbol // variable symbol entry of the read statement
	}

	// WriteStatement node represents a write statement in the AST.
	WriteStatementNode struct {
		ParentNode Node       // parent node of the write statement
		Expression Expression // expression of the write statement
	}

	// CallStatement node represents a call statement in the AST.
	CallStatementNode struct {
		ParentNode Node    // parent node of the call statement
		Symbol     *Symbol // procedure symbol entry of the call statement
	}

	// IfStatement node represents an if-then statement in the AST.
	IfStatementNode struct {
		ParentNode Node       // parent node of the if-then statement
		Condition  Expression // if-condition of the if-then statement
		Statement  Statement  // then-statement of the if-then statement
	}

	// WhileStatement node represents a while-do statement in the AST.
	WhileStatementNode struct {
		ParentNode Node       // parent node of the while-do statement
		Condition  Expression // while-condition of the while-do statement
		Statement  Statement  // do-statement of the while-do statement
	}

	// CompoundStatement node represents a begin-end statement in the AST.
	CompoundStatementNode struct {
		ParentNode Node        // parent node of the begin-end compound statement
		Statements []Statement // all statements of the begin-end compound statement
	}

	// A visitor is an interface for visiting nodes in the abstract syntax tree. It allows all the methods for a parser pass to be grouped in a single visitor struct.
	// The visitor design pattern allows implementing double dispatch for traversing the abstract syntax tree. Each parser pass method is chosen based on:
	//   the dynamic type of the object (the AST node) determines the method to be called, and
	//   the dynamic type of the argument (the visitor) determines the behavior of the method.
	Visitor interface {
		VisitSymbol(symbol *Symbol)
		VisitBlock(block *BlockNode)
		VisitConstantDeclaration(declaration *ConstantDeclarationNode)
		VisitVariableDeclaration(declaration *VariableDeclarationNode)
		VisitProcedureDeclaration(declaration *ProcedureDeclarationNode)
		VisitLiteral(literal *LiteralNode)
		VisitConstantReference(constant *ConstantReferenceNode)
		VisitVariableReference(variable *VariableReferenceNode)
		VisitUnaryOperation(operation *UnaryOperationNode)
		VisitBinaryOperation(operation *BinaryOperationNode)
		VisitConditionalOperation(operation *ConditionalOperationNode)
		VisitAssignmentStatement(assignment *AssignmentStatementNode)
		VisitReadStatement(read *ReadStatementNode)
		VisitWriteStatement(write *WriteStatementNode)
		VisitCallStatement(call *CallStatementNode)
		VisitIfStatement(ifStmt *IfStatementNode)
		VisitWhileStatement(whileStmt *WhileStatementNode)
		VisitCompoundStatement(compound *CompoundStatementNode)
	}
)

// KindNames maps symbol kinds to their string representation.
var KindNames = map[Entry]string{
	Constant:  "constant",
	Variable:  "variable",
	Procedure: "procedure",
}

// NewScope creates a new scope with an empty symbol table.
func NewScope(outer *Scope) *Scope {
	symbolTable := make(SymbolTable)
	return &Scope{Outer: outer, symbolTable: &symbolTable}
}

// Insert a symbol into the symbol table of the scope. If the symbol already exists, it will be overwritten.
func (s *Scope) Insert(symbol *Symbol) {
	if s.LookupCurrent(symbol.Name) == nil {
		s.names = append(s.names, symbol.Name)
	}

	(*s.symbolTable)[symbol.Name] = symbol
}

// Lookup a symbol in the symbol table of the scope. If the symbol is not found, the outer scope is searched.
func (s *Scope) Lookup(name string) *Symbol {
	if symbol := s.LookupCurrent(name); symbol != nil {
		return symbol
	}

	if s.Outer != nil {
		return s.Outer.Lookup(name)
	}

	return nil
}

// Lookup a symbol in the symbol table of the current scope. If the symbol is not found, nil is returned.
func (s *Scope) LookupCurrent(name string) *Symbol {
	if symbol, ok := (*s.symbolTable)[name]; ok {
		return symbol
	}

	return nil
}

// Deterministically iterate over all symbols in the symbol table of the current scope.
func (s *Scope) IterateCurrent() <-chan *Symbol {
	symbols := make(chan *Symbol)

	go func() {
		for _, name := range s.names {
			symbols <- (*s.symbolTable)[name]
		}

		close(symbols)
	}()

	return symbols
}

// An empty block does not generate code.
func NewEmptyBlock() Block {
	return NewBlock("", 0, NewScope(nil), make([]Block, 0), NewEmptyStatement())
}

// An empty declaration is a 0 constant with empty name and should only occur during a parsing error.
func NewEmptyDeclaration() Declaration {
	return NewConstantDeclaration("", int64(0), Integer64, 0)
}

// An empty expression is a 0 literal and should only occur during a parsing error.
func NewEmptyExpression() Expression {
	return NewLiteral(int64(0), Integer64)
}

// An empty statement does not generate code.
func NewEmptyStatement() Statement {
	return newCompoundStatement(make([]Statement, 0))
}

// NewBlock creates a new block node in the abstract syntax tree.
func NewBlock(name string, depth int32, scope *Scope, procedures []Block, statement Statement) Block {
	return newBlock(name, depth, scope, procedures, statement)
}

// NewConstantDeclaration creates a new constant declaration node in the abstract syntax tree.
func NewConstantDeclaration(name string, value any, dataType DataType, index int) Declaration {
	return newConstantDeclaration(name, value, dataType, index)
}

// NewVariableDeclaration creates a new variable declaration node in the abstract syntax tree.
func NewVariableDeclaration(name string, dataType DataType, index int) Declaration {
	return newVariableDeclaration(name, dataType, index)
}

// NewProcedureDeclaration creates a new procedure declaration node in the abstract syntax tree.
func NewProcedureDeclaration(name string, block Block, index int) Declaration {
	return newProcedureDeclaration(name, block, index)
}

// NewLiteral creates a new literal node in the abstract syntax tree.
func NewLiteral(value any, dataType DataType) Expression {
	return newLiteral(value, dataType)
}

// NewConstantReference creates a new constant-reference node in the abstract syntax tree.
func NewConstantReference(entry *Symbol) Expression {
	return newConstantReference(entry)
}

// NewVariableReference creates a new variable-reference node in the abstract syntax tree.
func NewVariableReference(entry *Symbol) Expression {
	return newVariableReference(entry)
}

// NewUnaryOperation creates a new unary operation node in the abstract syntax tree.
func NewUnaryOperation(operation UnaryOperator, operand Expression) Expression {
	return newUnaryOperation(operation, operand)
}

// NewBinaryOperation creates a new binary operation node in the abstract syntax tree.
func NewBinaryOperation(operation BinaryOperator, left, right Expression) Expression {
	return newBinaryOperation(operation, left, right)
}

// NewConditionalOperation creates a new conditional operation node in the abstract syntax tree.
func NewConditionalOperation(operation RelationalOperator, left, right Expression) Expression {
	return newConditionalOperation(operation, left, right)
}

// NewAssignmentStatement creates a new assignment statement node in the abstract syntax tree.
func NewAssignmentStatement(entry *Symbol, expression Expression) Statement {
	return newAssignmentStatement(entry, expression)
}

// NewReadStatement creates a new read statement node in the abstract syntax tree.
func NewReadStatement(entry *Symbol) Statement {
	return newReadStatement(entry)
}

// NewWriteStatement creates a new write statement node in the abstract syntax tree.
func NewWriteStatement(expression Expression) Statement {
	return newWriteStatement(expression)
}

// NewCallStatement creates a new call statement node in the abstract syntax tree.
func NewCallStatement(entry *Symbol) Statement {
	return newCallStatement(entry)
}

// NewIfStatement creates a new if-then statement node in the abstract syntax tree.
func NewIfStatement(condition Expression, statement Statement) Statement {
	return newIfStatement(condition, statement)
}

// NewWhileStatement creates a new while-do statement node in the abstract syntax tree.
func NewWhileStatement(condition Expression, statement Statement) Statement {
	return newWhileStatement(condition, statement)
}

// NewCompoundStatement creates a compound statement node in the abstract syntax tree.
func NewCompoundStatement(statements []Statement) Statement {
	return newCompoundStatement(statements)
}

// Walk traverses an abstract syntax tree in a specific order and calls the visitor for each node.
func Walk(parent Node, order TraversalOrder, visitor Visitor) error {
	return walk(parent, order, visitor)
}

// SearchBlock searches for a parent block node in the abstract syntax tree based on the search mode.
func SearchBlock(mode BlockSearchMode, node Node) *BlockNode {
	for node != nil {
		if block, ok := node.(*BlockNode); ok {
			if mode == CurrentBlock {
				return block
			} else if mode == RootBlock && block.Parent() == nil {
				return block
			}
		}

		node = node.Parent()
	}

	return nil
}

// Print the abstract syntax tree to the specified writer.
func PrintAbstractSyntaxTree(node Node, indent string, last bool, print io.Writer) {
	print.Write([]byte(fmt.Sprintf("%v+- %v\n", indent, node)))

	if last {
		indent += "   "
	} else {
		indent += "|  "
	}

	for i, child := range node.Children() {
		PrintAbstractSyntaxTree(child, indent, i == len(node.Children())-1, print)
	}
}
