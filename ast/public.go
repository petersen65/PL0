// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

// Package ast implements the abstract syntax tree (AST) for the PL/0 parser.
package ast

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

// Data types of symbols.
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

	// Describes a range of lines in the source code.
	SourceDescription struct {
		ParentNode Node // parent node in the abstract syntax tree
		From, To   int  // range of lines in the source code

		// Lines of source code.
		Lines []struct {
			Line int    // line number
			Code []byte // line of source code
		}
	}

	// A scope is a data structure that stores information about declared identifiers. Scopes are nested from the outermost scope to the innermost scope.
	Scope struct {
		Outer       *Scope       // outer scope or nil if this is the outermost scope
		SymbolTable *SymbolTable // symbol table of the scope
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
		ParentNode Node               // parent node of the block
		Name       string             // name of the block that can be used for lookup in the symbol table
		Depth      int32              // declaration nesting depth
		Scope      *Scope             // scope with symbol table of the block that has its own outer scope chain
		Procedures []Block            // nested procedures of the block
		Statement  Statement          // statement of the block
		Source     *SourceDescription // source description for the block node
	}

	// Literal node represents the usage of a literal value in the AST.
	LiteralNode struct {
		ParentNode Node               // parent node of the literal
		Value      any                // literal value
		DataType   DataType           // data type of the literal
		Source     *SourceDescription // source description for the literal node
	}

	// ConstantReferenceNode represents the usage of a constant in the AST.
	ConstantReferenceNode struct {
		ParentNode Node               // parent node of the constant reference
		Symbol     *Symbol            // constant symbol entry
		Source     *SourceDescription // source description for the constant node
	}

	// VariableReferenceNode represents the usage of a variable in the AST.
	VariableReferenceNode struct {
		ParentNode Node               // parent node of the variable reference
		Symbol     *Symbol            // variable symbol entry
		Source     *SourceDescription // source description for the variable node
	}

	// UnaryOperation node represents a unary operation in the AST.
	UnaryOperationNode struct {
		ParentNode Node               // parent node of the unary operation
		Operation  UnaryOperator      // unary operation
		Operand    Expression         // operand of the unary operation
		Source     *SourceDescription // source description for the unary operation node
	}

	// BinaryOperation node represents a binary operation in the AST.
	BinaryOperationNode struct {
		ParentNode Node               // parent node of the binary operation
		Operation  BinaryOperator     // binary operation
		Left       Expression         // left operand of the binary operation
		Right      Expression         // right operand of the binary operation
		Source     *SourceDescription // source description for the binary operation node
	}

	// ConditionalOperation node represents a conditional operation in the AST.
	ConditionalOperationNode struct {
		ParentNode Node               // parent node of the conditional
		Operation  RelationalOperator // conditional operation
		Left       Expression         // left operand of the conditional operation
		Right      Expression         // right operand of the conditional operation
		Source     *SourceDescription // source description for the conditional operation node
	}

	// AssignmentStatement node represents an assignment statement in the AST.
	AssignmentStatementNode struct {
		ParentNode Node               // parent node of the assignment statement
		Symbol     *Symbol            // variable symbol entry on the left side of the assignment statement
		Expression Expression         // expression on the right side of the assignment statement
		Source     *SourceDescription // source description for the assignment statement node
	}

	// ReadStatement node represents a read statement in the AST.
	ReadStatementNode struct {
		ParentNode Node               // parent node of the read statement
		Symbol     *Symbol            // variable symbol entry of the read statement
		Source     *SourceDescription // source description for the read statement node
	}

	// WriteStatement node represents a write statement in the AST.
	WriteStatementNode struct {
		ParentNode Node               // parent node of the write statement
		Expression Expression         // expression of the write statement
		Source     *SourceDescription // source description for the write statement node
	}

	// CallStatement node represents a call statement in the AST.
	CallStatementNode struct {
		ParentNode Node               // parent node of the call statement
		Symbol     *Symbol            // procedure symbol entry of the call statement
		Source     *SourceDescription // source description for the call statement node
	}

	// IfStatement node represents an if-then statement in the AST.
	IfStatementNode struct {
		ParentNode Node               // parent node of the if-then statement
		Condition  Expression         // if-condition of the if-then statement
		Statement  Statement          // then-statement of the if-then statement
		Source     *SourceDescription // source description for the if-then statement node
	}

	// WhileStatement node represents a while-do statement in the AST.
	WhileStatementNode struct {
		ParentNode Node               // parent node of the while-do statement
		Condition  Expression         // while-condition of the while-do statement
		Statement  Statement          // do-statement of the while-do statement
		Source     *SourceDescription // source description for the while-do statement node
	}

	// CompoundStatement node represents a begin-end statement in the AST.
	CompoundStatementNode struct {
		ParentNode Node               // parent node of the begin-end compound statement
		Statements []Statement        // all statements of the begin-end compound statement
		Source     *SourceDescription // source description for the begin-end statement node
	}

	// A visitor is an interface for visiting nodes in the abstract syntax tree. It allows all the methods for a parser pass to be grouped in a single visitor struct.
	// The visitor design pattern allows implementing double dispatch for traversing the abstract syntax tree. Each parser pass method is chosen based on:
	//   the dynamic type of the object (the AST node) determines the method to be called, and
	//   the dynamic type of the argument (the visitor) determines the behavior of the method.
	Visitor interface {
		VisitSymbol(symbol *Symbol)
		VisitSourceDescription(source *SourceDescription)
		VisitBlock(block *BlockNode)
		VisitLiteral(literal *LiteralNode)
		VisitConstant(constant *ConstantReferenceNode)
		VisitVariable(variable *VariableReferenceNode)
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
	return &Scope{Outer: outer, SymbolTable: &symbolTable}
}

// Insert a symbol into the symbol table of the scope. If the symbol already exists, it will be overwritten.
func (s *Scope) Insert(symbol *Symbol) {
	(*s.SymbolTable)[symbol.Name] = symbol
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
	if symbol, ok := (*s.SymbolTable)[name]; ok {
		return symbol
	}

	return nil
}

// NewBlock creates a new block node in the abstract syntax tree.
func NewBlock(name string, depth int32, scope *Scope, procedures []Block, statement Statement, source *SourceDescription) Block {
	return newBlock(name, depth, scope, procedures, statement, source)
}

// NewLiteral creates a new literal node in the abstract syntax tree.
func NewLiteral(value any, dataType DataType, source *SourceDescription) Expression {
	return newLiteral(value, dataType, source)
}

// NewConstantReference creates a new constant-reference node in the abstract syntax tree.
func NewConstantReference(entry *Symbol, source *SourceDescription) Expression {
	return newConstantReference(entry, source)
}

// NewVariableReference creates a new variable-reference node in the abstract syntax tree.
func NewVariableReference(entry *Symbol, source *SourceDescription) Expression {
	return newVariableReference(entry, source)
}

// NewUnaryOperation creates a new unary operation node in the abstract syntax tree.
func NewUnaryOperation(operation UnaryOperator, operand Expression, source *SourceDescription) Expression {
	return newUnaryOperation(operation, operand, source)
}

// NewBinaryOperation creates a new binary operation node in the abstract syntax tree.
func NewBinaryOperation(operation BinaryOperator, left, right Expression, source *SourceDescription) Expression {
	return newBinaryOperation(operation, left, right, source)
}

// NewConditionalOperation creates a new conditional operation node in the abstract syntax tree.
func NewConditionalOperation(operation RelationalOperator, left, right Expression, source *SourceDescription) Expression {
	return newConditionalOperation(operation, left, right, source)
}

// NewAssignmentStatement creates a new assignment statement node in the abstract syntax tree.
func NewAssignmentStatement(entry *Symbol, expression Expression, source *SourceDescription) Statement {
	return newAssignmentStatement(entry, expression, source)
}

// NewReadStatement creates a new read statement node in the abstract syntax tree.
func NewReadStatement(entry *Symbol, source *SourceDescription) Statement {
	return newReadStatement(entry, source)
}

// NewWriteStatement creates a new write statement node in the abstract syntax tree.
func NewWriteStatement(expression Expression, source *SourceDescription) Statement {
	return newWriteStatement(expression, source)
}

// NewCallStatement creates a new call statement node in the abstract syntax tree.
func NewCallStatement(entry *Symbol, source *SourceDescription) Statement {
	return newCallStatement(entry, source)
}

// NewIfStatement creates a new if-then statement node in the abstract syntax tree.
func NewIfStatement(condition Expression, statement Statement, source *SourceDescription) Statement {
	return newIfStatement(condition, statement, source)
}

// NewWhileStatement creates a new while-do statement node in the abstract syntax tree.
func NewWhileStatement(condition Expression, statement Statement, source *SourceDescription) Statement {
	return newWhileStatement(condition, statement, source)
}

// NewCompoundStatement creates a compound statement node in the abstract syntax tree.
func NewCompoundStatement(statements []Statement, source *SourceDescription) Statement {
	return newCompoundStatement(statements, source)
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
