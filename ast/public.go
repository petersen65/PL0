// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

// Package ast implements the abstract syntax tree (AST) for the PL/0 parser.
package ast

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

// Supported node types in the abstract syntax tree.
const (
	_ = NodeType(iota)
	SymbolNode
	SourceDescriptionNode
	BlockNode
	LiteralNode
	ConstantNode
	VariableNode
	UnaryOperationNode
	BinaryOperationNode
	ConditionalOperationNode
	AssignmentStatementNode
	ReadStatementNode
	WriteStatementNode
	CallStatementNode
	IfStatementNode
	WhileStatementNode
	CompoundStatementNode
)

type (
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

	// The type of a node in the abstract syntax tree.
	NodeType int

	// A symbol is a data structure that stores all the necessary information related to a declared identifier that the compiler must know.
	Symbol struct {
		Name    string // name of constant, variable, or procedure
		Kind    Entry  // constant, variable, or procedure
		Depth   int32  // declaration nesting depth of constant, variable, or procedure
		Value   int64  // value of constant
		Offset  uint64 // offset of variable in its runtime procedure stack frame
		Address uint64 // absolute address of procedure in text section
	}

	// A symbol table is a data structure that stores a mapping from symbol name (string) to the symbol.
	SymbolTable map[string]*Symbol

	// Describes a range of lines in the source code.
	SourceDescription struct {
		From, To int // range of lines in the source code

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
		Type() NodeType
		Title() string
		Parent() Node
		Children() []Node
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

// NewLiteralReference creates a new literal-reference node in the abstract syntax tree.
func NewLiteralReference(value any, dataType DataType, source *SourceDescription) Expression {
	return newLiteralReference(value, dataType, source)
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
