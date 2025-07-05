// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package ast implements the abstract syntax tree (AST) for the PL/0 parser.
package ast

import (
	"io"

	cor "github.com/petersen65/PL0/v2/core"
)

// Empty scopes are required to use this number as their scope id
const EmptyScopeId = -1

// EmptyConstantName allows the detection of empty constants because of parsing errors. They should be ignored in all compiler phases.
const EmptyConstantName = "@constant"

// Types of nodes in the abstract syntax tree.
const (
	BlockType = NodeType(iota)
	ConstantDeclarationType
	VariableDeclarationType
	ProcedureDeclarationType
	LiteralType
	IdentifierUseType
	UnaryOperationType
	BinaryOperationType
	ConditionalOperationType
	AssignmentStatementType
	ReadStatementType
	WriteStatementType
	CallStatementType
	IfStatementType
	WhileStatementType
	CompoundStatementType
)

// Traverse the abstract syntax tree in specific orders.
const (
	PreOrder = TraversalOrder(iota)
	InOrder
	PostOrder
	LevelOrder
)

// Search parent block nodes in the abstract syntax tree.
const (
	CurrentBlock = BlockSearchMode(iota)
	RootBlock
)

// Operators with one operand.
const (
	Odd = UnaryOperator(iota)
	Negate
)

// Operators with two operands.
const (
	Plus = BinaryOperator(iota)
	Minus
	Times
	Divide
)

// Operators for comparison.
const (
	Equal = RelationalOperator(iota)
	NotEqual
	Less
	LessEqual
	Greater
	GreaterEqual
)

// Data types of literals, constants, and variables.
const (
	Integer64  = DataType(iota) // signed 64-bit integer
	Integer32                   // signed 32-bit integer
	Integer16                   // signed 16-bit integer
	Integer8                    // signed 8-bit integer
	Float64                     // IEEE 754 64-bit floating-point number
	Float32                     // IEEE 754 32-bit floating-point number
	Unsigned64                  // unsigned 64-bit integer
	Unsigned32                  // unsigned 32-bit integer
	Unsigned16                  // unsigned 16-bit integer
	Unsigned8                   // unsigned 8-bit integer
	Unicode                     // signed 32-bit Unicode code point (U+0000 ... U+10FFFF)
	Boolean                     // unsigned 8-bit boolean (0 or 1, false or true)
)

// Kind of supported symbol entry as bit-mask.
const (
	Constant Entry = 1 << iota
	Variable
	Procedure
)

// Usage mode of an identifier as bit-mask.
const (
	Read Usage = 1 << iota
	Write
	Execute
)

type (
	// Type of a node in the abstract syntax tree.
	NodeType int

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

	// String representation of a data type.
	DataTypeRepresentation string

	// Kind of symbol entries (bit-mask).
	Entry uint64

	// Usage mode of an identifier (bit-mask).
	Usage uint64

	// Support for symbol table extensions of compiler phases
	ExtensionType int

	// A symbol is a data structure that stores all the necessary information related to a declared identifier that the compiler must know.
	Symbol struct {
		Name        string                // name of the symbol
		Kind        Entry                 // kind of the symbol
		Declaration Declaration           // declaration node of the symbol
		Extension   map[ExtensionType]any // extensions for compiler phases
	}

	// A symbol table is a data structure that stores a mapping from symbol name (string) to the symbol.
	SymbolTable map[string]*Symbol

	// A scope is a data structure that stores information about declared identifiers. Scopes are nested from the outermost scope to the innermost scope.
	Scope struct {
		Outer             *Scope                // outer scope or nil if this is the outermost scope
		Extension         map[ExtensionType]any // extensions for compiler phases
		id                int32                 // each scope has a unique identifier
		identifierCounter map[rune]uint64       // counter for compiler-generated unique identifier names
		names             []string              // enable deterministic iteration over the symbol table
		symbolTable       *SymbolTable          // symbol table of the scope
	}

	// A node in the abstract syntax tree.
	Node interface {
		Type() NodeType
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
		Print(print io.Writer, args ...any) error
		Export(format cor.ExportFormat, print io.Writer) error
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
		TypeName     string        `json:"type"`         // type name of the block node
		ParentNode   Node          `json:"-"`            // parent node of the block
		Depth        int32         `json:"depth"`        // block nesting depth
		Scope        *Scope        `json:"-"`            // scope with symbol table of the block that has its own outer scope chain
		Declarations []Declaration `json:"declarations"` // all declarations of the block
		Closure      []Declaration `json:"closure"`      // all captured variable declarations of the block
		Statement    Statement     `json:"statement"`    // statement of the block
	}

	// ConstantDeclaration node represents a constant declaration in the AST.
	ConstantDeclarationNode struct {
		TypeName         string       `json:"type"`               // type name of the constant declaration node
		ParentNode       Node         `json:"-"`                  // parent node of the constant declaration
		Name             string       `json:"name"`               // name of the constant
		Value            any          `json:"value"`              // value of constant
		DataType         DataType     `json:"data_type"`          // data type of the constant
		Scope            *Scope       `json:"-"`                  // scope of the constant declaration
		Usage            []Expression `json:"-"`                  // all usages of the constant
		TokenStreamIndex int          `json:"token_stream_index"` // index of the token in the token stream
	}

	// VariableDeclaration node represents a variable declaration in the AST.
	VariableDeclarationNode struct {
		TypeName         string       `json:"type"`               // type name of the variable declaration node
		ParentNode       Node         `json:"-"`                  // parent node of the variable declaration
		Name             string       `json:"name"`               // name of the variable
		DataType         DataType     `json:"data_type"`          // data type of the variable
		Scope            *Scope       `json:"-"`                  // scope of the variable declaration
		Usage            []Expression `json:"-"`                  // all usages of the variable
		TokenStreamIndex int          `json:"token_stream_index"` // index of the token in the token stream
	}

	// ProcedureDeclaration node represents a procedure declaration in the AST.
	ProcedureDeclarationNode struct {
		TypeName         string       `json:"type"`               // type name of the procedure declaration node
		ParentNode       Node         `json:"-"`                  // parent node of the procedure declaration
		Name             string       `json:"name"`               // name of the procedure
		Block            Block        `json:"block"`              // block of the procedure
		Scope            *Scope       `json:"-"`                  // scope of the procedure declaration
		Usage            []Expression `json:"-"`                  // all usages of the procedure
		TokenStreamIndex int          `json:"token_stream_index"` // index of the token in the token stream
	}

	// Literal node represents the usage of a literal value in the AST.
	LiteralNode struct {
		TypeName         string   `json:"type"`               // type name of the literal node
		ParentNode       Node     `json:"-"`                  // parent node of the literal
		Value            any      `json:"value"`              // literal value
		DataType         DataType `json:"data_type"`          // data type of the literal
		Scope            *Scope   `json:"-"`                  // scope of the literal usage
		TokenStreamIndex int      `json:"token_stream_index"` // index of the token in the token stream
	}

	// IdentifierUseNode represents the usage of an identifier in the AST.
	IdentifierUseNode struct {
		TypeName         string `json:"type"`               // type name of the identifier usage node
		ParentNode       Node   `json:"-"`                  // parent node of the identifier usage
		Name             string `json:"name"`               // name of the identifier
		Scope            *Scope `json:"-"`                  // scope of the identifier usage
		Context          Entry  `json:"context"`            // context of the identifier
		Use              Usage  `json:"use"`                // usage mode of the identifier
		TokenStreamIndex int    `json:"token_stream_index"` // index of the token in the token stream
	}

	// UnaryOperation node represents a unary operation in the AST.
	UnaryOperationNode struct {
		TypeName         string        `json:"type"`               // type name of the unary operation node
		ParentNode       Node          `json:"-"`                  // parent node of the unary operation
		Operation        UnaryOperator `json:"operation"`          // unary operation
		Operand          Expression    `json:"operand"`            // operand of the unary operation
		TokenStreamIndex int           `json:"token_stream_index"` // index of the token in the token stream
	}

	// BinaryOperation node represents a binary operation in the AST.
	BinaryOperationNode struct {
		TypeName         string         `json:"type"`               // type name of the binary operation node
		ParentNode       Node           `json:"-"`                  // parent node of the binary operation
		Operation        BinaryOperator `json:"operation"`          // binary operation
		Left             Expression     `json:"left"`               // left operand of the binary operation
		Right            Expression     `json:"right"`              // right operand of the binary operation
		TokenStreamIndex int            `json:"token_stream_index"` // index of the token in the token stream
	}

	// ConditionalOperation node represents a conditional operation in the AST.
	ConditionalOperationNode struct {
		TypeName         string             `json:"type"`               // type name of the conditional operation node
		ParentNode       Node               `json:"-"`                  // parent node of the conditional
		Operation        RelationalOperator `json:"operation"`          // conditional operation
		Left             Expression         `json:"left"`               // left operand of the conditional operation
		Right            Expression         `json:"right"`              // right operand of the conditional operation
		TokenStreamIndex int                `json:"token_stream_index"` // index of the token in the token stream
	}

	// AssignmentStatement node represents an assignment statement in the AST.
	AssignmentStatementNode struct {
		TypeName         string     `json:"type"`               // type name of the assignment statement node
		ParentNode       Node       `json:"-"`                  // parent node of the assignment statement
		Variable         Expression `json:"variable"`           // variable use on the left side of the assignment statement
		Expression       Expression `json:"expression"`         // expression on the right side of the assignment statement
		TokenStreamIndex int        `json:"token_stream_index"` // index of the token in the token stream
	}

	// ReadStatement node represents a read statement in the AST.
	ReadStatementNode struct {
		TypeName         string     `json:"type"`               // type name of the read statement node
		ParentNode       Node       `json:"-"`                  // parent node of the read statement
		Variable         Expression `json:"variable"`           // variable use of the read statement
		TokenStreamIndex int        `json:"token_stream_index"` // index of the token in the token stream
	}

	// WriteStatement node represents a write statement in the AST.
	WriteStatementNode struct {
		TypeName         string     `json:"type"`               // type name of the write statement node
		ParentNode       Node       `json:"-"`                  // parent node of the write statement
		Expression       Expression `json:"expression"`         // expression of the write statement
		TokenStreamIndex int        `json:"token_stream_index"` // index of the token in the token stream
	}

	// CallStatement node represents a call statement in the AST.
	CallStatementNode struct {
		TypeName         string     `json:"type"`               // type name of the call statement node
		ParentNode       Node       `json:"-"`                  // parent node of the call statement
		Procedure        Expression `json:"procedure"`          // procedure use of the call statement
		TokenStreamIndex int        `json:"token_stream_index"` // index of the token in the token stream
	}

	// IfStatement node represents an if-then statement in the AST.
	IfStatementNode struct {
		TypeName         string     `json:"type"`               // type name of the if-then statement node
		ParentNode       Node       `json:"-"`                  // parent node of the if-then statement
		Condition        Expression `json:"condition"`          // if-condition of the if-then statement
		Statement        Statement  `json:"statement"`          // then-statement of the if-then statement
		TokenStreamIndex int        `json:"token_stream_index"` // index of the token in the token stream
	}

	// WhileStatement node represents a while-do statement in the AST.
	WhileStatementNode struct {
		TypeName         string     `json:"type"`               // type name of the while-do statement node
		ParentNode       Node       `json:"-"`                  // parent node of the while-do statement
		Condition        Expression `json:"condition"`          // while-condition of the while-do statement
		Statement        Statement  `json:"statement"`          // do-statement of the while-do statement
		TokenStreamIndex int        `json:"token_stream_index"` // index of the token in the token stream
	}

	// CompoundStatement node represents a begin-end statement in the AST.
	CompoundStatementNode struct {
		TypeName   string      `json:"type"`       // type name of the compound statement node
		ParentNode Node        `json:"-"`          // parent node of the begin-end compound statement
		Statements []Statement `json:"statements"` // all statements of the begin-end compound statement
	}

	// A visitor is an interface for visiting nodes in the abstract syntax tree. It allows all the methods for a parser phase to be grouped in a single visitor struct.
	// The visitor design pattern allows implementing double dispatch for traversing the abstract syntax tree. Each parser phase method is chosen based on:
	//   the dynamic type of the object (the AST node) determines the method to be called, and
	//   the dynamic type of the argument (the visitor) determines the behavior of the method.
	Visitor interface {
		VisitBlock(block *BlockNode)
		VisitConstantDeclaration(declaration *ConstantDeclarationNode)
		VisitVariableDeclaration(declaration *VariableDeclarationNode)
		VisitProcedureDeclaration(declaration *ProcedureDeclarationNode)
		VisitLiteral(literal *LiteralNode)
		VisitIdentifierUse(use *IdentifierUseNode)
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

var (
	// DataTypeNames maps a data type to its string representation.
	DataTypeNames = map[DataType]string{
		Integer64:  "int64_t",
		Integer32:  "int32_t",
		Integer16:  "int16_t",
		Integer8:   "int8_t",
		Float64:    "double",
		Float32:    "float",
		Unsigned64: "uint64_t",
		Unsigned32: "uint32_t",
		Unsigned16: "uint16_t",
		Unsigned8:  "uint8_t",
		Unicode:    "char32_t",
		Boolean:    "bool",
	}

	// NodeTypeNames maps node types to their string representation.
	NodeTypeNames = map[NodeType]string{
		BlockType:                "block",
		ConstantDeclarationType:  "constant",
		VariableDeclarationType:  "variable",
		ProcedureDeclarationType: "procedure",
		LiteralType:              "literal",
		IdentifierUseType:        "use",
		UnaryOperationType:       "unary",
		BinaryOperationType:      "binary",
		ConditionalOperationType: "conditional",
		AssignmentStatementType:  "assignment",
		ReadStatementType:        "read",
		WriteStatementType:       "write",
		CallStatementType:        "call",
		IfStatementType:          "if",
		WhileStatementType:       "while",
		CompoundStatementType:    "compound",
	}

	// KindNames maps symbol kinds to their string representation.
	KindNames = map[Entry]string{
		Constant:  "constant",
		Variable:  "variable",
		Procedure: "procedure",
	}

	// UsageNames maps usage modes to their string representation.
	UsageNames = map[Usage]string{
		Read:    "read",
		Write:   "write",
		Execute: "execute",
	}
)

// NewScope creates a new scope with an empty symbol table and requires a number that is unique accross all compilation phases.
func NewScope(uniqueId int32, outer *Scope) *Scope {
	return newScope(uniqueId, outer)
}

// Create a new entry for the symbol table.
func NewSymbol(name string, kind Entry, declaration Declaration) *Symbol {
	return newSymbol(name, kind, declaration)
}

// An empty scope should only be used in the context of parser errors and is free from any side-effect.
func NewEmptyScope() *Scope {
	return newScope(EmptyScopeId, nil)
}

// An empty declaration is a 0 constant with special name, should only be used in the context of parser errors, and is free from any side-effect.
func NewEmptyDeclaration() Declaration {
	return newConstantDeclaration(EmptyConstantName, int64(0), Integer64, NewEmptyScope(), 0)
}

// An empty expression is a 0 literal, should only be used in the context of parser errors, and is free from any side-effect.
func NewEmptyExpression() Expression {
	return newLiteral(int64(0), Integer64, NewEmptyScope(), 0)
}

// An empty statement does not generate code, should only be used in the context of parser errors, and is free from any side-effect.
func NewEmptyStatement() Statement {
	return newCompoundStatement(make([]Statement, 0))
}

// NewBlock creates a new block node in the abstract syntax tree.
func NewBlock(depth int32, scope *Scope, declarations []Declaration, statement Statement) Block {
	return newBlock(depth, scope, declarations, statement)
}

// NewConstantDeclaration creates a new constant declaration node in the abstract syntax tree.
func NewConstantDeclaration(name string, value any, dataType DataType, scope *Scope, index int) Declaration {
	return newConstantDeclaration(name, value, dataType, scope, index)
}

// NewVariableDeclaration creates a new variable declaration node in the abstract syntax tree.
func NewVariableDeclaration(name string, dataType DataType, scope *Scope, index int) Declaration {
	return newVariableDeclaration(name, dataType, scope, index)
}

// NewProcedureDeclaration creates a new procedure declaration node in the abstract syntax tree.
func NewProcedureDeclaration(name string, block Block, scope *Scope, index int) Declaration {
	return newProcedureDeclaration(name, block, scope, index)
}

// NewLiteral creates a new literal node in the abstract syntax tree.
func NewLiteral(value any, dataType DataType, scope *Scope, index int) Expression {
	return newLiteral(value, dataType, scope, index)
}

// NewIdentifierUse creates a new identifier-use node in the abstract syntax tree.
func NewIdentifierUse(name string, scope *Scope, context Entry, index int) Expression {
	return newIdentifierUse(name, scope, context, index)
}

// NewUnaryOperation creates a new unary operation node in the abstract syntax tree.
func NewUnaryOperation(operation UnaryOperator, operand Expression, index int) Expression {
	return newUnaryOperation(operation, operand, index)
}

// NewBinaryOperation creates a new binary operation node in the abstract syntax tree.
func NewBinaryOperation(operation BinaryOperator, left, right Expression, index int) Expression {
	return newBinaryOperation(operation, left, right, index)
}

// NewConditionalOperation creates a new conditional operation node in the abstract syntax tree.
func NewConditionalOperation(operation RelationalOperator, left, right Expression, index int) Expression {
	return newConditionalOperation(operation, left, right, index)
}

// NewAssignmentStatement creates a new assignment statement node in the abstract syntax tree.
func NewAssignmentStatement(variable, expression Expression, index int) Statement {
	return newAssignmentStatement(variable, expression, index)
}

// NewReadStatement creates a new read statement node in the abstract syntax tree.
func NewReadStatement(variable Expression, index int) Statement {
	return newReadStatement(variable, index)
}

// NewWriteStatement creates a new write statement node in the abstract syntax tree.
func NewWriteStatement(expression Expression, index int) Statement {
	return newWriteStatement(expression, index)
}

// NewCallStatement creates a new call statement node in the abstract syntax tree.
func NewCallStatement(procedure Expression, index int) Statement {
	return newCallStatement(procedure, index)
}

// NewIfStatement creates a new if-then statement node in the abstract syntax tree.
func NewIfStatement(condition Expression, statement Statement, index int) Statement {
	return newIfStatement(condition, statement, index)
}

// NewWhileStatement creates a new while-do statement node in the abstract syntax tree.
func NewWhileStatement(condition Expression, statement Statement, index int) Statement {
	return newWhileStatement(condition, statement, index)
}

// NewCompoundStatement creates a compound statement node in the abstract syntax tree.
func NewCompoundStatement(statements []Statement) Statement {
	return newCompoundStatement(statements)
}

// Walk traverses an abstract syntax tree in a specific order and calls the visitor or the visit function for each node.
func Walk(parent Node, order TraversalOrder, visitor any, visit func(node Node, visitor any)) error {
	return walk(parent, order, visitor, visit)
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
