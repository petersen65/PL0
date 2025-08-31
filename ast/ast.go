// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package ast implements the abstract syntax tree (AST) produced by the parser for the programming language PL/0.
package ast

import (
	"io"

	exp "github.com/petersen65/pl0/v3/export"
	sym "github.com/petersen65/pl0/v3/symbol"
	tok "github.com/petersen65/pl0/v3/token"
	ts "github.com/petersen65/pl0/v3/typesystem"
)

// EmptyConstantName allows the detection of empty constants because of parsing errors. They should be ignored in all compiler phases.
const EmptyConstantName = "@constant"

// Types of nodes in the abstract syntax tree.
const (
	BlockType NodeType = iota
	ConstantDeclarationType
	VariableDeclarationType
	ProcedureDeclarationType
	LiteralType
	IdentifierUseType
	UnaryOperationType
	BinaryOperationType
	ComparisonOperationType
	AssignmentStatementType
	ReadStatementType
	WriteStatementType
	CallStatementType
	IfStatementType
	WhileStatementType
	CompoundStatementType
)

// Operators with one operand.
const (
	Odd UnaryOperator = iota
	Negate
)

// Operators with two operands.
const (
	Plus BinaryOperator = iota
	Minus
	Times
	Divide
)

// Operators for comparison.
const (
	Equal ComparisonOperator = iota
	NotEqual
	Less
	LessEqual
	Greater
	GreaterEqual
)

// Usage mode of an identifier as bit-mask.
const (
	Read Usage = 1 << iota
	Write
	Execute
)

// Search parent block nodes in the abstract syntax tree.
const (
	CurrentBlock BlockSearchMode = iota
	RootBlock
)

// Traverse the abstract syntax tree in specific orders.
const (
	PreOrder TraversalOrder = iota
	InOrder
	PostOrder
	LevelOrder
)

type (
	// Type of a node in the abstract syntax tree.
	NodeType int

	// Take one operand and perform an operation on it.
	UnaryOperator int

	// Take two operands and perform an operation on them.
	BinaryOperator int

	// Take two operands and perform a comparison on them.
	ComparisonOperator int

	// Usage mode of an identifier (bit-mask).
	Usage uint64

	// Search mode for block nodes in the abstract syntax tree.
	BlockSearchMode int

	// Traversal order for the abstract syntax tree.
	TraversalOrder int

	// Block node represents a block in the AST.
	BlockNode struct {
		TypeName     string                 `json:"type"`         // type name of the block node
		ParentNode   Node                   `json:"-"`            // parent node of the block
		Depth        int32                  `json:"depth"`        // block nesting depth
		Scope        sym.Scope[Declaration] `json:"scope"`        // scope with the symbol table of the block
		Declarations []Declaration          `json:"declarations"` // all declarations of the block
		Closure      []Declaration          `json:"closure"`      // all captured variable declarations from lexical parents of the block
		Statement    Statement              `json:"statement"`    // statement of the block
	}

	// ConstantDeclaration node represents a constant declaration in the AST.
	ConstantDeclarationNode struct {
		TypeName         string                 `json:"type"`               // type name of the constant declaration node
		ParentNode       Node                   `json:"-"`                  // parent node of the constant declaration
		Name             string                 `json:"name"`               // name of the constant
		Value            any                    `json:"value"`              // value of constant
		DataType         ts.PrimitiveDataType   `json:"data_type"`          // datatype of the constant
		Scope            sym.Scope[Declaration] `json:"scope"`              // scope of the constant declaration
		Usage            []Expression           `json:"usage"`              // all usages of the constant
		TokenStreamIndex int                    `json:"token_stream_index"` // index of the token in the token stream
	}

	// VariableDeclaration node represents a variable declaration in the AST.
	VariableDeclarationNode struct {
		TypeName         string                 `json:"type"`               // type name of the variable declaration node
		ParentNode       Node                   `json:"-"`                  // parent node of the variable declaration
		Name             string                 `json:"name"`               // name of the variable
		DataType         ts.PrimitiveDataType   `json:"data_type"`          // datatype of the variable
		Scope            sym.Scope[Declaration] `json:"scope"`              // scope of the variable declaration
		Usage            []Expression           `json:"usage"`              // all usages of the variable
		TokenStreamIndex int                    `json:"token_stream_index"` // index of the token in the token stream
	}

	// ProcedureDeclaration node represents a procedure declaration in the AST.
	ProcedureDeclarationNode struct {
		TypeName         string                 `json:"type"`               // type name of the procedure declaration node
		ParentNode       Node                   `json:"-"`                  // parent node of the procedure declaration
		Name             string                 `json:"name"`               // name of the procedure
		Block            Block                  `json:"block"`              // block of the procedure
		Scope            sym.Scope[Declaration] `json:"scope"`              // scope of the procedure declaration
		Usage            []Expression           `json:"usage"`              // all usages of the procedure
		TokenStreamIndex int                    `json:"token_stream_index"` // index of the token in the token stream
	}

	// Literal node represents the usage of a literal value in the AST.
	LiteralNode struct {
		TypeName         string                 `json:"type"`               // type name of the literal node
		ParentNode       Node                   `json:"-"`                  // parent node of the literal
		Value            any                    `json:"value"`              // literal value
		DataType         ts.PrimitiveDataType   `json:"data_type"`          // datatype of the literal
		Scope            sym.Scope[Declaration] `json:"scope"`              // scope in which the literal is used
		TokenStreamIndex int                    `json:"token_stream_index"` // index of the token in the token stream
	}

	// IdentifierUseNode represents the usage of an identifier in the AST.
	IdentifierUseNode struct {
		TypeName         string                 `json:"type"`               // type name of the identifier usage node
		ParentNode       Node                   `json:"-"`                  // parent node of the identifier usage
		Name             string                 `json:"name"`               // name of the identifier
		Scope            sym.Scope[Declaration] `json:"scope"`              // scope in which the identifier is used
		Context          sym.Entry              `json:"context"`            // context of the identifier
		Use              Usage                  `json:"use"`                // usage mode of the identifier
		TokenStreamIndex int                    `json:"token_stream_index"` // index of the token in the token stream
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

	// ComparisonOperationNode node represents a comparison operation in the AST.
	ComparisonOperationNode struct {
		TypeName         string             `json:"type"`               // type name of the comparison operation node
		ParentNode       Node               `json:"-"`                  // parent node of the comparison operation
		Operation        ComparisonOperator `json:"operation"`          // comparison operation
		Left             Expression         `json:"left"`               // left operand of the comparison operation
		Right            Expression         `json:"right"`              // right operand of the comparison operation
		TokenStreamIndex int                `json:"token_stream_index"` // index of the token in the token stream
	}

	// AssignmentStatement node represents an assignment statement in the AST.
	AssignmentStatementNode struct {
		TypeName              string     `json:"type"`                     // type name of the assignment statement node
		ParentNode            Node       `json:"-"`                        // parent node of the assignment statement
		Variable              Expression `json:"variable"`                 // variable use on the left side of the assignment statement
		Expression            Expression `json:"expression"`               // expression on the right side of the assignment statement
		TokenStreamIndexBegin int        `json:"token_stream_index_begin"` // begin index of the token in the token stream
		TokenStreamIndexEnd   int        `json:"token_stream_index_end"`   // end index of the token in the token stream
	}

	// ReadStatement node represents a read statement in the AST.
	ReadStatementNode struct {
		TypeName              string     `json:"type"`                     // type name of the read statement node
		ParentNode            Node       `json:"-"`                        // parent node of the read statement
		Variable              Expression `json:"variable"`                 // variable use of the read statement
		TokenStreamIndexBegin int        `json:"token_stream_index_begin"` // begin index of the token in the token stream
		TokenStreamIndexEnd   int        `json:"token_stream_index_end"`   // end index of the token in the token stream
	}

	// WriteStatement node represents a write statement in the AST.
	WriteStatementNode struct {
		TypeName              string     `json:"type"`                     // type name of the write statement node
		ParentNode            Node       `json:"-"`                        // parent node of the write statement
		Expression            Expression `json:"expression"`               // expression of the write statement
		TokenStreamIndexBegin int        `json:"token_stream_index_begin"` // begin index of the token in the token stream
		TokenStreamIndexEnd   int        `json:"token_stream_index_end"`   // end index of the token in the token stream
	}

	// CallStatement node represents a call statement in the AST.
	CallStatementNode struct {
		TypeName              string     `json:"type"`                     // type name of the call statement node
		ParentNode            Node       `json:"-"`                        // parent node of the call statement
		Procedure             Expression `json:"procedure"`                // procedure use of the call statement
		TokenStreamIndexBegin int        `json:"token_stream_index_begin"` // begin index of the token in the token stream
		TokenStreamIndexEnd   int        `json:"token_stream_index_end"`   // end index of the token in the token stream
	}

	// IfStatement node represents an if-then statement in the AST.
	IfStatementNode struct {
		TypeName              string     `json:"type"`                     // type name of the if-then statement node
		ParentNode            Node       `json:"-"`                        // parent node of the if-then statement
		Condition             Expression `json:"condition"`                // if-condition of the if-then statement
		Statement             Statement  `json:"statement"`                // then-statement of the if-then statement
		TokenStreamIndexBegin int        `json:"token_stream_index_begin"` // begin index of the token in the token stream
		TokenStreamIndexEnd   int        `json:"token_stream_index_end"`   // end index of the token in the token stream
	}

	// WhileStatement node represents a while-do statement in the AST.
	WhileStatementNode struct {
		TypeName              string     `json:"type"`                     // type name of the while-do statement node
		ParentNode            Node       `json:"-"`                        // parent node of the while-do statement
		Condition             Expression `json:"condition"`                // while-condition of the while-do statement
		Statement             Statement  `json:"statement"`                // do-statement of the while-do statement
		TokenStreamIndexBegin int        `json:"token_stream_index_begin"` // begin index of the token in the token stream
		TokenStreamIndexEnd   int        `json:"token_stream_index_end"`   // end index of the token in the token stream
	}

	// CompoundStatement node represents a begin-end statement in the AST.
	CompoundStatementNode struct {
		TypeName              string      `json:"type"`                     // type name of the compound statement node
		ParentNode            Node        `json:"-"`                        // parent node of the begin-end compound statement
		Statements            []Statement `json:"statements"`               // all statements of the begin-end compound statement
		TokenStreamIndexBegin int         `json:"token_stream_index_begin"` // begin index of the token in the token stream
		TokenStreamIndexEnd   int         `json:"token_stream_index_end"`   // end index of the token in the token stream
	}

	// A node in the abstract syntax tree.
	Node interface {
		Type() NodeType
		SetParent(node Node)
		Parent() Node
		Children() []Node
		Index() int
		String() string
		Accept(visitor Visitor)
	}

	// A block represented as an abstract syntax tree.
	Block interface {
		Node
		BlockString() string
		Print(print io.Writer, args ...any) error
		Export(format exp.ExportFormat, print io.Writer) error
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
		IndexPair() (int, int)
		StatementString() string
	}

	// A visitor is an interface for visiting nodes in the abstract syntax tree. It allows all the methods for a parser phase to be grouped in a single visitor struct.
	// The visitor design pattern allows implementing double dispatch for traversing the abstract syntax tree. Each parser phase method is chosen based on:
	//   - the dynamic type of the object (the AST node) determines the method to be called, and
	//   - the dynamic type of the argument (the visitor) determines the behavior of the method.
	Visitor interface {
		VisitBlock(block *BlockNode)
		VisitConstantDeclaration(declaration *ConstantDeclarationNode)
		VisitVariableDeclaration(declaration *VariableDeclarationNode)
		VisitProcedureDeclaration(declaration *ProcedureDeclarationNode)
		VisitLiteral(literal *LiteralNode)
		VisitIdentifierUse(use *IdentifierUseNode)
		VisitUnaryOperation(operation *UnaryOperationNode)
		VisitBinaryOperation(operation *BinaryOperationNode)
		VisitComparisonOperation(operation *ComparisonOperationNode)
		VisitAssignmentStatement(assignment *AssignmentStatementNode)
		VisitReadStatement(read *ReadStatementNode)
		VisitWriteStatement(write *WriteStatementNode)
		VisitCallStatement(call *CallStatementNode)
		VisitIfStatement(ifStmt *IfStatementNode)
		VisitWhileStatement(whileStmt *WhileStatementNode)
		VisitCompoundStatement(compound *CompoundStatementNode)
	}
)

// NewBlock creates a new block node in the abstract syntax tree.
func NewBlock(depth int32, scope sym.Scope[Declaration], declarations []Declaration, statement Statement) Block {
	return newBlock(depth, scope, declarations, statement)
}

// An empty declaration is a 0 constant with special name, should only be used in the context of parser errors, and is free from any side-effect.
func NewEmptyDeclaration() Declaration {
	return newConstantDeclaration(EmptyConstantName, int64(0), ts.Integer64, sym.NewEmptyScope[Declaration](), tok.NoTokenStreamIndex)
}

// An empty expression is a 0 literal, should only be used in the context of parser errors, and is free from any side-effect.
func NewEmptyExpression() Expression {
	return newLiteral(int64(0), ts.Integer64, sym.NewEmptyScope[Declaration](), tok.NoTokenStreamIndex)
}

// An empty statement does not generate code, should only be used in the context of parser errors, and is free from any side-effect.
func NewEmptyStatement() Statement {
	return newCompoundStatement(make([]Statement, 0), tok.NoTokenStreamIndex, tok.NoTokenStreamIndex)
}

// NewConstantDeclaration creates a new constant declaration node in the abstract syntax tree.
func NewConstantDeclaration(name string, value any, dataType ts.PrimitiveDataType, scope sym.Scope[Declaration], index int) Declaration {
	return newConstantDeclaration(name, value, dataType, scope, index)
}

// NewVariableDeclaration creates a new variable declaration node in the abstract syntax tree.
func NewVariableDeclaration(name string, dataType ts.PrimitiveDataType, scope sym.Scope[Declaration], index int) Declaration {
	return newVariableDeclaration(name, dataType, scope, index)
}

// NewProcedureDeclaration creates a new procedure declaration node in the abstract syntax tree.
func NewProcedureDeclaration(name string, block Block, scope sym.Scope[Declaration], index int) Declaration {
	return newProcedureDeclaration(name, block, scope, index)
}

// NewLiteral creates a new literal node in the abstract syntax tree.
func NewLiteral(value any, dataType ts.PrimitiveDataType, scope sym.Scope[Declaration], index int) Expression {
	return newLiteral(value, dataType, scope, index)
}

// NewIdentifierUse creates a new identifier-use node in the abstract syntax tree.
func NewIdentifierUse(name string, scope sym.Scope[Declaration], context sym.Entry, index int) Expression {
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

// NewComparisonOperation creates a new comparison operation node in the abstract syntax tree.
func NewComparisonOperation(operation ComparisonOperator, left, right Expression, index int) Expression {
	return newComparisonOperation(operation, left, right, index)
}

// NewAssignmentStatement creates a new assignment statement node in the abstract syntax tree.
func NewAssignmentStatement(variable, expression Expression, beginIndex, endIndex int) Statement {
	return newAssignmentStatement(variable, expression, beginIndex, endIndex)
}

// NewReadStatement creates a new read statement node in the abstract syntax tree.
func NewReadStatement(variable Expression, beginIndex, endIndex int) Statement {
	return newReadStatement(variable, beginIndex, endIndex)
}

// NewWriteStatement creates a new write statement node in the abstract syntax tree.
func NewWriteStatement(expression Expression, beginIndex, endIndex int) Statement {
	return newWriteStatement(expression, beginIndex, endIndex)
}

// NewCallStatement creates a new call statement node in the abstract syntax tree.
func NewCallStatement(procedure Expression, beginIndex, endIndex int) Statement {
	return newCallStatement(procedure, beginIndex, endIndex)
}

// NewIfStatement creates a new if-then statement node in the abstract syntax tree.
func NewIfStatement(condition Expression, statement Statement, beginIndex, endIndex int) Statement {
	return newIfStatement(condition, statement, beginIndex, endIndex)
}

// NewWhileStatement creates a new while-do statement node in the abstract syntax tree.
func NewWhileStatement(condition Expression, statement Statement, beginIndex, endIndex int) Statement {
	return newWhileStatement(condition, statement, beginIndex, endIndex)
}

// NewCompoundStatement creates a compound statement node in the abstract syntax tree.
func NewCompoundStatement(statements []Statement, beginIndex, endIndex int) Statement {
	return newCompoundStatement(statements, beginIndex, endIndex)
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

// Walk traverses an abstract syntax tree in a specific order and calls the visitor or the visit function for each node.
func Walk(parent Node, order TraversalOrder, visitor any, visit func(node Node, visitor any)) error {
	return walk(parent, order, visitor, visit)
}
