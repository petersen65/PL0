// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package ast

import (
	"errors"
	"fmt"
	"strings"
)

// Create a new block node in the abstract syntax tree.
func newBlock(name string, depth int32, scope *Scope, procedures []Block, statement Statement, source *SourceDescription) Block {
	block := &BlockNode{Name: name, Depth: depth, Scope: scope, Procedures: procedures, Statement: statement, Source: source}

	for _, procedure := range block.Procedures {
		procedure.SetParent(block)
	}

	statement.SetParent(block)
	source.SetParent(block)
	return block
}

// Create a new literal-reference node in the abstract syntax tree.
func newLiteralReference(value any, dataType DataType, source *SourceDescription) Expression {
	literal := &LiteralNode{Value: value, DataType: dataType, Source: source}
	source.SetParent(literal)
	return literal
}

// Create a new constant-usage node in the abstract syntax tree.
func newConstantReference(entry *Symbol, source *SourceDescription) Expression {
	constant := &ConstantNode{Symbol: entry, Source: source}
	entry.SetParent(constant)
	source.SetParent(constant)
	return constant
}

// Create a new variable-usage node in the abstract syntax tree.
func newVariableReference(entry *Symbol, source *SourceDescription) Expression {
	variable := &VariableNode{Symbol: entry, Source: source}
	entry.SetParent(variable)
	source.SetParent(variable)
	return variable
}

// Create a new unary operation node in the abstract syntax tree.
func newUnaryOperation(operation UnaryOperator, operand Expression, source *SourceDescription) Expression {
	unary := &UnaryOperationNode{Operation: operation, Operand: operand, Source: source}
	operand.SetParent(unary)
	source.SetParent(unary)
	return unary
}

// Create a new binary operation node in the abstract syntax tree.
func newBinaryOperation(operation BinaryOperator, left, right Expression, source *SourceDescription) Expression {
	binary := &BinaryOperationNode{Operation: operation, Left: left, Right: right, Source: source}
	left.SetParent(binary)
	right.SetParent(binary)
	source.SetParent(binary)
	return binary
}

// Create a new conditional operation node in the abstract syntax tree.
func newConditionalOperation(operation RelationalOperator, left, right Expression, source *SourceDescription) Expression {
	conditional := &ConditionalOperationNode{Operation: operation, Left: left, Right: right, Source: source}
	left.SetParent(conditional)
	right.SetParent(conditional)
	source.SetParent(conditional)
	return conditional
}

// Create a new assignment statement node in the abstract syntax tree.
func newAssignmentStatement(entry *Symbol, expression Expression, source *SourceDescription) Statement {
	assignment := &AssignmentStatementNode{Symbol: entry, Expression: expression, Source: source}
	entry.SetParent(assignment)
	expression.SetParent(assignment)
	source.SetParent(assignment)
	return assignment
}

// Create a new read statement node in the abstract syntax tree.
func newReadStatement(entry *Symbol, source *SourceDescription) Statement {
	read := &ReadStatementNode{Symbol: entry, Source: source}
	entry.SetParent(read)
	source.SetParent(read)
	return read
}

// Create a new write statement node in the abstract syntax tree.
func newWriteStatement(expression Expression, source *SourceDescription) Statement {
	write := &WriteStatementNode{Expression: expression, Source: source}
	expression.SetParent(write)
	source.SetParent(write)
	return write
}

// Create a new call statement node in the abstract syntax tree.
func newCallStatement(entry *Symbol, source *SourceDescription) Statement {
	call := &CallStatementNode{Symbol: entry, Source: source}
	entry.SetParent(call)
	source.SetParent(call)
	return call
}

// Create a new if-then statement node in the abstract syntax tree.
func newIfStatement(condition Expression, statement Statement, source *SourceDescription) Statement {
	ifStmt := &IfStatementNode{Condition: condition, Statement: statement, Source: source}
	condition.SetParent(ifStmt)
	statement.SetParent(ifStmt)
	source.SetParent(ifStmt)
	return ifStmt
}

// Create a new while-do statement node in the abstract syntax tree.
func newWhileStatement(condition Expression, statement Statement, source *SourceDescription) Statement {
	whileStmt := &WhileStatementNode{Condition: condition, Statement: statement, Source: source}
	condition.SetParent(whileStmt)
	statement.SetParent(whileStmt)
	source.SetParent(whileStmt)
	return whileStmt
}

// Create a new compound statement node in the abstract syntax tree.
func newCompoundStatement(statements []Statement, source *SourceDescription) Statement {
	compound := &CompoundStatementNode{Statements: statements, Source: source}

	for _, statement := range compound.Statements {
		statement.SetParent(compound)
	}

	source.SetParent(compound)
	return compound
}

// Set the parent Node of the symbol entry node.
func (s *Symbol) SetParent(parent Node) {
	s.ParentNode = parent
}

// String of the symbol entry node.
func (s *Symbol) String() string {
	return fmt.Sprintf("symbol(%v,%v:%v)", KindNames[s.Kind], s.Name, s.Depth)
}

// Parent node of the symbol entry node.
func (s *Symbol) Parent() Node {
	return s.ParentNode
}

// Children nodes of the symbol entry node.
func (s *Symbol) Children() []Node {
	return make([]Node, 0)
}

// Accept the visitor for the symbol entry node.
func (s *Symbol) Accept(visitor Visitor) {
	visitor.VisitSymbol(s)
}

// Set the parent Node of the source description node.
func (s *SourceDescription) SetParent(parent Node) {
	s.ParentNode = parent
}

// String of the source description node.
func (s *SourceDescription) String() string {
	if len(s.Lines) == 0 {
		return fmt.Sprintf("source %v-%v", s.From, s.To)
	}

	if len(s.Lines) == 1 {
		return fmt.Sprintf("source %v", strings.Trim(string(s.Lines[0].Code), " \t\n"))
	}

	return fmt.Sprintf(
		"source %v ... %v",
		strings.Trim(string(s.Lines[0].Code), " \t\n"),
		strings.Trim(string(s.Lines[len(s.Lines)-1].Code), " \t\n"))
}

// Parent node of the source description node.
func (s *SourceDescription) Parent() Node {
	return s.ParentNode
}

// Children nodes of the source description node.
func (s *SourceDescription) Children() []Node {
	return make([]Node, 0)
}

// Accept the visitor for the source description node.
func (s *SourceDescription) Accept(visitor Visitor) {
	visitor.VisitSourceDescription(s)
}

// Set the parent Node of the block node.
func (b *BlockNode) SetParent(parent Node) {
	b.ParentNode = parent
}

// String of the block node.
func (b *BlockNode) String() string {
	return fmt.Sprintf("block %v:%v", b.Name, b.Depth)
}

// Parent node of the block node.
func (b *BlockNode) Parent() Node {
	return b.ParentNode
}

// Children nodes of the block node.
func (b *BlockNode) Children() []Node {
	children := make([]Node, 0, len(*b.Scope.SymbolTable)+len(b.Procedures)+2)

	for _, entry := range *b.Scope.SymbolTable {
		children = append(children, entry)
	}

	for _, procedure := range b.Procedures {
		children = append(children, procedure)
	}

	return append(children, b.Statement, b.Source)
}

// BlockString returns the string representation of the block.
func (b *BlockNode) BlockString() string {
	return b.String()
}

// Accept the visitor for the block node.
func (b *BlockNode) Accept(visitor Visitor) {
	visitor.VisitBlock(b)
}

// Set the parent Node of the literal node.
func (e *LiteralNode) SetParent(parent Node) {
	e.ParentNode = parent
}

// String of the literal node.
func (e *LiteralNode) String() string {
	switch e.DataType {
	case Integer64:
		return fmt.Sprintf("literal(%v:i64)", e.Value)

	default:
		return fmt.Sprintf("literal(%v)", e.Value)
	}
}

// Parent node of the literal node.
func (e *LiteralNode) Parent() Node {
	return e.ParentNode
}

// Children nodes of the literal node.
func (e *LiteralNode) Children() []Node {
	return []Node{e.Source}
}

// ExpressionString returns the string representation of the literal expression.
func (e *LiteralNode) ExpressionString() string {
	return e.String()
}

// Accept the visitor for the literal node.
func (e *LiteralNode) Accept(visitor Visitor) {
	visitor.VisitLiteral(e)
}

// Set the parent Node of the constant node.
func (e *ConstantNode) SetParent(parent Node) {
	e.ParentNode = parent
}

// String of the constant.
func (e *ConstantNode) String() string {
	return fmt.Sprintf("constant(%v=%v)", e.Symbol.Name, e.Symbol.Value)
}

// Parent node of the constant node.
func (e *ConstantNode) Parent() Node {
	return e.ParentNode
}

// Children nodes of the constant node.
func (e *ConstantNode) Children() []Node {
	return []Node{e.Symbol, e.Source}
}

// ExpressionString returns the string representation of the constant expression.
func (e *ConstantNode) ExpressionString() string {
	return e.String()
}

// Accept the visitor for the constant node.
func (e *ConstantNode) Accept(visitor Visitor) {
	visitor.VisitConstant(e)
}

// Set the parent Node of the variable node.
func (e *VariableNode) SetParent(parent Node) {
	e.ParentNode = parent
}

// String of the variable node.
func (e *VariableNode) String() string {
	return fmt.Sprintf("variable(%v:%v)", e.Symbol.Name, e.Symbol.Depth)
}

// Parent node of the variable node.
func (e *VariableNode) Parent() Node {
	return e.ParentNode
}

// Children nodes of the variable node.
func (e *VariableNode) Children() []Node {
	return []Node{e.Symbol, e.Source}
}

// ExpressionString returns the string representation of the variable expression.
func (e *VariableNode) ExpressionString() string {
	return e.String()
}

// Accept the visitor for the variable node.
func (e *VariableNode) Accept(visitor Visitor) {
	visitor.VisitVariable(e)
}

// Set the parent Node of the unary operation node.
func (e *UnaryOperationNode) SetParent(parent Node) {
	e.ParentNode = parent
}

// String of the unary operation node.
func (e *UnaryOperationNode) String() string {
	switch e.Operation {
	case Odd:
		return "odd"

	case Negate:
		return "negate"
	}

	return "unknown"
}

// Parent node of the unary operation node.
func (e *UnaryOperationNode) Parent() Node {
	return e.ParentNode
}

// Children nodes of the unary operation node.
func (e *UnaryOperationNode) Children() []Node {
	return []Node{e.Operand, e.Source}
}

// ExpressionString returns the string representation of the unary operation expression.
func (e *UnaryOperationNode) ExpressionString() string {
	return e.String()
}

// Accept the visitor for the unary operation node.
func (e *UnaryOperationNode) Accept(visitor Visitor) {
	visitor.VisitUnaryOperation(e)
}

// Set the parent Node of the binary operation node.
func (e *BinaryOperationNode) SetParent(parent Node) {
	e.ParentNode = parent
}

// String of the binary operation node.
func (e *BinaryOperationNode) String() string {
	switch e.Operation {
	case Plus:
		return "addition"

	case Minus:
		return "subtraction"

	case Times:
		return "multiplication"

	case Divide:
		return "division"
	}

	return "unknown"
}

// Parent node of the binary operation node.
func (e *BinaryOperationNode) Parent() Node {
	return e.ParentNode
}

// Children nodes of the binary operation node.
func (e *BinaryOperationNode) Children() []Node {
	return []Node{e.Left, e.Right, e.Source}
}

// ExpressionString returns the string representation of the binary operation expression.
func (e *BinaryOperationNode) ExpressionString() string {
	return e.String()
}

// Accept the visitor for the binary operation node.
func (e *BinaryOperationNode) Accept(visitor Visitor) {
	visitor.VisitBinaryOperation(e)
}

// Set the parent Node of the conditional operation node.
func (e *ConditionalOperationNode) SetParent(parent Node) {
	e.ParentNode = parent
}

// String of the conditional operation node.
func (e *ConditionalOperationNode) String() string {
	switch e.Operation {
	case Equal:
		return "equal"

	case NotEqual:
		return "not equal"

	case Less:
		return "less"

	case LessEqual:
		return "less equal"

	case Greater:
		return "greater"

	case GreaterEqual:
		return "greater equal"
	}

	return "unknown"
}

// Parent node of the conditional operation node.
func (e *ConditionalOperationNode) Parent() Node {
	return e.ParentNode
}

// Children nodes of the conditional operation node.
func (e *ConditionalOperationNode) Children() []Node {
	return []Node{e.Left, e.Right, e.Source}
}

// ConditionString returns the string representation of the conditional operation expression.
func (e *ConditionalOperationNode) ExpressionString() string {
	return e.String()
}

// Accept the visitor for the conditional operation node.
func (e *ConditionalOperationNode) Accept(visitor Visitor) {
	visitor.VisitConditionalOperation(e)
}

// Set the parent Node of the assignment statement node.
func (s *AssignmentStatementNode) SetParent(parent Node) {
	s.ParentNode = parent
}

// String of the assignment statement node.
func (s *AssignmentStatementNode) String() string {
	return "assignment"
}

// Parent node of the assignment statement node.
func (s *AssignmentStatementNode) Parent() Node {
	return s.ParentNode
}

// Children nodes of the assignment statement node.
func (s *AssignmentStatementNode) Children() []Node {
	return []Node{s.Symbol, s.Expression, s.Source}
}

// StatementString returns the string representation of the assignment statement.
func (s *AssignmentStatementNode) StatementString() string {
	return s.String()
}

// Accept the visitor for the assignment statement node.
func (s *AssignmentStatementNode) Accept(visitor Visitor) {
	visitor.VisitAssignmentStatement(s)
}

// Set the parent Node of the read statement node.
func (s *ReadStatementNode) SetParent(parent Node) {
	s.ParentNode = parent
}

// String of the read statement node.
func (s *ReadStatementNode) String() string {
	return "read"
}

// Parent node of the read statement node.
func (s *ReadStatementNode) Parent() Node {
	return s.ParentNode
}

// Children nodes of the read statement node.
func (s *ReadStatementNode) Children() []Node {
	return []Node{s.Symbol, s.Source}
}

// StatementString returns the string representation of the read statement.
func (s *ReadStatementNode) StatementString() string {
	return s.String()
}

// Accept the visitor for the read statement node.
func (s *ReadStatementNode) Accept(visitor Visitor) {
	visitor.VisitReadStatement(s)
}

// Set the parent Node of the write statement node.
func (s *WriteStatementNode) SetParent(parent Node) {
	s.ParentNode = parent
}

// String of the write statement node.
func (s *WriteStatementNode) String() string {
	return "write"
}

// Parent node of the write statement node.
func (s *WriteStatementNode) Parent() Node {
	return s.ParentNode
}

// Children nodes of the write statement node.
func (s *WriteStatementNode) Children() []Node {
	return []Node{s.Expression, s.Source}
}

// StatementString returns the string representation of the write statement.
func (s *WriteStatementNode) StatementString() string {
	return s.String()
}

// Accept the visitor for the write statement node.
func (s *WriteStatementNode) Accept(visitor Visitor) {
	visitor.VisitWriteStatement(s)
}

// Set the parent Node of the call statement node.
func (s *CallStatementNode) SetParent(parent Node) {
	s.ParentNode = parent
}

// String of the call statement node.
func (s *CallStatementNode) String() string {
	return "call"
}

// Parent node of the call statement node.
func (s *CallStatementNode) Parent() Node {
	return s.ParentNode
}

// Children nodes of the call statement node.
func (s *CallStatementNode) Children() []Node {
	return []Node{s.Symbol, s.Source}
}

// StatementString returns the string representation of the call statement.
func (s *CallStatementNode) StatementString() string {
	return s.String()
}

// Accept the visitor for the call statement node.
func (s *CallStatementNode) Accept(visitor Visitor) {
	visitor.VisitCallStatement(s)
}

// Set the parent Node of the if-then statement node.
func (s *IfStatementNode) SetParent(parent Node) {
	s.ParentNode = parent
}

// String of the if-then statement node.
func (s *IfStatementNode) String() string {
	return "if"
}

// Parent node of the if-then statement node.
func (s *IfStatementNode) Parent() Node {
	return s.ParentNode
}

// Children nodes of the if-then statement node.
func (s *IfStatementNode) Children() []Node {
	return []Node{s.Condition, s.Statement, s.Source}
}

// StatementString returns the string representation of the if-then statement.
func (s *IfStatementNode) StatementString() string {
	return s.String()
}

// Accept the visitor for the if-then statement node.
func (s *IfStatementNode) Accept(visitor Visitor) {
	visitor.VisitIfStatement(s)
}

// Set the parent Node of the while-do statement node.
func (s *WhileStatementNode) SetParent(parent Node) {
	s.ParentNode = parent
}

// String of the while-do statement node.
func (s *WhileStatementNode) String() string {
	return "while"
}

// Parent node of the while-do statement node.
func (s *WhileStatementNode) Parent() Node {
	return s.ParentNode
}

// Children nodes of the while-do statement node.
func (s *WhileStatementNode) Children() []Node {
	return []Node{s.Condition, s.Statement, s.Source}
}

// StatementString returns the string representation of the while statement.
func (s *WhileStatementNode) StatementString() string {
	return s.String()
}

// Accept the visitor for the while-do statement node.
func (s *WhileStatementNode) Accept(visitor Visitor) {
	visitor.VisitWhileStatement(s)
}

// Set the parent Node of the compound statement node.
func (s *CompoundStatementNode) SetParent(parent Node) {
	s.ParentNode = parent
}

// String of the compound statement node.
func (s *CompoundStatementNode) String() string {
	return "compound"
}

// Parent node of the compound statement node.
func (s *CompoundStatementNode) Parent() Node {
	return s.ParentNode
}

// Children nodes of the compound statement node.
func (s *CompoundStatementNode) Children() []Node {
	children := make([]Node, 0, len(s.Statements)+1)

	for _, statement := range s.Statements {
		children = append(children, statement)
	}

	return append(children, s.Source)
}

// StatementString returns the string representation of the compound statement.
func (s *CompoundStatementNode) StatementString() string {
	return s.String()
}

// Accept the visitor for the compound statement node.
func (s *CompoundStatementNode) Accept(visitor Visitor) {
	visitor.VisitCompoundStatement(s)
}

// Walk traverses a abstract syntax tree in a specific order and calls the visitor for each node.
// Example tree:
//
//	    A
//	   / \
//	  B   C
//	 / \   \
//	D   E   F
func walk(parent Node, order TraversalOrder, visitor Visitor) error {
	switch order {
	// Pre-order traversal is a method of traversing a tree data structure in which each node is processed before (pre) its child nodes.
	// This is commonly used in certain tree-related algorithms, including those for parsing expressions and serializing or deserializing trees.
	//
	// The order of operations for pre-order traversal is:
	//   1. Visit the parent node
	//   2. Traverse the childs left to right in pre-order
	// A pre-order traversal would visit the nodes in the following order: A, B, D, E, C, F.
	case PreOrder:
		parent.Accept(visitor)

		for _, child := range parent.Children() {
			Walk(child, order, visitor)
		}

	// In-order traversal is a method of traversing a tree data structure in which each node is processed between (in) its child nodes.
	// This traversal method visits the nodes of a binary search tree in ascending order (if the tree is correctly formed).
	// This can be useful for operations like printing out the nodes of the tree in sorted order.
	//
	// The order of operations for in-order traversal is:
	//   1. Traverse the left subtree in in-order
	//   2. Visit the parent node
	//   3. Traverse the right subtree in in-order
	// An in-order traversal would visit the nodes in the following order: D, B, E, A, C, F.
	case InOrder:
		if len(parent.Children()) != 2 {
			return errors.New("in-order traversal requires exactly two children")
		}

		Walk(parent.Children()[0], order, visitor)
		parent.Accept(visitor)
		Walk(parent.Children()[1], order, visitor)

	// Post-order traversal is a method of traversing a tree data structure in which each node is processed after (post) its child nodes.
	// This method is often used when you need to ensure that a node is processed after its descendants, such as when deleting or freeing nodes of a tree.
	//
	// The order of operations for post-order traversal is:
	//   1. Traverse the childs left to right in post-order
	//   2. Visit the parent node
	// A post-order traversal would visit the nodes in the following order: D, E, B, F, C, A.
	case PostOrder:
		for _, child := range parent.Children() {
			Walk(child, order, visitor)
		}

		parent.Accept(visitor)

	// Level-order traversal is a method of traversing a tree data structure in which each node is processed level by level.
	// This method is often used when you need to process the nodes of a tree in a breadth-first manner.
	// In a level-order traversal, all nodes at the current depth (or "level") are processed before moving on to nodes at the next depth.
	// This is different from pre-order, in-order, and post-order traversals, which are all types of depth-first traversals.
	//
	// The order of operations for level-order traversal is:
	//   1. Visit the parent node
	//   2. Visit all the nodes at the next depth (i.e., the children of the parent node)
	//   3. Repeat step 2 for each subsequent depth, visiting all nodes at each depth before moving on to the next
	// A level-order traversal would visit the nodes in the following order: A, B, C, D, E, F.
	case LevelOrder:
		queue := make([]Node, 0)
		queue = append(queue, parent)

		for len(queue) > 0 {
			node := queue[0]                          // get the first node in the queue
			queue = queue[1:]                         // remove the first node from the queue
			node.Accept(visitor)                      // visit the node
			queue = append(queue, node.Children()...) // add the node's children to the end of the queue
		}
	}

	return nil
}
