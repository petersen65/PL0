// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"errors"
	"fmt"
)

// Create a new block node in the abstract syntax tree.
func newBlock(name string, depth int32, scope *Scope, declarations []Declaration, statement Statement) Block {
	block := &BlockNode{Name: name, Depth: depth, Scope: scope, Declarations: declarations, Statement: statement}

	for _, declaration := range block.Declarations {
		declaration.SetParent(block)
	}

	statement.SetParent(block)
	return block
}

// Create a new constant declaration node in the abstract syntax tree.
func newConstantDeclaration(name string, value any, dataType DataType, scope *Scope, index int) Declaration {
	return &ConstantDeclarationNode{Name: name, Value: value, DataType: dataType, Scope: scope, TokenStreamIndex: index}
}

// Create a new variable declaration node in the abstract syntax tree.
func newVariableDeclaration(name string, dataType DataType, scope *Scope, index int) Declaration {
	return &VariableDeclarationNode{Name: name, DataType: dataType, Scope: scope, TokenStreamIndex: index}
}

// Create a new procedure declaration node in the abstract syntax tree.
func newProcedureDeclaration(name string, block Block, scope *Scope, index int) Declaration {
	declaration := &ProcedureDeclarationNode{Name: name, Block: block, Scope: scope, TokenStreamIndex: index}
	block.SetParent(declaration)
	return declaration
}

// Create a new literal node in the abstract syntax tree.
func newLiteral(value any, dataType DataType) Expression {
	return &LiteralNode{Value: value, DataType: dataType}
}

// Create a new constant-reference node in the abstract syntax tree.
func newConstantReference(name string, scope *Scope, index int) Expression {
	return &ConstantReferenceNode{Name: name, Scope: scope, TokenStreamIndex: index}
}

// Create a new variable-reference node in the abstract syntax tree.
func newVariableReference(name string, scope *Scope, index int) Expression {
	return &VariableReferenceNode{Name: name, Scope: scope, TokenStreamIndex: index}
}

// Create a new procedure-reference node in the abstract syntax tree.
func newProcedureReference(name string, scope *Scope, index int) Statement {
	return &ProcedureReferenceNode{Name: name, Scope: scope, TokenStreamIndex: index}
}

// Create a new unary operation node in the abstract syntax tree.
func newUnaryOperation(operation UnaryOperator, operand Expression) Expression {
	unary := &UnaryOperationNode{Operation: operation, Operand: operand}
	operand.SetParent(unary)
	return unary
}

// Create a new binary operation node in the abstract syntax tree.
func newBinaryOperation(operation BinaryOperator, left, right Expression) Expression {
	binary := &BinaryOperationNode{Operation: operation, Left: left, Right: right}
	left.SetParent(binary)
	right.SetParent(binary)
	return binary
}

// Create a new conditional operation node in the abstract syntax tree.
func newConditionalOperation(operation RelationalOperator, left, right Expression) Expression {
	conditional := &ConditionalOperationNode{Operation: operation, Left: left, Right: right}
	left.SetParent(conditional)
	right.SetParent(conditional)
	return conditional
}

// Create a new assignment statement node in the abstract syntax tree.
func newAssignmentStatement(variable, expression Expression) Statement {
	assignment := &AssignmentStatementNode{Variable: variable, Expression: expression}
	variable.SetParent(assignment)
	expression.SetParent(assignment)
	return assignment
}

// Create a new read statement node in the abstract syntax tree.
func newReadStatement(variable Expression) Statement {
	read := &ReadStatementNode{Variable: variable}
	variable.SetParent(read)
	return read
}

// Create a new write statement node in the abstract syntax tree.
func newWriteStatement(expression Expression) Statement {
	write := &WriteStatementNode{Expression: expression}
	expression.SetParent(write)
	return write
}

// Create a new call statement node in the abstract syntax tree.
func newCallStatement(procedure Statement) Statement {
	call := &CallStatementNode{Procedure: procedure}
	procedure.SetParent(call)
	return call
}

// Create a new if-then statement node in the abstract syntax tree.
func newIfStatement(condition Expression, statement Statement) Statement {
	ifStmt := &IfStatementNode{Condition: condition, Statement: statement}
	condition.SetParent(ifStmt)
	statement.SetParent(ifStmt)
	return ifStmt
}

// Create a new while-do statement node in the abstract syntax tree.
func newWhileStatement(condition Expression, statement Statement) Statement {
	whileStmt := &WhileStatementNode{Condition: condition, Statement: statement}
	condition.SetParent(whileStmt)
	statement.SetParent(whileStmt)
	return whileStmt
}

// Create a new compound statement node in the abstract syntax tree.
func newCompoundStatement(statements []Statement) Statement {
	compound := &CompoundStatementNode{Statements: statements}

	for _, statement := range compound.Statements {
		statement.SetParent(compound)
	}

	return compound
}

// Set the parent Node of the block node.
func (b *BlockNode) SetParent(parent Node) {
	b.ParentNode = parent
}

// String of the block node.
func (b *BlockNode) String() string {
	return fmt.Sprintf("block n=%v,d=%v", b.Name, b.Depth)
}

// Parent node of the block node.
func (b *BlockNode) Parent() Node {
	return b.ParentNode
}

// Children nodes of the block node.
func (b *BlockNode) Children() []Node {
	children := make([]Node, 0, len(b.Declarations)+1)

	for _, declaration := range b.Declarations {
		children = append(children, declaration)
	}

	return append(children, b.Statement)
}

// BlockString returns the string representation of the block.
func (b *BlockNode) BlockString() string {
	return b.String()
}

// Accept the visitor for the block node.
func (b *BlockNode) Accept(visitor Visitor) {
	visitor.VisitBlock(b)
}

// Set the parent Node of the constant declaration node.
func (d *ConstantDeclarationNode) SetParent(parent Node) {
	d.ParentNode = parent
}

// String of the constant declaration node.
func (d *ConstantDeclarationNode) String() string {
	switch d.DataType {
	case Integer64:
		return fmt.Sprintf("declaration(%v,n=%v,v=%v,t=i64)", KindNames[Constant], d.Name, d.Value)

	default:
		panic("abstract syntax tree error: unknown constant data type")
	}
}

// Parent node of the constant declaration node.
func (d *ConstantDeclarationNode) Parent() Node {
	return d.ParentNode
}

// Children nodes of the constant declaration node.
func (d *ConstantDeclarationNode) Children() []Node {
	return make([]Node, 0)
}

// DeclarationString returns the string representation of the constant declaration.
func (d *ConstantDeclarationNode) DeclarationString() string {
	return d.String()
}

// Accept the visitor for the constant declaration node.
func (d *ConstantDeclarationNode) Accept(visitor Visitor) {
	visitor.VisitConstantDeclaration(d)
}

// Set the parent Node of the variable declaration node.
func (d *VariableDeclarationNode) SetParent(parent Node) {
	d.ParentNode = parent
}

// String of the variable declaration node.
func (d *VariableDeclarationNode) String() string {
	switch d.DataType {
	case Integer64:
		return fmt.Sprintf("declaration(%v,n=%v,o=%v,t=i64)", KindNames[Variable], d.Name, d.Offset)

	default:
		panic("abstract syntax tree error: unknown variable data type")
	}
}

// Parent node of the variable declaration node.
func (d *VariableDeclarationNode) Parent() Node {
	return d.ParentNode
}

// Children nodes of the variable declaration node.
func (d *VariableDeclarationNode) Children() []Node {
	return make([]Node, 0)
}

// DeclarationString returns the string representation of the variable declaration.
func (d *VariableDeclarationNode) DeclarationString() string {
	return d.String()
}

// Accept the visitor for the variable declaration node.
func (d *VariableDeclarationNode) Accept(visitor Visitor) {
	visitor.VisitVariableDeclaration(d)
}

// Set the parent Node of the procedure declaration node.
func (d *ProcedureDeclarationNode) SetParent(parent Node) {
	d.ParentNode = parent
}

// String of the procedure declaration node.
func (d *ProcedureDeclarationNode) String() string {
	return fmt.Sprintf("declaration(%v,n=%v,a=%v)", KindNames[Procedure], d.Name, d.Address)
}

// Parent node of the procedure declaration node.
func (d *ProcedureDeclarationNode) Parent() Node {
	return d.ParentNode
}

// Children nodes of the procedure declaration node.
func (d *ProcedureDeclarationNode) Children() []Node {
	return []Node{d.Block}
}

// DeclarationString returns the string representation of the procedure declaration.
func (d *ProcedureDeclarationNode) DeclarationString() string {
	return d.String()
}

// Accept the visitor for the procedure declaration node.
func (d *ProcedureDeclarationNode) Accept(visitor Visitor) {
	visitor.VisitProcedureDeclaration(d)
}

// Set the parent Node of the literal node.
func (e *LiteralNode) SetParent(parent Node) {
	e.ParentNode = parent
}

// String of the literal node.
func (e *LiteralNode) String() string {
	switch e.DataType {
	case Integer64:
		return fmt.Sprintf("literal(v=%v,t=i64)", e.Value)

	default:
		panic("abstract syntax tree error: unknown literal data type")
	}
}

// Parent node of the literal node.
func (e *LiteralNode) Parent() Node {
	return e.ParentNode
}

// Children nodes of the literal node.
func (e *LiteralNode) Children() []Node {
	return make([]Node, 0)
}

// ExpressionString returns the string representation of the literal expression.
func (e *LiteralNode) ExpressionString() string {
	return e.String()
}

// Accept the visitor for the literal node.
func (e *LiteralNode) Accept(visitor Visitor) {
	visitor.VisitLiteral(e)
}

// Set the parent Node of the constant reference node.
func (e *ConstantReferenceNode) SetParent(parent Node) {
	e.ParentNode = parent
}

// String of the constant reference node.
func (e *ConstantReferenceNode) String() string {
	return fmt.Sprintf("reference(%v,n=%v)", KindNames[Constant], e.Name)
}

// Parent node of the constant reference node.
func (e *ConstantReferenceNode) Parent() Node {
	return e.ParentNode
}

// Children nodes of the constant reference node.
func (e *ConstantReferenceNode) Children() []Node {
	return make([]Node, 0)
}

// ExpressionString returns the string representation of the constant expression.
func (e *ConstantReferenceNode) ExpressionString() string {
	return e.String()
}

// Accept the visitor for the constant reference node.
func (e *ConstantReferenceNode) Accept(visitor Visitor) {
	visitor.VisitConstantReference(e)
}

// Set the parent Node of the variable reference node.
func (e *VariableReferenceNode) SetParent(parent Node) {
	e.ParentNode = parent
}

// String of the variable reference node.
func (e *VariableReferenceNode) String() string {
	return fmt.Sprintf("reference(%v,n=%v)", KindNames[Variable], e.Name)
}

// Parent node of the variable reference node.
func (e *VariableReferenceNode) Parent() Node {
	return e.ParentNode
}

// Children nodes of the variable reference node.
func (e *VariableReferenceNode) Children() []Node {
	return make([]Node, 0)
}

// ExpressionString returns the string representation of the variable expression.
func (e *VariableReferenceNode) ExpressionString() string {
	return e.String()
}

// Accept the visitor for the variable reference node.
func (e *VariableReferenceNode) Accept(visitor Visitor) {
	visitor.VisitVariableReference(e)
}

// Set the parent Node of the procedure reference node.
func (e *ProcedureReferenceNode) SetParent(parent Node) {
	e.ParentNode = parent
}

// String of the procedure reference node.
func (e *ProcedureReferenceNode) String() string {
	return fmt.Sprintf("reference(%v,n=%v)", KindNames[Procedure], e.Name)
}

// Parent node of the procedure reference node.
func (e *ProcedureReferenceNode) Parent() Node {
	return e.ParentNode
}

// Children nodes of the procedure reference node.
func (e *ProcedureReferenceNode) Children() []Node {
	return make([]Node, 0)
}

// StatementString returns the string representation of the call statement.
func (e *ProcedureReferenceNode) StatementString() string {
	return e.String()
}

// Accept the visitor for the procedure reference node.
func (e *ProcedureReferenceNode) Accept(visitor Visitor) {
	visitor.VisitProcedureReference(e)
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

	default:
		panic("abstract syntax tree error: unknown unary operation")
	}
}

// Parent node of the unary operation node.
func (e *UnaryOperationNode) Parent() Node {
	return e.ParentNode
}

// Children nodes of the unary operation node.
func (e *UnaryOperationNode) Children() []Node {
	return []Node{e.Operand}
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

	default:
		panic("abstract syntax tree error: unknown binary operation")
	}
}

// Parent node of the binary operation node.
func (e *BinaryOperationNode) Parent() Node {
	return e.ParentNode
}

// Children nodes of the binary operation node.
func (e *BinaryOperationNode) Children() []Node {
	return []Node{e.Left, e.Right}
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

	default:
		panic("abstract syntax tree error: unknown conditional operation")
	}
}

// Parent node of the conditional operation node.
func (e *ConditionalOperationNode) Parent() Node {
	return e.ParentNode
}

// Children nodes of the conditional operation node.
func (e *ConditionalOperationNode) Children() []Node {
	return []Node{e.Left, e.Right}
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
	return []Node{s.Variable, s.Expression}
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
	return []Node{s.Variable}
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
	return []Node{s.Expression}
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
	return []Node{s.Procedure}
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
	return []Node{s.Condition, s.Statement}
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
	return []Node{s.Condition, s.Statement}
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
	children := make([]Node, 0, len(s.Statements))

	for _, statement := range s.Statements {
		children = append(children, statement)
	}

	return children
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
