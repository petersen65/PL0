// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"fmt"
	"io"
	"strings"

	eh "github.com/petersen65/pl0/v3/errors"
	sym "github.com/petersen65/pl0/v3/symbol"
	ts "github.com/petersen65/pl0/v3/typesystem"
)

// Allows the detection of empty constants because of parsing errors. They should be ignored in all compiler phases.
const emptyConstantName = "@constant"

// Separator for the string representation of a bit-mask.
const bitMaskSeparator = "|"

var (
	// Map kind of nodes to their string representation.
	nodeKindNames = map[NodeKind]string{
		KindBlock:                "block",
		KindConstantDeclaration:  "constant",
		KindVariableDeclaration:  "variable",
		KindProcedureDeclaration: "procedure",
		KindLiteral:              "literal",
		KindIdentifierUse:        "use",
		KindUnaryOperation:       "unary",
		KindBinaryOperation:      "binary",
		KindComparisonOperation:  "comparison",
		KindAssignmentStatement:  "assignment",
		KindReadStatement:        "read",
		KindWriteStatement:       "write",
		KindCallStatement:        "call",
		KindIfStatement:          "if",
		KindWhileStatement:       "while",
		KindCompoundStatement:    "compound",
	}

	// Map usage modes to their string representation.
	usageNames = map[Usage]string{
		Read:    "read",
		Write:   "write",
		Execute: "execute",
	}
)

// Create a new literal node in the abstract syntax tree.
func newLiteral(value any, dataType ts.PrimitiveDataType, scope sym.Scope[Declaration], index int) Expression {
	return &LiteralNode{
		TypeName:         nodeKindNames[KindLiteral],
		Value:            value,
		DataType:         dataType,
		Scope:            scope,
		TokenStreamIndex: index,
	}
}

// Create a new identifier-use node in the abstract syntax tree.
func newIdentifierUse(name string, scope sym.Scope[Declaration], context sym.Entry, index int) Expression {
	return &IdentifierUseNode{
		TypeName:         nodeKindNames[KindIdentifierUse],
		Name:             name,
		Scope:            scope,
		Context:          context,
		TokenStreamIndex: index,
	}
}

// Create a new unary operation node in the abstract syntax tree.
func newUnaryOperation(operation UnaryOperator, operand Expression, index int) Expression {
	unary := &UnaryOperationNode{
		TypeName:         nodeKindNames[KindUnaryOperation],
		Operation:        operation,
		Operand:          operand,
		TokenStreamIndex: index,
	}

	operand.SetParent(unary)
	return unary
}

// Create a new binary operation node in the abstract syntax tree.
func newBinaryOperation(operation BinaryOperator, left, right Expression, index int) Expression {
	binary := &BinaryOperationNode{
		TypeName:         nodeKindNames[KindBinaryOperation],
		Operation:        operation,
		Left:             left,
		Right:            right,
		TokenStreamIndex: index,
	}

	left.SetParent(binary)
	right.SetParent(binary)
	return binary
}

// Create a new comparison operation node in the abstract syntax tree.
func newComparisonOperation(operation ComparisonOperator, left, right Expression, index int) Expression {
	comparison := &ComparisonOperationNode{
		TypeName:         nodeKindNames[KindComparisonOperation],
		Operation:        operation,
		Left:             left,
		Right:            right,
		TokenStreamIndex: index,
	}

	left.SetParent(comparison)
	right.SetParent(comparison)
	return comparison
}

// Create a new assignment statement node in the abstract syntax tree.
func newAssignmentStatement(variable, expression Expression, beginIndex, endIndex int) Statement {
	assignment := &AssignmentStatementNode{
		TypeName:              nodeKindNames[KindAssignmentStatement],
		Variable:              variable,
		Expression:            expression,
		TokenStreamIndexBegin: beginIndex,
		TokenStreamIndexEnd:   endIndex,
	}

	variable.SetParent(assignment)
	expression.SetParent(assignment)
	return assignment
}

// Create a new read statement node in the abstract syntax tree.
func newReadStatement(variable Expression, beginIndex, endIndex int) Statement {
	read := &ReadStatementNode{
		TypeName:              nodeKindNames[KindReadStatement],
		Variable:              variable,
		TokenStreamIndexBegin: beginIndex,
		TokenStreamIndexEnd:   endIndex,
	}

	variable.SetParent(read)
	return read
}

// Create a new write statement node in the abstract syntax tree.
func newWriteStatement(expression Expression, beginIndex, endIndex int) Statement {
	write := &WriteStatementNode{
		TypeName:              nodeKindNames[KindWriteStatement],
		Expression:            expression,
		TokenStreamIndexBegin: beginIndex,
		TokenStreamIndexEnd:   endIndex,
	}

	expression.SetParent(write)
	return write
}

// Create a new call statement node in the abstract syntax tree.
func newCallStatement(procedure Expression, beginIndex, endIndex int) Statement {
	call := &CallStatementNode{
		TypeName:              nodeKindNames[KindCallStatement],
		Procedure:             procedure,
		TokenStreamIndexBegin: beginIndex,
		TokenStreamIndexEnd:   endIndex,
	}

	procedure.SetParent(call)
	return call
}

// Create a new if-then statement node in the abstract syntax tree.
func newIfStatement(condition Expression, statement Statement, beginIndex, endIndex int) Statement {
	ifStmt := &IfStatementNode{
		TypeName:              nodeKindNames[KindIfStatement],
		Condition:             condition,
		Statement:             statement,
		TokenStreamIndexBegin: beginIndex,
		TokenStreamIndexEnd:   endIndex,
	}

	condition.SetParent(ifStmt)
	statement.SetParent(ifStmt)
	return ifStmt
}

// Create a new while-do statement node in the abstract syntax tree.
func newWhileStatement(condition Expression, statement Statement, beginIndex, endIndex int) Statement {
	whileStmt := &WhileStatementNode{
		TypeName:              nodeKindNames[KindWhileStatement],
		Condition:             condition,
		Statement:             statement,
		TokenStreamIndexBegin: beginIndex,
		TokenStreamIndexEnd:   endIndex,
	}

	condition.SetParent(whileStmt)
	statement.SetParent(whileStmt)
	return whileStmt
}

// Create a new compound statement node in the abstract syntax tree.
func newCompoundStatement(statements []Statement, beginIndex, endIndex int) Statement {
	compound := &CompoundStatementNode{
		TypeName:              nodeKindNames[KindCompoundStatement],
		Statements:            statements,
		TokenStreamIndexBegin: beginIndex,
		TokenStreamIndexEnd:   endIndex,
	}

	for _, statement := range compound.Statements {
		statement.SetParent(compound)
	}

	return compound
}

// String representation of a usage mode bit-mask.
func (u Usage) String() string {
	var parts []string

	for usage, name := range usageNames {
		if u&usage != 0 {
			parts = append(parts, name)
		}
	}

	return strings.Join(parts, bitMaskSeparator)
}

// Kind of node for each node in the AST.
func (n *CommonNode) Kind() NodeKind {
	return n.NodeKind
}

// Parent node for each node in the AST.
func (n *CommonNode) Parent() Node {
	return n.ParentNode
}

// Set the parent node for each node in the AST.
func (n *CommonNode) SetParent(parent Node) {
	n.ParentNode = parent
}

// All usages of the declared identifier in expressions.
func (n *DeclarationNode) Usage() []Expression {
	return n.IdentifierUsage
}

// Token stream index of the identifier declaration node.
func (n *DeclarationNode) Index() int {
	return n.TokenStreamIndex
}

// Kind of the literal node.
func (e *LiteralNode) Kind() NodeKind {
	return KindLiteral
}

// Set the parent Node of the literal node.
func (e *LiteralNode) SetParent(parent Node) {
	e.ParentNode = parent
}

// String of the literal node.
func (e *LiteralNode) String() string {
	return fmt.Sprintf("literal(value=%v,type=%v)", e.Value, e.DataType)
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

// Index returns the token stream index of the literal node.
func (e *LiteralNode) Index() int {
	return e.TokenStreamIndex
}

// Accept the visitor for the literal node.
func (e *LiteralNode) Accept(visitor Visitor) {
	visitor.VisitLiteral(e)
}

// Kind of the identifier-use node.
func (u *IdentifierUseNode) Kind() NodeKind {
	return KindIdentifierUse
}

// Set the parent Node of the identifier-use node.
func (u *IdentifierUseNode) SetParent(parent Node) {
	u.ParentNode = parent
}

// String of the identifier-use node.
func (u *IdentifierUseNode) String() string {
	if symbol := u.Scope.Lookup(u.Name); symbol != nil {
		switch symbol.Kind {
		case sym.ConstantEntry:
			return fmt.Sprintf("use(kind=%v,name=%v,value=%v,usage=%v)",
				symbol.Kind, symbol.Name, symbol.Declaration.(*ConstantDeclarationNode).Value, u.Use)

		case sym.VariableEntry:
			return fmt.Sprintf("use(kind=%v,name=%v,usage=%v)", symbol.Kind, symbol.Name, u.Use)

		case sym.ProcedureEntry:
			return fmt.Sprintf("use(kind=%v,name=%v,usage=%v)", symbol.Kind, symbol.Name, u.Use)

		default:
			panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownSymbolKind, nil, nil))
		}
	}

	// if the symbol is not found, return a generic identifier-use string
	return fmt.Sprintf("use(kind=unknown,name=%v,usage=%v)", u.Name, u.Use)
}

// Parent node of the identifier-use node.
func (u *IdentifierUseNode) Parent() Node {
	return u.ParentNode
}

// Children nodes of the identifier-use node.
func (u *IdentifierUseNode) Children() []Node {
	return make([]Node, 0)
}

// Index returns the token stream index of the identifier-use node.
func (u *IdentifierUseNode) Index() int {
	return u.TokenStreamIndex
}

// ExpressionString returns the string representation of an identifier-use.
func (u *IdentifierUseNode) ExpressionString() string {
	return u.String()
}

// Accept the visitor for the identifier-use node.
func (u *IdentifierUseNode) Accept(visitor Visitor) {
	visitor.VisitIdentifierUse(u)
}

// Kind of the unary operation node.
func (e *UnaryOperationNode) Kind() NodeKind {
	return KindUnaryOperation
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
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownUnaryOperation, nil, nil))
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

// Index returns the token stream index of the unary operation node.
func (e *UnaryOperationNode) Index() int {
	return e.TokenStreamIndex
}

// ExpressionString returns the string representation of the unary operation expression.
func (e *UnaryOperationNode) ExpressionString() string {
	return e.String()
}

// Accept the visitor for the unary operation node.
func (e *UnaryOperationNode) Accept(visitor Visitor) {
	visitor.VisitUnaryOperation(e)
}

// Kind of the binary operation node.
func (e *BinaryOperationNode) Kind() NodeKind {
	return KindBinaryOperation
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
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownBinaryOperation, nil, nil))
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

// Index returns the token stream index of the binary operation node.
func (e *BinaryOperationNode) Index() int {
	return e.TokenStreamIndex
}

// ExpressionString returns the string representation of the binary operation expression.
func (e *BinaryOperationNode) ExpressionString() string {
	return e.String()
}

// Accept the visitor for the binary operation node.
func (e *BinaryOperationNode) Accept(visitor Visitor) {
	visitor.VisitBinaryOperation(e)
}

// Kind of the comparison operation node.
func (e *ComparisonOperationNode) Kind() NodeKind {
	return KindComparisonOperation
}

// Set the parent Node of the comparison operation node.
func (e *ComparisonOperationNode) SetParent(parent Node) {
	e.ParentNode = parent
}

// String of the comparison operation node.
func (e *ComparisonOperationNode) String() string {
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
		panic(eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Fatal, unknownComparisonOperation, nil, nil))

	}
}

// Parent node of the comparison operation node.
func (e *ComparisonOperationNode) Parent() Node {
	return e.ParentNode
}

// Children nodes of the comparison operation node.
func (e *ComparisonOperationNode) Children() []Node {
	return []Node{e.Left, e.Right}
}

// Index returns the token stream index of the comparison operation node.
func (e *ComparisonOperationNode) Index() int {
	return e.TokenStreamIndex
}

// ExpressionString returns the string representation of the comparison operation expression.
func (e *ComparisonOperationNode) ExpressionString() string {
	return e.String()
}

// Accept the visitor for the comparison operation node.
func (e *ComparisonOperationNode) Accept(visitor Visitor) {
	visitor.VisitComparisonOperation(e)
}

// Kind of the assignment statement node.
func (s *AssignmentStatementNode) Kind() NodeKind {
	return KindAssignmentStatement
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

// Index returns the token stream index of the assignment statement node.
func (s *AssignmentStatementNode) Index() int {
	return s.TokenStreamIndexBegin
}

// IndexPair returns the token stream index begin/end pair of the assignment statement node.
func (s *AssignmentStatementNode) IndexPair() (int, int) {
	return s.TokenStreamIndexBegin, s.TokenStreamIndexEnd
}

// StatementString returns the string representation of the assignment statement.
func (s *AssignmentStatementNode) StatementString() string {
	return s.String()
}

// Accept the visitor for the assignment statement node.
func (s *AssignmentStatementNode) Accept(visitor Visitor) {
	visitor.VisitAssignmentStatement(s)
}

// Kind of the read statement node.
func (s *ReadStatementNode) Kind() NodeKind {
	return KindReadStatement
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

// Index returns the token stream index of the read statement node.
func (s *ReadStatementNode) Index() int {
	return s.TokenStreamIndexBegin
}

// IndexPair returns the token stream index begin/end pair of the read statement node.
func (s *ReadStatementNode) IndexPair() (int, int) {
	return s.TokenStreamIndexBegin, s.TokenStreamIndexEnd
}

// StatementString returns the string representation of the read statement.
func (s *ReadStatementNode) StatementString() string {
	return s.String()
}

// Accept the visitor for the read statement node.
func (s *ReadStatementNode) Accept(visitor Visitor) {
	visitor.VisitReadStatement(s)
}

// Kind of the write statement node.
func (s *WriteStatementNode) Kind() NodeKind {
	return KindWriteStatement
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

// Index returns the token stream index of the write statement node.
func (s *WriteStatementNode) Index() int {
	return s.TokenStreamIndexBegin
}

// IndexPair returns the token stream index begin/end pair of the write statement node.
func (s *WriteStatementNode) IndexPair() (int, int) {
	return s.TokenStreamIndexBegin, s.TokenStreamIndexEnd
}

// Accept the visitor for the write statement node.
func (s *WriteStatementNode) Accept(visitor Visitor) {
	visitor.VisitWriteStatement(s)
}

// Kind of the call statement node.
func (s *CallStatementNode) Kind() NodeKind {
	return KindCallStatement
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

// Index returns the token stream index of the call statement node.
func (s *CallStatementNode) Index() int {
	return s.TokenStreamIndexBegin
}

// IndexPair returns the token stream index begin/end pair of the call statement node.
func (s *CallStatementNode) IndexPair() (int, int) {
	return s.TokenStreamIndexBegin, s.TokenStreamIndexEnd
}

// StatementString returns the string representation of the call statement.
func (s *CallStatementNode) StatementString() string {
	return s.String()
}

// Accept the visitor for the call statement node.
func (s *CallStatementNode) Accept(visitor Visitor) {
	visitor.VisitCallStatement(s)
}

// Kind of the if-then statement node.
func (s *IfStatementNode) Kind() NodeKind {
	return KindIfStatement
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

// Index returns the token stream index of the if-then statement node.
func (s *IfStatementNode) Index() int {
	return s.TokenStreamIndexBegin
}

// IndexPair returns the token stream index begin/end pair of the if-then statement node.
func (s *IfStatementNode) IndexPair() (int, int) {
	return s.TokenStreamIndexBegin, s.TokenStreamIndexEnd
}

// StatementString returns the string representation of the if-then statement.
func (s *IfStatementNode) StatementString() string {
	return s.String()
}

// Accept the visitor for the if-then statement node.
func (s *IfStatementNode) Accept(visitor Visitor) {
	visitor.VisitIfStatement(s)
}

// Kind of the while-do statement node.
func (s *WhileStatementNode) Kind() NodeKind {
	return KindWhileStatement
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

// Index returns the token stream index of the while-do statement node.
func (s *WhileStatementNode) Index() int {
	return s.TokenStreamIndexBegin
}

// IndexPair returns the token stream index begin/end pair of the while-do statement node.
func (s *WhileStatementNode) IndexPair() (int, int) {
	return s.TokenStreamIndexBegin, s.TokenStreamIndexEnd
}

// StatementString returns the string representation of the while statement.
func (s *WhileStatementNode) StatementString() string {
	return s.String()
}

// Accept the visitor for the while-do statement node.
func (s *WhileStatementNode) Accept(visitor Visitor) {
	visitor.VisitWhileStatement(s)
}

// Kind of the compound statement node.
func (s *CompoundStatementNode) Kind() NodeKind {
	return KindCompoundStatement
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

// Index returns the token stream index of the compound statement node.
func (s *CompoundStatementNode) Index() int {
	return s.TokenStreamIndexBegin
}

// IndexPair returns the token stream index begin/end pair of the compound statement node.
func (s *CompoundStatementNode) IndexPair() (int, int) {
	return s.TokenStreamIndexBegin, s.TokenStreamIndexEnd
}

// StatementString returns the string representation of the compound statement.
func (s *CompoundStatementNode) StatementString() string {
	return s.String()
}

// Accept the visitor for the compound statement node.
func (s *CompoundStatementNode) Accept(visitor Visitor) {
	visitor.VisitCompoundStatement(s)
}

// Walk traverses a abstract syntax tree in a specific order and calls the visitor or the visit function for each node.
// Example tree:
//
//	    A
//	   / \
//	  B   C
//	 / \   \
//	D   E   F
func walk(parent Node, order TraversalOrder, visitor any, visit func(node Node, visitor any)) error {
	// check preconditions for walking the tree and return an error if any are violated
	if parent == nil {
		return eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Error, cannotWalkOnNilNode, nil, nil)
	} else if visitor == nil && visit == nil {
		return eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Error, walkRequiresVisitorOrFunction, nil, nil)
	} else if _, ok := visitor.(Visitor); !ok && visit == nil {
		return eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Error, walkRequiresInterfaceOrFunction, nil, nil)
	}

	// filter out empty constants
	if constant, ok := parent.(*ConstantDeclarationNode); ok && constant.Name == emptyConstantName {
		return nil
	}

	// switch on the order of traversal
	switch order {
	// Pre-order traversal is a method of traversing a tree data structure in which each node is processed before (pre) its child nodes.
	// This is commonly used in certain tree-related algorithms, including those for parsing expressions and serializing or deserializing trees.
	//
	// The order of operations for pre-order traversal is:
	//   1. Visit the parent node
	//   2. Traverse the childs left to right in pre-order
	// A pre-order traversal would visit the nodes in the following order: A, B, D, E, C, F.
	case PreOrder:
		// call the visit function or visit the parent node
		if visit != nil {
			visit(parent, visitor)
		} else {
			parent.Accept(visitor.(Visitor))
		}

		// traverse the childs left to right in pre-order
		for _, child := range parent.Children() {
			walk(child, order, visitor, visit)
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
			return eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Error, inOrderRequiresTwoChildren, nil, nil)
		}

		// traverse the left subtree in in-order
		walk(parent.Children()[0], order, visitor, visit)

		// call the visit function or visit the parent node
		if visit != nil {
			visit(parent, visitor)
		} else {
			parent.Accept(visitor.(Visitor))
		}

		// traverse the right subtree in in-order
		walk(parent.Children()[1], order, visitor, visit)

	// Post-order traversal is a method of traversing a tree data structure in which each node is processed after (post) its child nodes.
	// This method is often used when you need to ensure that a node is processed after its descendants, such as when deleting or freeing nodes of a tree.
	//
	// The order of operations for post-order traversal is:
	//   1. Traverse the childs left to right in post-order
	//   2. Visit the parent node
	// A post-order traversal would visit the nodes in the following order: D, E, B, F, C, A.
	case PostOrder:
		// traverse the childs left to right in post-order
		for _, child := range parent.Children() {
			walk(child, order, visitor, visit)
		}

		// call the visit function or visit the parent node
		if visit != nil {
			visit(parent, visitor)
		} else {
			parent.Accept(visitor.(Visitor))
		}

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
			node := queue[0]  // get the first node in the queue
			queue = queue[1:] // remove the first node from the queue

			// call the visit function or visit the node
			if visit != nil {
				visit(node, visitor)
			} else {
				node.Accept(visitor.(Visitor))
			}

			queue = append(queue, node.Children()...) // add the node's children to the end of the queue
		}
	}

	return nil
}

// Print the abstract syntax tree to the specified writer by recursively traversing the tree in pre-order.
func printAbstractSyntaxTree(node Node, indent string, last bool, print io.Writer) error {
	if _, err := fmt.Fprintf(print, "%v+- %v\n", indent, node); err != nil {
		return err
	}

	if last {
		indent += "   "
	} else {
		indent += "|  "
	}

	for i, child := range node.Children() {
		if err := printAbstractSyntaxTree(child, indent, i == len(node.Children())-1, print); err != nil {
			return err
		}
	}

	return nil
}
