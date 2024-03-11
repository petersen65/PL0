// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package ast

type (
	// A node in the abstract syntax tree (with embedded public Node interface).
	node interface {
		Node
		setParent(node)
	}

	// The symbol entry (with embedded public Symbol struct).
	symbol struct {
		*Symbol
		parent node
	}

	// Describes a range of lines in the source code (with embedded public SourceDescription struct).
	sourceDescription struct {
		*SourceDescription
		parent node
	}

	// Block node represents a block in the AST.
	block struct {
		parent     node               // parent node of the block
		name       string             // name of the block that can be used for lookup in the symbol table
		depth      int32              // declaration nesting depth
		scope      *Scope             // scope with symbol table of the block that has its own outer scope chain
		procedures []Block            // nested procedures of the block
		statement  Statement          // statement of the block
		source     *sourceDescription // source description for the block node
	}

	// Literal node represents the usage of a literal value in the AST.
	literal struct {
		parent   node              // parent node of the literal
		value    any               // literal value
		dataType DataType          // data type of the literal
		source   sourceDescription // source description for the literal node
	}

	// Constant node represents the usage of a constant in the AST.
	constant struct {
		parent node              // parent node of the constant
		symbol symbol            // constant symbol entry
		source sourceDescription // source description for the constant node
	}

	// Variable node represents the usage of a variable in the AST.
	variable struct {
		parent node              // parent node of the variable
		symbol symbol            // variable symbol entry
		source sourceDescription // source description for the variable node
	}

	// UnaryOperation node represents a unary operation in the AST.
	unaryOperation struct {
		parent    node              // parent node of the unary operation
		operation UnaryOperator     // unary operation
		operand   Expression        // operand of the unary operation
		source    sourceDescription // source description for the unary operation node
	}

	// BinaryOperation node represents a binary operation in the AST.
	binaryOperation struct {
		parent    node              // parent node of the binary operation
		operation BinaryOperator    // binary operation
		left      Expression        // left operand of the binary operation
		right     Expression        // right operand of the binary operation
		source    sourceDescription // source description for the binary operation node
	}

	// ConditionalOperation node represents a conditional operation in the AST.
	conditionalOperation struct {
		parent    node               // parent node of the conditional
		operation RelationalOperator // conditional operation
		left      Expression         // left operand of the conditional operation
		right     Expression         // right operand of the conditional operation
		source    sourceDescription  // source description for the conditional operation node
	}

	// AssignmentStatement node represents an assignment statement in the AST.
	assignmentStatement struct {
		parent     node              // parent node of the assignment statement
		symbol     symbol            // variable symbol entry on the left side of the assignment statement
		expression Expression        // expression on the right side of the assignment statement
		source     sourceDescription // source description for the assignment statement node
	}

	// ReadStatement node represents a read statement in the AST.
	readStatement struct {
		parent node              // parent node of the read statement
		symbol symbol            // variable symbol entry of the read statement
		source sourceDescription // source description for the read statement node
	}

	// WriteStatement node represents a write statement in the AST.
	writeStatement struct {
		parent     node              // parent node of the write statement
		expression Expression        // expression of the write statement
		source     sourceDescription // source description for the write statement node
	}

	// CallStatement node represents a call statement in the AST.
	callStatement struct {
		parent node              // parent node of the call statement
		symbol symbol            // procedure symbol entry of the call statement
		source sourceDescription // source description for the call statement node
	}

	// IfStatement node represents an if-then statement in the AST.
	ifStatement struct {
		parent    node              // parent node of the if-then statement
		condition Expression        // if-condition of the if-then statement
		statement Statement         // then-statement of the if-then statement
		source    sourceDescription // source description for the if-then statement node
	}

	// WhileStatement node represents a while-do statement in the AST.
	whileStatement struct {
		parent    node              // parent node of the while-do statement
		condition Expression        // while-condition of the while-do statement
		statement Statement         // do-statement of the while-do statement
		source    sourceDescription // source description for the while-do statement node
	}

	// CompoundStatement node represents a begin-end statement in the AST.
	compoundStatement struct {
		parent     node              // parent node of the begin-end compound statement
		statements []Statement       // all statements of the begin-end compound statement
		source     sourceDescription // source description for the begin-end statement node
	}
)

// Create a new block node in the abstract syntax tree.
func newBlock(name string, depth int32, scope *Scope, procedures []Block, statement Statement, source *SourceDescription) Block {
	parent := new(block)

	for _, procedure := range procedures {
		procedure.(*block).parent = parent
	}

	statement.(node).setParent(parent)

	parent.name = name
	parent.depth = depth
	parent.scope = scope
	parent.procedures = procedures
	parent.statement = statement
	parent.source = &sourceDescription{source, parent}

	return parent
}

// Create a new literal-usage node in the abstract syntax tree.
func newLiteralReference(value any, dataType DataType, source *SourceDescription) Expression {
	parent := new(literal)

	parent.value = value
	parent.dataType = dataType
	parent.source = sourceDescription{source, parent}

	return parent
}

// Create a new constant-usage node in the abstract syntax tree.
func newConstantReference(entry *Symbol, source *SourceDescription) Expression {
	parent := new(constant)

	parent.symbol = symbol{entry, parent}
	parent.source = sourceDescription{source, parent}

	return parent
}

// Create a new variable-usage node in the abstract syntax tree.
func newVariableReference(entry *Symbol, source *SourceDescription) Expression {
	parent := new(variable)

	parent.symbol = symbol{entry, parent}
	parent.source = sourceDescription{source, parent}

	return parent
}

// Create a new unary operation node in the abstract syntax tree.
func newUnaryOperation(operation UnaryOperator, operand Expression, source *SourceDescription) Expression {
	parent := new(unaryOperation)

	operand.(node).setParent(parent)

	parent.operation = operation
	parent.operand = operand
	parent.source = sourceDescription{source, parent}

	return parent
}

// Create a new binary operation node in the abstract syntax tree.
func newBinaryOperation(operation BinaryOperator, left, right Expression, source *SourceDescription) Expression {
	parent := new(binaryOperation)

	left.(node).setParent(parent)
	right.(node).setParent(parent)

	parent.operation = operation
	parent.left = left
	parent.right = right
	parent.source = sourceDescription{source, parent}

	return parent
}

// Create a new conditional operation node in the abstract syntax tree.
func newConditionalOperation(operation RelationalOperator, left, right Expression, source *SourceDescription) Expression {
	parent := new(conditionalOperation)

	left.(node).setParent(parent)
	right.(node).setParent(parent)

	parent.operation = operation
	parent.left = left
	parent.right = right
	parent.source = sourceDescription{source, parent}

	return parent
}

// Create a new assignment statement node in the abstract syntax tree.
func newAssignmentStatement(entry *Symbol, expression Expression, source *SourceDescription) Statement {
	parent := new(assignmentStatement)

	expression.(node).setParent(parent)

	parent.symbol = symbol{entry, parent}
	parent.expression = expression
	parent.source = sourceDescription{source, parent}

	return parent
}

// Create a new read statement node in the abstract syntax tree.
func newReadStatement(entry *Symbol, source *SourceDescription) Statement {
	parent := new(readStatement)

	parent.symbol = symbol{entry, parent}
	parent.source = sourceDescription{source, parent}

	return parent
}

// Create a new write statement node in the abstract syntax tree.
func newWriteStatement(expression Expression, source *SourceDescription) Statement {
	parent := new(writeStatement)

	expression.(node).setParent(parent)

	parent.expression = expression
	parent.source = sourceDescription{source, parent}

	return parent
}

// Create a new call statement node in the abstract syntax tree.
func newCallStatement(entry *Symbol, source *SourceDescription) Statement {
	parent := new(callStatement)

	parent.symbol = symbol{entry, parent}
	parent.source = sourceDescription{source, parent}

	return parent
}

// Create a new if-then statement node in the abstract syntax tree.
func newIfStatement(condition Expression, statement Statement, source *SourceDescription) Statement {
	parent := new(ifStatement)

	condition.(node).setParent(parent)
	statement.(node).setParent(parent)

	parent.condition = condition
	parent.statement = statement
	parent.source = sourceDescription{source, parent}

	return parent
}

// Create a new while-do statement node in the abstract syntax tree.
func newWhileStatement(condition Expression, statement Statement, source *SourceDescription) Statement {
	parent := new(whileStatement)

	condition.(node).setParent(parent)
	statement.(node).setParent(parent)

	parent.condition = condition
	parent.statement = statement
	parent.source = sourceDescription{source, parent}

	return parent
}

// Create a new compound statement node in the abstract syntax tree.
func newCompoundStatement(statements []Statement, source *SourceDescription) Statement {
	parent := new(compoundStatement)

	for _, statement := range statements {
		statement.(node).setParent(parent)
	}

	parent.statements = statements
	parent.source = sourceDescription{source, parent}

	return parent
}

// Node type of the symbol entry.
func (s symbol) Type() NodeType {
	return SymbolNode
}

// Title of the symbol entry.
func (s symbol) Title() string {
	return s.String()
}

// Parent node of the symbol entry node.
func (s symbol) Parent() Node {
	return s.parent
}

// Children nodes of the symbol entry node.
func (s symbol) Children() []Node {
	return make([]Node, 0)
}

// Determine if the symbol entry is a declaration (otherwise it is a reference).
func (s symbol) isDeclaration() bool {
	return s.parent != nil && s.parent.Type() == BlockNode
}

// Node type of the source description.
func (s sourceDescription) Type() NodeType {
	return SourceDescriptionNode
}

// Title of the source description.
func (s sourceDescription) Title() string {
	return s.String()
}

// Parent node of the source description node.
func (s sourceDescription) Parent() Node {
	return s.parent
}

// Children nodes of the source description node.
func (s sourceDescription) Children() []Node {
	return make([]Node, 0)
}

// Node type of the literal.
func (e *literal) Type() NodeType {
	return LiteralNode
}

// Title of the literal.
func (e *literal) Title() string {
	return e.ExpressionString()
}

// Parent node of the literal node.
func (e *literal) Parent() Node {
	return e.parent
}

// Children nodes of the literal node.
func (e *literal) Children() []Node {
	return []Node{e.source}
}

// Set the parent node of the literal node.
func (e *literal) setParent(parent node) {
	e.parent = parent
}

// Node type of the constant.
func (e *constant) Type() NodeType {
	return ConstantNode
}

// Title of the constant.
func (e *constant) Title() string {
	return e.ExpressionString()
}

// Parent node of the constant node.
func (e *constant) Parent() Node {
	return e.parent
}

// Children nodes of the constant node.
func (e *constant) Children() []Node {
	return []Node{e.symbol, e.source}
}

// Set the parent node of the constant node.
func (e *constant) setParent(parent node) {
	e.parent = parent
}

// Node type of the variable.
func (e *variable) Type() NodeType {
	return VariableNode
}

// Title of the variable.
func (e *variable) Title() string {
	return e.ExpressionString()
}

// Parent node of the variable node.
func (e *variable) Parent() Node {
	return e.parent
}

// Children nodes of the variable node.
func (e *variable) Children() []Node {
	return []Node{e.symbol, e.source}
}

// Set the parent node of the variable node.
func (e *variable) setParent(parent node) {
	e.parent = parent
}

// Node type of the unary operation.
func (e *unaryOperation) Type() NodeType {
	return UnaryOperationNode
}

// Title of the unary operation.
func (e *unaryOperation) Title() string {
	return e.ExpressionString()
}

// Parent node of the unary operation node.
func (e *unaryOperation) Parent() Node {
	return e.parent
}

// Children nodes of the unary operation node.
func (e *unaryOperation) Children() []Node {
	return []Node{e.operand, e.source}
}

// Set the parent node of the unary operation node.
func (e *unaryOperation) setParent(parent node) {
	e.parent = parent
}

// Node type of the binary operation.
func (e *binaryOperation) Type() NodeType {
	return BinaryOperationNode
}

// Title of the binary operation.
func (e *binaryOperation) Title() string {
	return e.ExpressionString()
}

// Parent node of the binary operation node.
func (e *binaryOperation) Parent() Node {
	return e.parent
}

// Children nodes of the binary operation node.
func (e *binaryOperation) Children() []Node {
	return []Node{e.left, e.right, e.source}
}

// Set the parent node of the binary operation node.
func (e *binaryOperation) setParent(parent node) {
	e.parent = parent
}

// Node type of the conditional operation.
func (e *conditionalOperation) Type() NodeType {
	return ConditionalOperationNode
}

// Title of the conditional operation.
func (e *conditionalOperation) Title() string {
	return e.ExpressionString()
}

// Parent node of the conditional operation node.
func (e *conditionalOperation) Parent() Node {
	return e.parent
}

// Children nodes of the conditional operation node.
func (e *conditionalOperation) Children() []Node {
	return []Node{e.left, e.right, e.source}
}

// Set the parent node of the conditional operation node.
func (e *conditionalOperation) setParent(parent node) {
	e.parent = parent
}

// Node type of the block.
func (b *block) Type() NodeType {
	return BlockNode
}

// Title of the block.
func (b *block) Title() string {
	return b.BlockString()
}

// Parent node of the block node.
func (b *block) Parent() Node {
	return b.parent
}

// Children nodes of the block node.
func (b *block) Children() []Node {
	children := make([]Node, 0, len(b.procedures)+2)

	for _, entry := range *b.scope.SymbolTable {
		children = append(children, symbol{entry, b})
	}

	for _, procedure := range b.procedures {
		children = append(children, procedure)
	}

	return append(children, b.statement, b.source)
}

// Set the parent node of the block node.
func (b *block) setParent(parent node) {
	b.parent = parent
}

// Node type of the assignment statement.
func (s *assignmentStatement) Type() NodeType {
	return AssignmentStatementNode
}

// Title of the assignment statement.
func (s *assignmentStatement) Title() string {
	return s.StatementString()
}

// Parent node of the assignment statement node.
func (s *assignmentStatement) Parent() Node {
	return s.parent
}

// Children nodes of the assignment statement node.
func (s *assignmentStatement) Children() []Node {
	return []Node{s.symbol, s.expression, s.source}
}

// Set the parent node of the assignment statement node.
func (s *assignmentStatement) setParent(parent node) {
	s.parent = parent
}

// Node type of the read statement.
func (s *readStatement) Type() NodeType {
	return ReadStatementNode
}

// Title of the read statement.
func (s *readStatement) Title() string {
	return s.StatementString()
}

// Parent node of the read statement node.
func (s *readStatement) Parent() Node {
	return s.parent
}

// Children nodes of the read statement node.
func (s *readStatement) Children() []Node {
	return []Node{s.symbol, s.source}
}

// Set the parent node of the read statement node.
func (s *readStatement) setParent(parent node) {
	s.parent = parent
}

// Node type of the write statement.
func (s *writeStatement) Type() NodeType {
	return WriteStatementNode
}

// Title of the write statement.
func (s *writeStatement) Title() string {
	return s.StatementString()
}

// Parent node of the write statement node.
func (s *writeStatement) Parent() Node {
	return s.parent
}

// Children nodes of the write statement node.
func (s *writeStatement) Children() []Node {
	return []Node{s.expression, s.source}
}

// Set the parent node of the write statement node.
func (s *writeStatement) setParent(parent node) {
	s.parent = parent
}

// Node type of the call statement.
func (s *callStatement) Type() NodeType {
	return CallStatementNode
}

// Title of the call statement.
func (s *callStatement) Title() string {
	return s.StatementString()
}

// Parent node of the call statement node.
func (s *callStatement) Parent() Node {
	return s.parent
}

// Children nodes of the call statement node.
func (s *callStatement) Children() []Node {
	return []Node{s.symbol, s.source}
}

// Set the parent node of the call statement node.
func (s *callStatement) setParent(parent node) {
	s.parent = parent
}

// Node type of the if-then statement.
func (s *ifStatement) Type() NodeType {
	return IfStatementNode
}

// Title of the if-then statement.
func (s *ifStatement) Title() string {
	return s.StatementString()
}

// Parent node of the if-then statement node.
func (s *ifStatement) Parent() Node {
	return s.parent
}

// Children nodes of the if-then statement node.
func (s *ifStatement) Children() []Node {
	return []Node{s.condition, s.statement, s.source}
}

// Set the parent node of the if-then statement node.
func (s *ifStatement) setParent(parent node) {
	s.parent = parent
}

// Node type of the while-do statement.
func (s *whileStatement) Type() NodeType {
	return WhileStatementNode
}

// Title of the while-do statement.
func (s *whileStatement) Title() string {
	return s.StatementString()
}

// Parent node of the while-do statement node.
func (s *whileStatement) Parent() Node {
	return s.parent
}

// Children nodes of the while-do statement node.
func (s *whileStatement) Children() []Node {
	return []Node{s.condition, s.statement, s.source}
}

// Set the parent node of the while-do statement node.
func (s *whileStatement) setParent(parent node) {
	s.parent = parent
}

// Node type of the compound statement.
func (s *compoundStatement) Type() NodeType {
	return CompoundStatementNode
}

// Title of the compound statement.
func (s *compoundStatement) Title() string {
	return s.StatementString()
}

// Parent node of the compound statement node.
func (c *compoundStatement) Parent() Node {
	return c.parent
}

// Children nodes of the compound statement node.
func (c *compoundStatement) Children() []Node {
	children := make([]Node, 0, len(c.statements)+1)

	for _, statement := range c.statements {
		children = append(children, statement)
	}

	return append(children, c.source)
}

// Set the parent node of the compound statement node.
func (c *compoundStatement) setParent(parent node) {
	c.parent = parent
}
