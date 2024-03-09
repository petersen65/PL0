// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package ast

type (
	// Block node represents a block in the AST.
	block struct {
		parent       Node              // parent node of the block
		symbol       Symbol            // procedure symbol entry of the block
		depth        int32             // declaration nesting depth
		declarations []Symbol          // local declarations of the block
		procedures   []Block           // nested procedures of the block
		statement    Statement         // statement of the block
		source       SourceDescription // source description for the block node
	}

	// Literal node represents the usage of a literal value in the AST.
	literal struct {
		parent   Node              // parent node of the literal
		value    any               // literal value
		dataType DataType          // data type of the literal
		source   SourceDescription // source description for the literal node
	}

	// Constant node represents the usage of a constant in the AST.
	constant struct {
		parent Node              // parent node of the constant
		symbol Symbol            // constant symbol entry
		source SourceDescription // source description for the constant node
	}

	// Variable node represents the usage of a variable in the AST.
	variable struct {
		parent Node              // parent node of the variable
		symbol Symbol            // variable symbol entry
		source SourceDescription // source description for the variable node
	}

	// UnaryOperation node represents a unary operation in the AST.
	unaryOperation struct {
		parent    Node              // parent node of the unary operation
		operation UnaryOperator     // unary operation
		operand   Expression        // operand of the unary operation
		source    SourceDescription // source description for the unary operation node
	}

	// BinaryOperation node represents a binary operation in the AST.
	binaryOperation struct {
		parent    Node              // parent node of the binary operation
		operation BinaryOperator    // binary operation
		left      Expression        // left operand of the binary operation
		right     Expression        // right operand of the binary operation
		source    SourceDescription // source description for the binary operation node
	}

	// ConditionalOperation node represents a conditional operation in the AST.
	conditionalOperation struct {
		parent    Node               // parent node of the conditional
		operation RelationalOperator // conditional operation
		left      Expression         // left operand of the conditional operation
		right     Expression         // right operand of the conditional operation
		source    SourceDescription  // source description for the conditional operation node
	}

	// AssignmentStatement node represents an assignment statement in the AST.
	assignmentStatement struct {
		parent     Node              // parent node of the assignment statement
		symbol     Symbol            // variable symbol entry on the left side of the assignment statement
		expression Expression        // expression on the right side of the assignment statement
		source     SourceDescription // source description for the assignment statement node
	}

	// ReadStatement node represents a read statement in the AST.
	readStatement struct {
		parent Node              // parent node of the read statement
		symbol Symbol            // variable symbol entry of the read statement
		source SourceDescription // source description for the read statement node
	}

	// WriteStatement node represents a write statement in the AST.
	writeStatement struct {
		parent     Node              // parent node of the write statement
		expression Expression        // expression of the write statement
		source     SourceDescription // source description for the write statement node
	}

	// CallStatement node represents a call statement in the AST.
	callStatement struct {
		parent Node              // parent node of the call statement
		symbol Symbol            // procedure symbol entry of the call statement
		source SourceDescription // source description for the call statement node
	}

	// IfStatement node represents an if-then statement in the AST.
	ifStatement struct {
		parent    Node              // parent node of the if-then statement
		condition Expression        // if-condition of the if-then statement
		statement Statement         // then-statement of the if-then statement
		source    SourceDescription // source description for the if-then statement node
	}

	// WhileStatement node represents a while-do statement in the AST.
	whileStatement struct {
		parent    Node              // parent node of the while-do statement
		condition Expression        // while-condition of the while-do statement
		statement Statement         // do-statement of the while-do statement
		source    SourceDescription // source description for the while-do statement node
	}

	// CompoundStatement node represents a begin-end statement in the AST.
	compoundStatement struct {
		parent     Node              // parent node of the begin-end compound statement
		statements []Statement       // all statements of the begin-end compound statement
		source     SourceDescription // source description for the begin-end statement node
	}
)

// Create a new block node in the abstract syntax tree.
func newBlock(symbol Symbol, depth int32, declarations []Symbol, procedures []Block, statement Statement, source SourceDescription) Block {
	parent := new(block)

	for i := range declarations {
		declarations[i].ParentNode = parent
	}

	for _, procedure := range procedures {
		procedure.(*block).parent = parent
	}

	setParent(statement, parent)
	symbol.ParentNode = parent
	source.ParentNode = parent

	parent.symbol = symbol
	parent.depth = depth
	parent.declarations = declarations
	parent.procedures = procedures
	parent.statement = statement
	parent.source = source

	return parent
}

// Create a new literal-usage node in the abstract syntax tree.
func newLiteral(value any, dataType DataType, source SourceDescription) Expression {
	parent := new(literal)
	source.ParentNode = parent

	parent.value = value
	parent.dataType = dataType
	parent.source = source

	return parent
}

// Create a new constant-usage node in the abstract syntax tree.
func newConstant(symbol Symbol, source SourceDescription) Expression {
	parent := new(constant)

	symbol.ParentNode = parent
	source.ParentNode = parent

	parent.symbol = symbol
	parent.source = source

	return parent
}

// Create a new variable-usage node in the abstract syntax tree.
func newVariable(symbol Symbol, source SourceDescription) Expression {
	parent := new(variable)

	symbol.ParentNode = parent
	source.ParentNode = parent

	parent.symbol = symbol
	parent.source = source

	return parent
}

// Create a new unary operation node in the abstract syntax tree.
func newUnaryOperation(operation UnaryOperator, operand Expression, source SourceDescription) Expression {
	parent := new(unaryOperation)

	setParent(operand, parent)
	source.ParentNode = parent

	parent.operation = operation
	parent.operand = operand
	parent.source = source

	return parent
}

// Create a new binary operation node in the abstract syntax tree.
func newBinaryOperation(operation BinaryOperator, left, right Expression, source SourceDescription) Expression {
	parent := new(binaryOperation)

	setParent(left, parent)
	setParent(right, parent)
	source.ParentNode = parent

	parent.operation = operation
	parent.left = left
	parent.right = right
	parent.source = source

	return parent
}

// Create a new conditional operation node in the abstract syntax tree.
func newConditionalOperation(operation RelationalOperator, left, right Expression, source SourceDescription) Expression {
	parent := new(conditionalOperation)

	setParent(left, parent)
	setParent(right, parent)
	source.ParentNode = parent

	parent.operation = operation
	parent.left = left
	parent.right = right
	parent.source = source

	return parent
}

// Create a new assignment statement node in the abstract syntax tree.
func newAssignmentStatement(symbol Symbol, expression Expression, source SourceDescription) Statement {
	parent := new(assignmentStatement)

	setParent(expression, parent)
	symbol.ParentNode = parent
	source.ParentNode = parent

	parent.symbol = symbol
	parent.expression = expression
	parent.source = source

	return parent
}

// Create a new read statement node in the abstract syntax tree.
func newReadStatement(symbol Symbol, source SourceDescription) Statement {
	parent := new(readStatement)

	symbol.ParentNode = parent
	source.ParentNode = parent

	parent.symbol = symbol
	parent.source = source

	return parent
}

// Create a new write statement node in the abstract syntax tree.
func newWriteStatement(expression Expression, source SourceDescription) Statement {
	parent := new(writeStatement)

	setParent(expression, parent)
	source.ParentNode = parent

	parent.expression = expression
	parent.source = source

	return parent
}

// Create a new call statement node in the abstract syntax tree.
func newCallStatement(symbol Symbol, source SourceDescription) Statement {
	parent := new(callStatement)

	symbol.ParentNode = parent
	source.ParentNode = parent

	parent.symbol = symbol
	parent.source = source

	return parent
}

// Create a new if-then statement node in the abstract syntax tree.
func newIfStatement(condition Expression, statement Statement, source SourceDescription) Statement {
	parent := new(ifStatement)

	setParent(condition, parent)
	setParent(statement, parent)
	source.ParentNode = parent

	parent.condition = condition
	parent.statement = statement
	parent.source = source

	return parent
}

// Create a new while-do statement node in the abstract syntax tree.
func newWhileStatement(condition Expression, statement Statement, source SourceDescription) Statement {
	parent := new(whileStatement)

	setParent(condition, parent)
	setParent(statement, parent)
	source.ParentNode = parent

	parent.condition = condition
	parent.statement = statement
	parent.source = source

	return parent
}

// Create a new compound statement node in the abstract syntax tree.
func newCompoundStatement(statements []Statement, source SourceDescription) Statement {
	parent := new(compoundStatement)

	for _, statement := range statements {
		setParent(statement, parent)
	}

	source.ParentNode = parent

	parent.statements = statements
	parent.source = source

	return parent
}

func (s Symbol) Title() string {
	return "Symbol"
}

func (s Symbol) Parent() Node {
	return s.ParentNode
}

func (s Symbol) Children() []Node {
	return make([]Node, 0)
}

func (s SourceDescription) Title() string {
	return "SourceDescription"
}

func (s SourceDescription) Parent() Node {
	return s.ParentNode
}

func (s SourceDescription) Children() []Node {
	return make([]Node, 0)
}

func (e *literal) Title() string {
	return "Literal"
}

func (e *literal) Parent() Node {
	return e.parent
}

func (e *literal) Children() []Node {
	return []Node{e.source}
}

func (e *constant) Title() string {
	return "Constant"
}

func (e *constant) Parent() Node {
	return e.parent
}

func (e *constant) Children() []Node {
	return []Node{e.symbol, e.source}
}

func (e *variable) Title() string {
	return "Variable"
}

func (e *variable) Parent() Node {
	return e.parent
}

func (e *variable) Children() []Node {
	return []Node{e.symbol, e.source}
}

func (e *unaryOperation) Title() string {
	return "UnaryOperation"
}

func (e *unaryOperation) Parent() Node {
	return e.parent
}

func (e *unaryOperation) Children() []Node {
	return []Node{e.operand, e.source}
}

func (e *binaryOperation) Title() string {
	return "BinaryOperation"
}

func (e *binaryOperation) Parent() Node {
	return e.parent
}

func (e *binaryOperation) Children() []Node {
	return []Node{e.left, e.right, e.source}
}

func (e *conditionalOperation) Title() string {
	return "ConditionalOperation"
}

func (e *conditionalOperation) Parent() Node {
	return e.parent
}

func (e *conditionalOperation) Children() []Node {
	return []Node{e.left, e.right, e.source}
}

func (b *block) Title() string {
	return "Block"
}

func (b *block) Parent() Node {
	return b.parent
}

func (b *block) Children() []Node {
	children := make([]Node, 0, len(b.declarations)+len(b.procedures)+2)

	for _, declaration := range b.declarations {
		children = append(children, declaration)
	}

	for _, procedure := range b.procedures {
		children = append(children, procedure)
	}

	return append(children, b.statement, b.source)
}

func (s *assignmentStatement) Title() string {
	return "AssignmentStatement"
}

func (s *assignmentStatement) Parent() Node {
	return s.parent
}

func (s *assignmentStatement) Children() []Node {
	return []Node{s.symbol, s.expression, s.source}
}

func (s *readStatement) Title() string {
	return "ReadStatement"
}

func (s *readStatement) Parent() Node {
	return s.parent
}

func (s *readStatement) Children() []Node {
	return []Node{s.symbol, s.source}
}

func (s *writeStatement) Title() string {
	return "WriteStatement"
}

func (s *writeStatement) Parent() Node {
	return s.parent
}

func (s *writeStatement) Children() []Node {
	return []Node{s.expression, s.source}
}

func (s *callStatement) Title() string {
	return "CallStatement"
}

func (s *callStatement) Parent() Node {
	return s.parent
}

func (s *callStatement) Children() []Node {
	return []Node{s.symbol, s.source}
}

func (s *ifStatement) Title() string {
	return "IfStatement"
}

func (s *ifStatement) Parent() Node {
	return s.parent
}

func (s *ifStatement) Children() []Node {
	return []Node{s.condition, s.statement, s.source}
}

func (s *whileStatement) Title() string {
	return "WhileStatement"
}

func (s *whileStatement) Parent() Node {
	return s.parent
}

func (s *whileStatement) Children() []Node {
	return []Node{s.condition, s.statement, s.source}
}

func (c *compoundStatement) Title() string {
	return "CompoundStatement"
}

func (c *compoundStatement) Parent() Node {
	return c.parent
}

func (c *compoundStatement) Children() []Node {
	children := make([]Node, 0, len(c.statements)+1)

	for _, statement := range c.statements {
		children = append(children, statement)
	}

	return append(children, c.source)
}

func setParent(node Node, parent Node) {
	switch n := node.(type) {
	case *block:
		n.parent = parent

	case *literal:
		n.parent = parent

	case *constant:
		n.parent = parent

	case *variable:
		n.parent = parent

	case *unaryOperation:
		n.parent = parent

	case *binaryOperation:
		n.parent = parent

	case *conditionalOperation:
		n.parent = parent

	case *assignmentStatement:
		n.parent = parent

	case *readStatement:
		n.parent = parent

	case *writeStatement:
		n.parent = parent

	case *callStatement:
		n.parent = parent

	case *ifStatement:
		n.parent = parent

	case *whileStatement:
		n.parent = parent

	case *compoundStatement:
		n.parent = parent
	}
}
