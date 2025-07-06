// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"encoding/json"
	"fmt"
	"io"
	"strings"

	cor "github.com/petersen65/PL0/v2/core"
)

var (
	// dataTypeNames maps a data type to its string representation.
	dataTypeNames = map[DataType]string{
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

	// nodeTypeNames maps node types to their string representation.
	nodeTypeNames = map[NodeType]string{
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

	// kindNames maps symbol kinds to their string representation.
	kindNames = map[Entry]string{
		Constant:  "constant",
		Variable:  "variable",
		Procedure: "procedure",
	}

	// usageNames maps usage modes to their string representation.
	usageNames = map[Usage]string{
		Read:    "read",
		Write:   "write",
		Execute: "execute",
	}
)

// NewScope creates a new scope with an empty symbol table and requires a number that is unique accross all compilation phases.
func newScope(uniqueId int32, outer *Scope) *Scope {
	symbolTable := make(SymbolTable)

	return &Scope{
		Outer:             outer,
		Extension:         make(map[ExtensionType]any),
		id:                uniqueId,
		identifierCounter: make(map[rune]uint64),
		names:             make([]string, 0),
		symbolTable:       &symbolTable,
	}
}

// Create a new entry for the symbol table.
func newSymbol(name string, kind Entry, declaration Declaration) *Symbol {
	return &Symbol{
		Name:        name,
		Kind:        kind,
		Declaration: declaration,
		Extension:   make(map[ExtensionType]any),
	}
}

// Create a new block node in the abstract syntax tree.
func newBlock(depth int32, scope *Scope, declarations []Declaration, statement Statement) Block {
	block := &BlockNode{
		TypeName:     nodeTypeNames[BlockType],
		Depth:        depth,
		Scope:        scope,
		Declarations: declarations,
		Closure:      make([]Declaration, 0),
		Statement:    statement,
	}

	for _, declaration := range block.Declarations {
		declaration.SetParent(block)
	}

	statement.SetParent(block)
	return block
}

// Create a new constant declaration node in the abstract syntax tree.
func newConstantDeclaration(name string, value any, dataType DataType, scope *Scope, index int) Declaration {
	return &ConstantDeclarationNode{
		TypeName:         nodeTypeNames[ConstantDeclarationType],
		Name:             name,
		Value:            value,
		DataType:         dataType,
		Scope:            scope,
		Usage:            make([]Expression, 0),
		TokenStreamIndex: index,
	}
}

// Create a new variable declaration node in the abstract syntax tree.
func newVariableDeclaration(name string, dataType DataType, scope *Scope, index int) Declaration {
	return &VariableDeclarationNode{
		TypeName:         nodeTypeNames[VariableDeclarationType],
		Name:             name,
		DataType:         dataType,
		Scope:            scope,
		Usage:            make([]Expression, 0),
		TokenStreamIndex: index,
	}
}

// Create a new procedure declaration node in the abstract syntax tree.
func newProcedureDeclaration(name string, block Block, scope *Scope, index int) Declaration {
	return &ProcedureDeclarationNode{
		TypeName:         nodeTypeNames[ProcedureDeclarationType],
		Name:             name,
		Block:            block,
		Scope:            scope,
		Usage:            make([]Expression, 0),
		TokenStreamIndex: index,
	}
}

// Create a new literal node in the abstract syntax tree.
func newLiteral(value any, dataType DataType, scope *Scope, index int) Expression {
	return &LiteralNode{
		TypeName:         nodeTypeNames[LiteralType],
		Value:            value,
		DataType:         dataType,
		Scope:            scope,
		TokenStreamIndex: index,
	}
}

// Create a new identifier-use node in the abstract syntax tree.
func newIdentifierUse(name string, scope *Scope, context Entry, index int) Expression {
	return &IdentifierUseNode{
		TypeName:         nodeTypeNames[IdentifierUseType],
		Name:             name,
		Scope:            scope,
		Context:          context,
		TokenStreamIndex: index,
	}
}

// Create a new unary operation node in the abstract syntax tree.
func newUnaryOperation(operation UnaryOperator, operand Expression, index int) Expression {
	unary := &UnaryOperationNode{
		TypeName:         nodeTypeNames[UnaryOperationType],
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
		TypeName:         nodeTypeNames[BinaryOperationType],
		Operation:        operation,
		Left:             left,
		Right:            right,
		TokenStreamIndex: index,
	}

	left.SetParent(binary)
	right.SetParent(binary)
	return binary
}

// Create a new conditional operation node in the abstract syntax tree.
func newConditionalOperation(operation RelationalOperator, left, right Expression, index int) Expression {
	conditional := &ConditionalOperationNode{
		TypeName:         nodeTypeNames[ConditionalOperationType],
		Operation:        operation,
		Left:             left,
		Right:            right,
		TokenStreamIndex: index,
	}

	left.SetParent(conditional)
	right.SetParent(conditional)
	return conditional
}

// Create a new assignment statement node in the abstract syntax tree.
func newAssignmentStatement(variable, expression Expression, index int) Statement {
	assignment := &AssignmentStatementNode{
		TypeName:         nodeTypeNames[AssignmentStatementType],
		Variable:         variable,
		Expression:       expression,
		TokenStreamIndex: index,
	}

	variable.SetParent(assignment)
	expression.SetParent(assignment)
	return assignment
}

// Create a new read statement node in the abstract syntax tree.
func newReadStatement(variable Expression, index int) Statement {
	read := &ReadStatementNode{
		TypeName:         nodeTypeNames[ReadStatementType],
		Variable:         variable,
		TokenStreamIndex: index,
	}

	variable.SetParent(read)
	return read
}

// Create a new write statement node in the abstract syntax tree.
func newWriteStatement(expression Expression, index int) Statement {
	write := &WriteStatementNode{
		TypeName:         nodeTypeNames[WriteStatementType],
		Expression:       expression,
		TokenStreamIndex: index,
	}

	expression.SetParent(write)
	return write
}

// Create a new call statement node in the abstract syntax tree.
func newCallStatement(procedure Expression, index int) Statement {
	call := &CallStatementNode{
		TypeName:         nodeTypeNames[CallStatementType],
		Procedure:        procedure,
		TokenStreamIndex: index,
	}

	procedure.SetParent(call)
	return call
}

// Create a new if-then statement node in the abstract syntax tree.
func newIfStatement(condition Expression, statement Statement, index int) Statement {
	ifStmt := &IfStatementNode{
		TypeName:         nodeTypeNames[IfStatementType],
		Condition:        condition,
		Statement:        statement,
		TokenStreamIndex: index,
	}

	condition.SetParent(ifStmt)
	statement.SetParent(ifStmt)
	return ifStmt
}

// Create a new while-do statement node in the abstract syntax tree.
func newWhileStatement(condition Expression, statement Statement, index int) Statement {
	whileStmt := &WhileStatementNode{
		TypeName:         nodeTypeNames[WhileStatementType],
		Condition:        condition,
		Statement:        statement,
		TokenStreamIndex: index,
	}

	condition.SetParent(whileStmt)
	statement.SetParent(whileStmt)
	return whileStmt
}

// Create a new compound statement node in the abstract syntax tree.
func newCompoundStatement(statements []Statement) Statement {
	compound := &CompoundStatementNode{
		TypeName:   nodeTypeNames[CompoundStatementType],
		Statements: statements,
	}

	for _, statement := range compound.Statements {
		statement.SetParent(compound)
	}

	return compound
}

// String representation of a data type.
func (dt DataType) String() string {
	return dataTypeNames[dt]
}

// Get a data type from its representation.
func (dtr DataTypeRepresentation) DataType() DataType {
	for dataType, representation := range dataTypeNames {
		if representation == string(dtr) {
			return dataType
		}
	}

	panic(cor.NewGeneralError(cor.AbstractSyntaxTree, failureMap, cor.Fatal, unknownDataTypeRepresentation, dtr, nil))
}

// String representation of a symbol entry kind.
func (e Entry) String() string {
	return kindNames[e]
}

// String representation of a usage mode bit-mask.
func (u Usage) String() string {
	var parts []string

	for usage, name := range usageNames {
		if u&usage != 0 {
			parts = append(parts, name)
		}
	}

	return strings.Join(parts, "|")
}

// Return the unique identifier of the scope
func (s *Scope) Id() int32 {
	return s.id
}

// Create a new compiler-generated unique identifier name for a scope.
func (s *Scope) NewIdentifier(prefix rune) string {
	if _, ok := s.identifierCounter[prefix]; !ok {
		s.identifierCounter[prefix] = 0
	}

	s.identifierCounter[prefix]++
	return fmt.Sprintf("%c%v.%v", prefix, s.id, s.identifierCounter[prefix])
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

// Get type of the block node.
func (b *BlockNode) Type() NodeType {
	return BlockType
}

// Set the parent Node of the block node.
func (b *BlockNode) SetParent(parent Node) {
	b.ParentNode = parent
}

// String of the block node.
func (b *BlockNode) String() string {
	return fmt.Sprintf("block depth=%v", b.Depth)
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

// Print the abstract syntax tree to the specified writer.
func (b *BlockNode) Print(print io.Writer, args ...any) error {
	// traverse the abstract syntax tree and print each node
	if err := printAbstractSyntaxTree(b, "", true, print); err != nil {
		return cor.NewGeneralError(cor.AbstractSyntaxTree, failureMap, cor.Error, abstractSyntaxExportFailed, nil, err)
	}

	return nil
}

// Export the abstract syntax tree of the block node.
func (b *BlockNode) Export(format cor.ExportFormat, print io.Writer) error {
	switch format {
	case cor.Json:
		// export the abstract syntax tree as a JSON object
		if raw, err := json.MarshalIndent(b, "", "  "); err != nil {
			return cor.NewGeneralError(cor.AbstractSyntaxTree, failureMap, cor.Error, abstractSyntaxExportFailed, nil, err)
		} else {
			_, err = print.Write(raw)

			if err != nil {
				err = cor.NewGeneralError(cor.AbstractSyntaxTree, failureMap, cor.Error, abstractSyntaxExportFailed, nil, err)
			}

			return err
		}

	case cor.Text:
		// print is a convenience function to export the abstract syntax tree as a string to the print writer
		return b.Print(print)

	default:
		panic(cor.NewGeneralError(cor.AbstractSyntaxTree, failureMap, cor.Fatal, unknownExportFormat, format, nil))
	}
}

// Type of the constant declaration node.
func (d *ConstantDeclarationNode) Type() NodeType {
	return ConstantDeclarationType
}

// Set the parent Node of the constant declaration node.
func (d *ConstantDeclarationNode) SetParent(parent Node) {
	d.ParentNode = parent
}

// String of the constant declaration node.
func (d *ConstantDeclarationNode) String() string {
	return fmt.Sprintf("declaration(%v,name=%v,value=%v,type=%v,used=%v)", kindNames[Constant], d.Name, d.Value, d.DataType, len(d.Usage))
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

// Type of the variable declaration node.
func (d *VariableDeclarationNode) Type() NodeType {
	return VariableDeclarationType
}

// Set the parent Node of the variable declaration node.
func (d *VariableDeclarationNode) SetParent(parent Node) {
	d.ParentNode = parent
}

// String of the variable declaration node.
func (d *VariableDeclarationNode) String() string {
	return fmt.Sprintf("declaration(%v,name=%v,type=%v,used=%v)", kindNames[Variable], d.Name, d.DataType, len(d.Usage))
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

// Type of the procedure declaration node.
func (d *ProcedureDeclarationNode) Type() NodeType {
	return ProcedureDeclarationType
}

// Set the parent Node of the procedure declaration node.
func (d *ProcedureDeclarationNode) SetParent(parent Node) {
	d.ParentNode = parent
}

// String of the procedure declaration node.
func (d *ProcedureDeclarationNode) String() string {
	return fmt.Sprintf("declaration(%v,name=%v,used=%v)", kindNames[Procedure], d.Name, len(d.Usage))
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

// Type of the literal node.
func (e *LiteralNode) Type() NodeType {
	return LiteralType
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

// Accept the visitor for the literal node.
func (e *LiteralNode) Accept(visitor Visitor) {
	visitor.VisitLiteral(e)
}

// Type of the identifier-use node.
func (u *IdentifierUseNode) Type() NodeType {
	return IdentifierUseType
}

// Set the parent Node of the identifier-use node.
func (u *IdentifierUseNode) SetParent(parent Node) {
	u.ParentNode = parent
}

// String of the identifier-use node.
func (u *IdentifierUseNode) String() string {
	if symbol := u.Scope.Lookup(u.Name); symbol != nil {
		switch symbol.Kind {
		case Constant:
			return fmt.Sprintf("use(kind=%v,name=%v,value=%v,usage=%v)", symbol.Kind, symbol.Name, symbol.Declaration.(*ConstantDeclarationNode).Value, u.Use)

		case Variable:
			return fmt.Sprintf("use(kind=%v,name=%v,usage=%v)", symbol.Kind, symbol.Name, u.Use)

		case Procedure:
			return fmt.Sprintf("use(kind=%v,name=%v,usage=%v)", symbol.Kind, symbol.Name, u.Use)

		default:
			panic(cor.NewGeneralError(cor.AbstractSyntaxTree, failureMap, cor.Fatal, unknownSymbolKind, nil, nil))
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

// ExpressionString returns the string representation of an identifier-use.
func (u *IdentifierUseNode) ExpressionString() string {
	return u.String()
}

// Accept the visitor for the identifier-use node.
func (u *IdentifierUseNode) Accept(visitor Visitor) {
	visitor.VisitIdentifierUse(u)
}

// Type of the unary operation node.
func (e *UnaryOperationNode) Type() NodeType {
	return UnaryOperationType
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
		panic(cor.NewGeneralError(cor.AbstractSyntaxTree, failureMap, cor.Fatal, unknownUnaryOperation, nil, nil))
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

// Type of the binary operation node.
func (e *BinaryOperationNode) Type() NodeType {
	return BinaryOperationType
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
		panic(cor.NewGeneralError(cor.AbstractSyntaxTree, failureMap, cor.Fatal, unknownBinaryOperation, nil, nil))
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

// Type of the conditional operation node.
func (e *ConditionalOperationNode) Type() NodeType {
	return ConditionalOperationType
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
		panic(cor.NewGeneralError(cor.AbstractSyntaxTree, failureMap, cor.Fatal, unknownConditionalOperation, nil, nil))

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

// Type of the assignment statement node.
func (s *AssignmentStatementNode) Type() NodeType {
	return AssignmentStatementType
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

// Type of the read statement node.
func (s *ReadStatementNode) Type() NodeType {
	return ReadStatementType
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

// Type of the write statement node.
func (s *WriteStatementNode) Type() NodeType {
	return WriteStatementType
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

// Type of the call statement node.
func (s *CallStatementNode) Type() NodeType {
	return CallStatementType
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

// Type of the if-then statement node.
func (s *IfStatementNode) Type() NodeType {
	return IfStatementType
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

// Type of the while-do statement node.
func (s *WhileStatementNode) Type() NodeType {
	return WhileStatementType
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

// Type of the compound statement node.
func (s *CompoundStatementNode) Type() NodeType {
	return CompoundStatementType
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
		return cor.NewGeneralError(cor.AbstractSyntaxTree, failureMap, cor.Error, cannotWalkOnNilNode, nil, nil)
	} else if visitor == nil && visit == nil {
		return cor.NewGeneralError(cor.AbstractSyntaxTree, failureMap, cor.Error, walkRequiresVisitorOrFunction, nil, nil)
	} else if _, ok := visitor.(Visitor); !ok && visit == nil {
		return cor.NewGeneralError(cor.AbstractSyntaxTree, failureMap, cor.Error, walkRequiresInterfaceOrFunction, nil, nil)
	}

	// filter out empty constants
	if constant, ok := parent.(*ConstantDeclarationNode); ok && constant.Name == EmptyConstantName {
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
			return cor.NewGeneralError(cor.AbstractSyntaxTree, failureMap, cor.Error, inOrderRequiresTwoChildren, nil, nil)
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
