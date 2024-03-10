// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package ast

import (
	"fmt"
	"strings"
)

// String returns the string representation of the symbol node.
func (s *symbol) String() string {
	if s.isDeclaration() {
		return fmt.Sprintf("sym_decl(%v:%v)", s.Name, s.Depth)
	}

	return fmt.Sprintf("sym_ref(%v:%v)", s.Name, s.Depth)
}

// String returns the string representation of the source description node.
func (s *sourceDescription) String() string {
	if len(s.Lines) == 0 {
		return fmt.Sprintf("source %v-%v", s.From, s.To)
	}

	if len(s.Lines) == 1 {
		return fmt.Sprintf("source %v", strings.Trim(string(s.Lines[0].Code), " "))
	}

	return fmt.Sprintf(
		"source %v ... %v",
		strings.Trim(string(s.Lines[0].Code), " "),
		strings.Trim(string(s.Lines[len(s.Lines)-1].Code), " "))
}

// BlockString returns the string representation of the block node.
func (b *block) BlockString() string {
	return fmt.Sprintf("block %v:%v", b.symbol.Name, b.depth)
}

// ExpressionString returns the string representation of the literal node.
func (e *literal) ExpressionString() string {
	switch e.dataType {
	case Integer64:
		return fmt.Sprintf("lit(%v:i64)", e.value)

	default:
		return fmt.Sprintf("lit(%v)", e.value)
	}
}

// ExpressionString returns the string representation of the constant node.
func (e *constant) ExpressionString() string {
	return fmt.Sprintf("const(%v=%v)", e.symbol.Name, e.symbol.Value)
}

// ExpressionString returns the string representation of the variable node.
func (e *variable) ExpressionString() string {
	return fmt.Sprintf("var(%v:%v)", e.symbol.Name, e.symbol.Depth)
}

// ExpressionString returns the string representation of the unary operation node.
func (e *unaryOperation) ExpressionString() string {
	var builder strings.Builder

	switch e.operation {
	case Odd:
		builder.WriteString("odd(")

	case Negate:
		builder.WriteString("neg(")
	}

	writeString(&builder, e.operand.ExpressionString(), ")")
	return builder.String()
}

// ExpressionString returns the string representation of the binary operation node.
func (e *binaryOperation) ExpressionString() string {
	var builder strings.Builder

	switch e.operation {
	case Plus:
		builder.WriteString("add(")

	case Minus:
		builder.WriteString("sub(")

	case Times:
		builder.WriteString("mul(")

	case Divide:
		builder.WriteString("div(")
	}

	writeString(&builder, e.left.ExpressionString(), ",", e.right.ExpressionString(), ")")
	return builder.String()
}

// ConditionString returns the string representation of the conditional operation node.
func (e *conditionalOperation) ExpressionString() string {
	var builder strings.Builder

	switch e.operation {
	case Equal:
		builder.WriteString("eq(")

	case NotEqual:
		builder.WriteString("neq(")

	case Less:
		builder.WriteString("ls(")

	case LessEqual:
		builder.WriteString("le(")

	case Greater:
		builder.WriteString("gr(")

	case GreaterEqual:
		builder.WriteString("ge(")
	}

	writeString(&builder, e.left.ExpressionString(), ",", e.right.ExpressionString(), ")")
	return builder.String()
}

// StatementString returns the string representation of the assignment statement node.
func (s *assignmentStatement) StatementString() string {
	return "assign"
}

// StatementString returns the string representation of the read statement node.
func (s *readStatement) StatementString() string {
	return "read"
}

// StatementString returns the string representation of the write statement node.
func (s *writeStatement) StatementString() string {
	return "write"
}

// StatementString returns the string representation of the call statement node.
func (s *callStatement) StatementString() string {
	return "call"
}

// StatementString returns the string representation of the if-then statement node.
func (s *ifStatement) StatementString() string {
	return "if"
}

// StatementString returns the string representation of the while-do statement node.
func (s *whileStatement) StatementString() string {
	return "while"
}

// StatementString returns the string representation of the compound statement node.
func (s *compoundStatement) StatementString() string {
	var builder strings.Builder

	builder.WriteString("compound")

	for _, statement := range s.statements {
		writeString(&builder, " ", statement.StatementString())
	}

	return builder.String()
}

// Write the specified string items to the builder.
func writeString(builder *strings.Builder, items ...string) {
	for _, item := range items {
		builder.WriteString(item)
	}
}
