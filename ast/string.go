// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package ast

import (
	"fmt"
	"strings"
)

const (
	// Indentation size for the block node.
	indentSize = 3

	// String used for indentation.
	indentString = "|  "
)

// BlockString returns the string representation of the block node.
func (b *block) BlockString() string {
	var builder strings.Builder

	writeString(&builder, indent(int(b.depth)), "+- block(", b.symbol.Name, ":", fmt.Sprintf("%v", b.depth), ",[")

	for i, declaration := range b.declarations {
		writeString(&builder, declaration.Name, ":", fmt.Sprintf("%v", declaration.Depth))

		if i < len(b.declarations)-1 {
			builder.WriteString(",")
		}
	}

	builder.WriteString("]\n")

	for i, procedure := range b.procedures {
		builder.WriteString(procedure.String())

		if i < len(b.procedures)-1 {
			builder.WriteString("\n")
		}
	}

	builder.WriteString(b.statement.StatementString(int(b.depth + 1)))
	return builder.String()
}

// String returns the string representation of the block node.
func (b *block) String() string {
	return b.BlockString()
}

// ExpressionString returns the string representation of the literal node.
func (e *literal) ExpressionString() string {
	switch e.dataType {
	case Integer64:
		return fmt.Sprintf("literal(%v:i64)", e.value.(int64))

	default:
		return fmt.Sprintf("literal(%v)", e.value)
	}
}

// String returns the string representation of the literal node.
func (e *literal) String() string {
	return e.ExpressionString()
}

// ExpressionString returns the string representation of the constant node.
func (e *constant) ExpressionString() string {
	return fmt.Sprintf("constant(%v=%v)", e.symbol.Name, e.symbol.Value)
}

// String returns the string representation of the constant node.
func (e *constant) String() string {
	return e.ExpressionString()
}

// ExpressionString returns the string representation of the variable node.
func (e *variable) ExpressionString() string {
	return fmt.Sprintf("variable(%v:%v)", e.symbol.Name, e.symbol.Depth)
}

// String returns the string representation of the variable node.
func (e *variable) String() string {
	return e.ExpressionString()
}

// ExpressionString returns the string representation of the unary operation node.
func (e *unaryOperation) ExpressionString() string {
	var builder strings.Builder

	switch e.operation {
	case Odd:
		builder.WriteString("odd(")

	case Negate:
		builder.WriteString("negate(")

	default:
		builder.WriteString("unary(")
	}

	writeString(&builder, e.operand.String(), ")")
	return builder.String()
}

// String returns the string representation of the unary operation node.
func (e *unaryOperation) String() string {
	return e.ExpressionString()
}

// ExpressionString returns the string representation of the binary operation node.
func (e *binaryOperation) ExpressionString() string {
	var builder strings.Builder

	switch e.operation {
	case Plus:
		builder.WriteString("addition(")

	case Minus:
		builder.WriteString("subtraction(")

	case Times:
		builder.WriteString("multiplication(")

	case Divide:
		builder.WriteString("division(")

	default:
		builder.WriteString("binary(")
	}

	writeString(&builder, e.left.String(), ",", e.right.String(), ")")
	return builder.String()
}

// String returns the string representation of the binary operation node.
func (e *binaryOperation) String() string {
	return e.ExpressionString()
}

// ConditionString returns the string representation of the conditional operation node.
func (e *conditionalOperation) ExpressionString() string {
	var builder strings.Builder

	switch e.operation {
	case Equal:
		builder.WriteString("equal(")

	case NotEqual:
		builder.WriteString("notEqual(")

	case Less:
		builder.WriteString("less(")

	case LessEqual:
		builder.WriteString("lessEqual(")

	case Greater:
		builder.WriteString("greater(")

	case GreaterEqual:
		builder.WriteString("greaterEqual(")

	default:
		builder.WriteString("conditional(")
	}

	writeString(&builder, e.left.String(), ",", e.right.String(), ")")
	return builder.String()
}

// String returns the string representation of the conditional operation node.
func (e *conditionalOperation) String() string {
	return e.ExpressionString()
}

// StatementString returns the string representation of the assignment statement node.
func (s *assignmentStatement) StatementString(depth int) string {
	var builder strings.Builder
	writeString(&builder, indent(depth), "+- assignment(", s.symbol.Name, "=", s.expression.String(), ")\n")
	return builder.String()
}

// StatementString returns the string representation of the read statement node.
func (s *readStatement) StatementString(depth int) string {
	var builder strings.Builder
	writeString(&builder, indent(depth), "+- read(", s.symbol.Name, ")\n")
	return builder.String()
}

// StatementString returns the string representation of the write statement node.
func (s *writeStatement) StatementString(depth int) string {
	var builder strings.Builder
	writeString(&builder, indent(depth), "+- write(", s.expression.String(), ")\n")
	return builder.String()
}

// StatementString returns the string representation of the call statement node.
func (s *callStatement) StatementString(depth int) string {
	var builder strings.Builder
	writeString(&builder, indent(depth), "+- call(", s.symbol.Name, ":", fmt.Sprintf("%v", s.symbol.Depth), ")\n")
	return builder.String()
}

// StatementString returns the string representation of the if-then statement node.
func (s *ifStatement) StatementString(depth int) string {
	var builder strings.Builder

	writeString(&builder, indent(depth), "+- if(", s.condition.String(), ") (\n")
	builder.WriteString(s.statement.StatementString(depth + 1))

	return builder.String()
}

// StatementString returns the string representation of the while-do statement node.
func (s *whileStatement) StatementString(depth int) string {
	var builder strings.Builder

	writeString(&builder, indent(depth), "+- while(", s.condition.String(), ") (\n")
	builder.WriteString(s.statement.StatementString(depth + 1))

	return builder.String()
}

// StatementString returns the string representation of the compound statement node.
func (s *compoundStatement) StatementString(depth int) string {
	var builder strings.Builder

	writeString(&builder, indent(depth), "+- compound\n")

	for _, statement := range s.statements {
		builder.WriteString(statement.StatementString(depth + 1))
	}

	return builder.String()
}

// Return a string with the specified number of spaces.
func indent(depth int) string {
	return strings.Replace(strings.Repeat("|  ", depth), "|", " ", 1)
}

// Write the specified string items to the builder.
func writeString(builder *strings.Builder, items ...string) {
	for _, item := range items {
		builder.WriteString(item)
	}
}

// PrintTree prints the tree structure of the AST.
func PrintTree(node Node, indent string, last bool) {
	fmt.Printf("%v+- %v\n", indent, node.Title())

	if last {
		indent += "   "
	} else {
		indent += "|  "
	}

	for i, child := range node.Children() {
		PrintTree(child, indent, i == len(node.Children())-1)
	}
}
