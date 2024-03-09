// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package ast

func (s Symbol) Children() []Node {
	return make([]Node, 0)
}

func (s SourceDescription) Children() []Node {
	return make([]Node, 0)
}

func (e *literal) Children() []Node {
	return []Node{e.source}
}

func (e *constant) Children() []Node {
	return []Node{e.symbol, e.source}
}

func (e *variable) Children() []Node {
	return []Node{e.symbol, e.source}
}

func (e *unaryOperation) Children() []Node {
	return []Node{e.operand, e.source}
}

func (e *binaryOperation) Children() []Node {
	return []Node{e.left, e.right, e.source}
}

func (e *conditionalOperation) Children() []Node {
	return []Node{e.left, e.right, e.source}
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

func (s *assignmentStatement) Children() []Node {
	return []Node{s.symbol, s.expression, s.source}
}

func (s *readStatement) Children() []Node {
	return []Node{s.symbol, s.source}
}

func (s *writeStatement) Children() []Node {
	return []Node{s.expression, s.source}
}

func (s *callStatement) Children() []Node {
	return []Node{s.symbol, s.source}
}

func (s *ifStatement) Children() []Node {
	return []Node{s.condition, s.statement, s.source}
}

func (s *whileStatement) Children() []Node {
	return []Node{s.condition, s.statement, s.source}
}

func (c *compoundStatement) Children() []Node {
	children := make([]Node, 0, len(c.statements)+1)

	for _, statement := range c.statements {
		children = append(children, statement)
	}

	return append(children, c.source)
}
