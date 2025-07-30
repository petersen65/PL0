// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package pl0 implements programming language specific constants and types for PL/0.
// PL/0 is a simple imperative programming language designed for teaching purposes.
package pl0

import cor "github.com/petersen65/PL0/v2/core"

// Tokens of the PL/0 programming language.
const (
	Unknown cor.Token = iota
	Identifier
	Number
	Plus
	Minus
	Times
	Divide
	Equal
	NotEqual
	Less
	LessEqual
	Greater
	GreaterEqual
	LeftParenthesis
	RightParenthesis
	Comma
	Colon
	Semicolon
	ProgramEnd
	Becomes
	Read
	Write
	OddWord
	BeginWord
	EndWord
	IfWord
	ThenWord
	WhileWord
	DoWord
	CallWord
	ConstWord
	VarWord
	ProcedureWord
)
