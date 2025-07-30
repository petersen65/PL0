// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package pl0

import cor "github.com/petersen65/PL0/v2/core"

// Map tokens to their string representation.
var tokenNames = map[cor.Token]string{
	Unknown:          "unknown",
	Identifier:       "identifier",
	Number:           "number",
	Plus:             "plus",
	Minus:            "minus",
	Times:            "times",
	Divide:           "divide",
	Equal:            "equal",
	NotEqual:         "notEqual",
	Less:             "less",
	LessEqual:        "lessEqual",
	Greater:          "greater",
	GreaterEqual:     "greaterEqual",
	LeftParenthesis:  "leftParenthesis",
	RightParenthesis: "rightParenthesis",
	Comma:            "comma",
	Colon:            "colon",
	Semicolon:        "semicolon",
	ProgramEnd:       "programEnd",
	Becomes:          "becomes",
	Read:             "read",
	Write:            "write",
	OddWord:          "odd",
	BeginWord:        "begin",
	EndWord:          "end",
	IfWord:           "if",
	ThenWord:         "then",
	WhileWord:        "while",
	DoWord:           "do",
	CallWord:         "call",
	ConstWord:        "const",
	VarWord:          "var",
	ProcedureWord:    "procedure",
}
