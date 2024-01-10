// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package scanner

import "unicode"

var (
	tokenMap = map[string]Token{
		"+":         Plus,
		"-":         Minus,
		"*":         Times,
		"/":         Divide,
		"=":         Equal,
		"#":         NotEqual,
		"<":         Less,
		"<=":        LessEqual,
		">":         Greater,
		">=":        GreaterEqual,
		"(":         LeftParenthesis,
		")":         RightParenthesis,
		",":         Comma,
		";":         Semicolon,
		".":         Period,
		":=":        Becomes,
		"odd":       OddWord,
		"begin":     BeginWord,
		"end":       EndWord,
		"if":        IfWord,
		"then":      ThenWord,
		"while":     WhileWord,
		"do":        DoWord,
		"call":      CallWord,
		"const":     ConstWord,
		"var":       VarWord,
		"procedure": ProcedureWord,
	}

	tokenNames = map[Token]string{
		Null:             "null",
		Eof:              "eof",
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
		Semicolon:        "semicolon",
		Period:           "period",
		Becomes:          "becomes",
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
)

func (s *scanner) getToken() (Token, error) {
	s.lastValue = ""

	if s.endOfFile {
		return Eof, nil
	}

	for unicode.IsSpace(s.lastCharacter) {
		if !s.nextCharacter() {
			return Eof, nil
		}
	}

	switch {
	case unicode.IsLetter(s.lastCharacter):
		return s.identifierOrWord()

	case unicode.IsDigit(s.lastCharacter):
		return s.number()

	case s.lastCharacter == ':':
		return s.becomes()

	default:
		return s.operator()
	}
}
