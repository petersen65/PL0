// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package scanner

import (
	"strings"
	"unicode"
)

var tokenMap = map[string]Token{
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
	":":         Colon,
	";":         Semicolon,
	".":         Period,
	":=":        Becomes,
	"?":         Read,
	"!":         Write,
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

	default:
		return s.operator()
	}
}

func (s *scanner) identifierOrWord() (Token, error) {
	var builder strings.Builder

	for unicode.IsLetter(s.lastCharacter) || unicode.IsDigit(s.lastCharacter) {
		builder.WriteRune(s.lastCharacter)

		if !s.nextCharacter() {
			return Identifier, s.error(eofIdentifier, builder.String())
		}
	}

	if token, ok := tokenMap[builder.String()]; ok {
		return token, nil
	}

	if len(builder.String()) > identifierMax {
		return Identifier, s.error(tooLongIdentifier, builder.String())
	}

	s.lastValue = builder.String()
	return Identifier, nil
}

func (s *scanner) number() (Token, error) {
	var builder strings.Builder

	for unicode.IsDigit(s.lastCharacter) {
		builder.WriteRune(s.lastCharacter)

		if !s.nextCharacter() {
			return Number, s.error(eofNumber, builder.String())
		}
	}

	if len(builder.String()) > digitsMax {
		return Number, s.error(tooLongNumber, builder.String())
	}

	s.lastValue = builder.String()
	return Number, nil
}

func (s *scanner) operator() (Token, error) {
	if token, ok := tokenMap[string(s.lastCharacter)]; ok {
		if !s.nextCharacter() && token != Period {
			return token, s.error(eofReached, nil)
		}

		return token, nil
	}

	return Null, s.error(unexpectedCharacter, s.lastCharacter)
}
