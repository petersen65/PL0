// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package scanner

import "slices"

const IntegerBitSize = 64 // number of bits of a signed integer

const (
	Null = Token(iota)
	Eof
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
	Period
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

const (
	None = TokenType(iota)
	Integer64
)

type (
	Token          int
	TokenType      int
	Tokens         []Token
	ConcreteSyntax []TokenDescription

	TokenDescription struct {
		Token        Token
		TokenName    string
		TokenValue   any
		TokenType    TokenType
		Line, Column int
		CurrentLine  []byte
	}

	TokenSet interface {
		ToTokens() Tokens
	}

	Scanner interface {
		Scan(content []byte) (ConcreteSyntax, error)
	}
)

var (
	Empty = Tokens{}

	TokenNames = map[Token]string{
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
		Colon:            "colon",
		Semicolon:        "semicolon",
		Period:           "period",
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

	KeyWords = Tokens{
		OddWord,
		BeginWord,
		EndWord,
		IfWord,
		ThenWord,
		WhileWord,
		DoWord,
		CallWord,
		ConstWord,
		VarWord,
		ProcedureWord,
	}

	Operators = Tokens{
		OddWord,
		Plus,
		Minus,
		Times,
		Divide,
		Equal,
		NotEqual,
		Less,
		LessEqual,
		Greater,
		GreaterEqual,
	}

	Declarations = Tokens{
		ConstWord,
		VarWord,
		ProcedureWord,
	}

	Statements = Tokens{
		Read,
		Write,
		BeginWord,
		CallWord,
		IfWord,
		WhileWord,
	}

	Factors = Tokens{
		Identifier,
		Number,
		LeftParenthesis,
	}

	NoSign = Tokens{
		Identifier,
		Number,
		RightParenthesis,
	}

)

func NewScanner() Scanner {
	return &scanner{}
}

func (s *scanner) Scan(content []byte) (ConcreteSyntax, error) {
	return s.scan(content)
}

func Set(tss ...TokenSet) Tokens {
	set := Tokens{}

	for _, ts := range tss {
		set = append(set, ts.ToTokens()...)
	}

	slices.Sort(set)
	return slices.Compact(set)
}

func (token Token) In(set Tokens) bool {
	return slices.Contains(set, token)
}

func (t Token) ToTokens() Tokens {
	return Tokens{t}
}

func (t Tokens) ToTokens() Tokens {
	return t
}
