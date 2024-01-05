package scanner

import (
	"slices"
)

const (
	Null = Token(iota)
	Eof
	Identifier
	Number
	Plus
	Minus
	Times
	Devide
	Equal
	NotEqual
	Less
	LessEqual
	Greater
	GreaterEqual
	LeftParenthesis
	RightParenthesis
	Comma
	Semicolon
	Period
	Becomes
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

type (
	Token          int
	Tokens         []Token
	ConcreteSyntax []TokenDescription

	TokenDescription struct {
		Token                 Token
		TokenName, TokenValue string
		Line, Column          int
		CurrentLine           []byte
	}

	TokenSet interface {
		ToTokens() Tokens
	}

	Scanner interface {
		Scan(content []byte) (ConcreteSyntax, error)
	}
)

var Empty = Tokens{}

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

	return set
}

func (t Token) ToTokens() Tokens {
	return Tokens{t}
}

func (t Tokens) ToTokens() Tokens {
	return t
}

func (token Token) In(set Tokens) bool {
	return slices.Contains(set, token)
}
