package scanner

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
	Token int

	Scanner interface {
		ResetSource(content []byte) error
		GetToken() (Token, error)
		GetTokenName() (string, error)
		GetTokenPosition() (int, int)
		GetTokenLine() []byte
		GetTokenValue() any
	}
)
