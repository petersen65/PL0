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
	Token          int
	ConcreteSyntax []TokenDescription

	TokenDescription struct {
		Token                 Token
		TokenName, TokenValue string
		Line, Column          int
		CurrentLine           []byte
	}

	Scanner interface {
		Scan(content []byte) (ConcreteSyntax, error)
	}
)
