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
	OddSymbol
	BeginSymbol
	EndSymbol
	IfSymbol
	ThenSymbol
	WhileSymbol
	DoSymbol
	CallSymbol
	ConstSymbol
	VarSymbol
	ProcedureSymbol
)

type Token int

type Scanner interface {
	LoadSource(sourceFilePath string) error
	GetToken() (Token, error)
	GetTokenName() (string, error)
	GetTokenPosition() (int, int)
	GetTokenValue() any
}
