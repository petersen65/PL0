package scanner

const (
	_ = failure(iota + 1000)
	eofReached
	eofIdentifier
	eofNumber
	eofOperator
	tooLongIdentifier
	tooLongNumber
	illegalInteger
	unexpectedCharacter
)

type failure int

type scanner struct {
	sourceIndex   int
	sourceCode    []byte
	line, column  int
	lastCharacter rune
	lastValue     any
	tokenMap      map[string]Token
	tokenNames    map[Token]string
	errorMap      map[failure]string
}

