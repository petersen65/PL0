package scanner

import "fmt"

const (
	digitsMax     = 14
	identifierMax = 10
)

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

var errorMap = map[failure]string{
	eofReached:          "unexpected end of file",
	eofIdentifier:       "unexpected end of file while reading identifier %s",
	eofNumber:           "unexpected end of file while reading number %s",
	eofOperator:         "unexpected end of file while reading operator %s",
	tooLongIdentifier:   "identifier %s is too long",
	tooLongNumber:       "number %s is too long",
	illegalInteger:      "cannot parse number %s into integer value",
	unexpectedCharacter: "unexpected character '%c'",
}

func (s *scanner) error(code failure, value any) error {
	var message string

	if value != nil {
		message = fmt.Sprintf(errorMap[code], value)
	} else {
		message = errorMap[code]
	}

	return fmt.Errorf("scanner error %v [%v,%v]: %v", code, s.line, s.column, message)
}