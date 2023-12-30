package scanner

import (
	"fmt"
	"strconv"
	"unicode"
	"unicode/utf8"
)

const (
	reservedWords      = 11   // number of reserved words
	identifierTableLen = 100  // length of identifier table
	digitsMax          = 14   // maximum number of digits in numbers
	identifierMax      = 10   // maximum length of identifier
	addressMax         = 2047 // maximum address
	codeArrayMax       = 200  // size of code array
	integerBitSize     = 64   // number of bits of a signed integer
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

func NewScanner() Scanner {
	return &scanner{
		tokenMap: map[string]Token{
			"+":         Plus,
			"-":         Minus,
			"*":         Times,
			"/":         Devide,
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
		},
		tokenNames: map[Token]string{
			Null:             "null",
			Eof:              "eof",
			Identifier:       "identifier",
			Number:           "number",
			Plus:             "plus",
			Minus:            "minus",
			Times:            "times",
			Devide:           "devide",
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
		},
		errorMap: map[failure]string{
			eofReached:          "unexpected end of file",
			eofIdentifier:       "unexpected end of file while reading identifier %s",
			eofNumber:           "unexpected end of file while reading number %s",
			eofOperator:         "unexpected end of file while reading operator %s",
			tooLongIdentifier:   "identifier %s is too long",
			tooLongNumber:       "number %s is too long",
			illegalInteger:      "cannot parse number %s into integer value",
			unexpectedCharacter: "unexpected character %c",
		},
	}
}

func (s *scanner) ResetSource(content []byte) error {
	s.sourceIndex = 0
	s.sourceCode = content
	s.line = 0
	s.column = 0
	s.lastValue = ""

	if !s.nextCharacter() {
		return s.error(eofReached, nil)
	} else {
		return nil
	}
}

func (s *scanner) GetToken() (Token, error) {
	s.lastValue = ""

	for unicode.IsSpace(s.lastCharacter) {
		if !s.nextCharacter() {
			return Eof, nil
		}
	}

	if unicode.IsLetter(s.lastCharacter) {
		for unicode.IsLetter(s.lastCharacter) || unicode.IsDigit(s.lastCharacter) {
			s.lastValue = s.lastValue.(string) + string(s.lastCharacter)

			if !s.nextCharacter() {
				return Identifier, s.error(eofIdentifier, s.lastValue)
			}
		}

		if token, ok := s.tokenMap[s.lastValue.(string)]; ok {
			return token, nil
		}

		if len(s.lastValue.(string)) > identifierMax {
			return Identifier, s.error(tooLongIdentifier, s.lastValue)
		}

		return Identifier, nil
	} else if unicode.IsDigit(s.lastCharacter) {
		for unicode.IsDigit(s.lastCharacter) {
			s.lastValue = s.lastValue.(string) + string(s.lastCharacter)

			if !s.nextCharacter() {
				return Number, s.error(eofNumber, s.lastValue)
			}
		}

		if len(s.lastValue.(string)) > digitsMax {
			return Number, s.error(tooLongNumber, s.lastValue)
		}

		if intValue, err := strconv.ParseInt(s.lastValue.(string), 10, integerBitSize); err != nil {
			return Number, s.error(illegalInteger, s.lastValue)
		} else {
			s.lastValue = intValue
			return Number, nil
		}
	} else if s.lastCharacter == ':' {
		if s.nextCharacter() && s.lastCharacter == '=' {
			if !s.nextCharacter() {
				return Becomes, s.error(eofReached, nil)
			}

			return Becomes, nil
		} else {
			return Null, s.error(unexpectedCharacter, s.lastCharacter)
		}
	} else if token, ok := s.tokenMap[string(s.lastCharacter)]; ok {
		if token == Less || token == Greater {
			s.lastValue = string(s.lastCharacter)
		}

		if !s.nextCharacter() {
			if token == Period {
				s.lastCharacter = ' '
			} else {
				return token, s.error(eofReached, nil)
			}
		}

		if (token == Less || token == Greater) && s.lastCharacter == '=' {
			s.lastValue = s.lastValue.(string) + string(s.lastCharacter)
			token = s.tokenMap[s.lastValue.(string)]

			if !s.nextCharacter() {
				return token, s.error(eofOperator, s.lastValue)
			}
		}

		return token, nil
	}

	return Null, s.error(unexpectedCharacter, s.lastCharacter)
}

func (s *scanner) GetTokenName() (string, error) {
	token, err := s.GetToken()
	return s.tokenNames[token], err
}

func (s *scanner) GetTokenPosition() (int, int) {
	return s.line, s.column
}

func (s *scanner) GetTokenValue() any {
	return s.lastValue
}

func (s *scanner) nextCharacter() bool {
	if s.sourceIndex < len(s.sourceCode) {
		character, width := utf8.DecodeRune(s.sourceCode[s.sourceIndex:])
		s.sourceIndex += width

		if character == utf8.RuneError {
			s.lastCharacter = 0
		} else {
			s.lastCharacter = character
		}

		if s.line == 0 {
			s.line = 1
			s.column = 0
		}

		if character == '\n' {
			s.line++
			s.column = 0
		} else {
			s.column++
		}

		return true
	}

	return false
}

func (s *scanner) error(code failure, value any) error {
	var message string

	if value != nil {
		message = fmt.Sprintf(s.errorMap[code], value)
	} else {
		message = s.errorMap[code]
	}

	return fmt.Errorf("scanner error %v [%v,%v]: %v", code, s.line, s.column, message)
}
