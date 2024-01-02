package scanner

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"
)

const (
	digitsMax      = 14 // maximum number of digits in numbers
	identifierMax  = 10 // maximum length of identifier
	integerBitSize = 64 // number of bits of a signed integer
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

type (
	failure int

	scanner struct {
		sourceIndex   int
		sourceCode    []byte
		line, column  int
		lastCharacter rune
		lastValue     any
		currentLine   []byte
		endOfFile     bool
		tokenMap      map[string]Token
		tokenNames    map[Token]string
		errorMap      map[failure]string
	}
)

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
			unexpectedCharacter: "unexpected character '%c'",
		},
	}
}

func (s *scanner) Scan(content []byte) (ConcreteSyntax, error) {
	concreteSyntax := ConcreteSyntax{}

	if err := s.reset(content); err != nil {
		return concreteSyntax, err
	}

	for {
		token, err := s.getToken()

		concreteSyntax = append(concreteSyntax, TokenDescription{
			Token:       token,
			TokenName:   s.tokenNames[token],
			TokenValue:  fmt.Sprintf("%v", s.lastValue),
			Line:        s.line,
			Column:      s.column,
			CurrentLine: s.currentLine,
		})

		if err != nil {
			return concreteSyntax, err
		}

		if token == Eof {
			return concreteSyntax, nil
		}
	}
}

func (s *scanner) reset(content []byte) error {
	s.sourceIndex = 0
	s.sourceCode = content
	s.line = 0
	s.column = 0
	s.lastValue = ""
	s.currentLine = []byte{}
	s.endOfFile = false

	if len(content) == 0 || !s.nextCharacter() {
		return s.error(eofReached, nil)
	}

	return nil
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

	case s.lastCharacter == ':':
		return s.becomes()

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

	if token, ok := s.tokenMap[builder.String()]; ok {
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

	if intValue, err := strconv.ParseInt(builder.String(), 10, integerBitSize); err != nil {
		return Number, s.error(illegalInteger, builder.String())
	} else {
		s.lastValue = intValue
		return Number, nil
	}
}

func (s *scanner) becomes() (Token, error) {
	if !s.nextCharacter() {
		return Becomes, s.error(eofOperator, ":=")
	}

	if s.lastCharacter == '=' {
		if !s.nextCharacter() {
			return Becomes, s.error(eofOperator, ":=")
		}

		return Becomes, nil
	} else {
		return Becomes, s.error(unexpectedCharacter, s.lastCharacter)
	}
}

func (s *scanner) operator() (Token, error) {
	var builder strings.Builder

	if token, ok := s.tokenMap[string(s.lastCharacter)]; ok {
		if token == Less || token == Greater {
			builder.WriteRune(s.lastCharacter)
		}

		if !s.nextCharacter() && token != Period {
			return token, s.error(eofReached, nil)
		}

		if (token == Less || token == Greater) && s.lastCharacter == '=' {
			builder.WriteRune(s.lastCharacter)
			token = s.tokenMap[builder.String()]

			if !s.nextCharacter() {
				return token, s.error(eofOperator, builder.String())
			}
		}

		return token, nil
	}

	return Null, s.error(unexpectedCharacter, s.lastCharacter)
}

func (s *scanner) nextCharacter() bool {
	if s.sourceIndex >= len(s.sourceCode) {
		s.endOfFile = true
		return false
	}

	character, width := utf8.DecodeRune(s.sourceCode[s.sourceIndex:])

	if character == utf8.RuneError {
		s.lastCharacter = ' '
	} else {
		s.lastCharacter = character
	}

	if s.line == 0 {
		s.line = 1
		s.column = 0
		s.setCurrentLine()
	}

	if character == '\n' {
		s.line++
		s.column = 0
		s.setCurrentLine()
	} else {
		s.column++
	}

	s.sourceIndex += width
	return true
}

func (s *scanner) setCurrentLine() {
	s.currentLine = []byte{}
	startIndex := s.sourceIndex

	if startIndex != 0 {
		startIndex++
	}

	for i := startIndex; i < len(s.sourceCode); {
		character, width := utf8.DecodeRune(s.sourceCode[i:])

		if character == '\n' {
			break
		}

		if character == utf8.RuneError {
			character = ' '
		}

		if character != '\r' {
			s.currentLine = utf8.AppendRune(s.currentLine, character)
		}

		i += width
	}
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
