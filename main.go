package main

import (
	"fmt"
	"os"
	"strconv"
	"unicode"
	"unicode/utf8"
)

const (
	null = token(iota)
	eof
	identifier
	number
	plus
	minus
	times
	devide
	equal
	notEqual
	less
	lessEqual
	greater
	greaterEqual
	leftParenthesis
	rightParenthesis
	comma
	semicolon
	period
	becomes
	oddSymbol
	beginSymbol
	endSymbol
	ifSymbol
	thenSymbol
	whileSymbol
	doSymbol
	callSymbol
	constSymbol
	varSymbol
	procedureSymbol
)

const (
	_ = failure(iota)
	eofReached
	eofIdentifier
	eofNumber
	eofOperator
	tooLongIdentifier
	tooLongNumber
	illegalInteger
	unexpectedCharacter
)

// const (
// 	lit = assembler(iota) // lit 0,a  :  load constant
// 	opr                   // opr 0,a  :  execute operation
// 	lod                   // lod l,a  :  load variable
// 	sto                   // sto l,a  :  store variable
// 	cal                   // cal l,a  :  call procedure
// 	inc                   // inc 0,a  :  increment t-register
// 	jmp                   // jmp 0,a  :  jump to a
// 	jpc                   // jpc 0,a  :  jump conditional to a
// )

// const (
// 	constant = object(iota)
// 	variable
// 	procedure
// )

const (
	reservedWords      = 11   // number of reserved words
	identifierTableLen = 100  // length of identifier table
	digitsMax          = 14   // maximum number of digits in numbers
	identifierMax      = 10   // maximum length of identifier
	addressMax         = 2047 // maximum address
	blockNestingMax    = 3    // maximum depth of block nesting
	codeArrayMax       = 200  // size of code array
	integerBitSize     = 64   // number of bits of a signed integer
)

type (
	token   int
	failure int
	// assembler int
	// object    int

	// alfa   [identifierMax]rune
	// symset map[token]bool

	// instruction struct {
	// 	f assembler // function code
	// 	l int       // level
	// 	a int       // displacement address
	// }

	scanner struct {
		sourceIndex   int
		sourceCode    []byte
		line, column  int
		lastCharacter rune
		lastValue     any
		tokenMap      map[string]token
		tokenNames    map[token]string
		errorMap      map[failure]string
	}

	// parser struct{}

	// emitter struct{}
)

// var (
// 	ch   rune  // last character read
// 	sym  token // last symbol read
// 	id   alfa  // last identifier read
// 	num  int   // last number read
// 	cc   int   // character count
// 	ll   int   // line length
// 	kk   int
// 	err  int
// 	cx   int      // code allocation index
// 	line [81]rune // line buffer
// 	a    alfa     // temporary string

// 	code                              [codeArrayMax]instruction
// 	word                              [reservedWords]alfa
// 	wsym                              [reservedWords]token
// 	ssym                              [128]token
// 	mnemonic                          [opr]alfa // mnemonic for operators
// 	declbegsys, statbegsys, facbegsys symset

// 	table [identifierTableLen]struct {
// 		name  alfa
// 		kind  object
// 		val   int
// 		level int
// 		adr   int
// 	}
// )

func NewScanner() *scanner {
	return &scanner{
		tokenMap: map[string]token{
			"+":         plus,
			"-":         minus,
			"*":         times,
			"/":         devide,
			"=":         equal,
			"#":         notEqual,
			"<":         less,
			"<=":        lessEqual,
			">":         greater,
			">=":        greaterEqual,
			"(":         leftParenthesis,
			")":         rightParenthesis,
			",":         comma,
			";":         semicolon,
			".":         period,
			":=":        becomes,
			"odd":       oddSymbol,
			"begin":     beginSymbol,
			"end":       endSymbol,
			"if":        ifSymbol,
			"then":      thenSymbol,
			"while":     whileSymbol,
			"do":        doSymbol,
			"call":      callSymbol,
			"const":     constSymbol,
			"var":       varSymbol,
			"procedure": procedureSymbol,
		},
		tokenNames: map[token]string{
			null:             "null",
			eof:              "eof",
			identifier:       "identifier",
			number:           "number",
			plus:             "plus",
			minus:            "minus",
			times:            "times",
			devide:           "devide",
			equal:            "equal",
			notEqual:         "notEqual",
			less:             "less",
			lessEqual:        "lessEqual",
			greater:          "greater",
			greaterEqual:     "greaterEqual",
			leftParenthesis:  "leftParenthesis",
			rightParenthesis: "rightParenthesis",
			comma:            "comma",
			semicolon:        "semicolon",
			period:           "period",
			becomes:          "becomes",
			oddSymbol:        "odd",
			beginSymbol:      "begin",
			endSymbol:        "end",
			ifSymbol:         "if",
			thenSymbol:       "then",
			whileSymbol:      "while",
			doSymbol:         "do",
			callSymbol:       "call",
			constSymbol:      "const",
			varSymbol:        "var",
			procedureSymbol:  "procedure",
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

func (s *scanner) LoadSource(sourceFilePath string) error {
	if content, err := os.ReadFile(sourceFilePath); err != nil {
		return err
	} else {
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
}

func (s *scanner) GetToken() (token, error) {
	s.lastValue = ""

	for unicode.IsSpace(s.lastCharacter) {
		if !s.nextCharacter() {
			return eof, nil
		}
	}

	if unicode.IsLetter(s.lastCharacter) {
		for unicode.IsLetter(s.lastCharacter) || unicode.IsDigit(s.lastCharacter) {
			s.lastValue = s.lastValue.(string) + string(s.lastCharacter)

			if !s.nextCharacter() {
				return identifier, s.error(eofIdentifier, s.lastValue)
			}
		}

		if token, ok := s.tokenMap[s.lastValue.(string)]; ok {
			return token, nil
		}

		if len(s.lastValue.(string)) > identifierMax {
			return identifier, s.error(tooLongIdentifier, s.lastValue)
		}

		return identifier, nil
	} else if unicode.IsDigit(s.lastCharacter) {
		for unicode.IsDigit(s.lastCharacter) {
			s.lastValue = s.lastValue.(string) + string(s.lastCharacter)

			if !s.nextCharacter() {
				return number, s.error(eofNumber, s.lastValue)
			}
		}

		if len(s.lastValue.(string)) > digitsMax {
			return number, s.error(tooLongNumber, s.lastValue)
		}

		if intValue, err := strconv.ParseInt(s.lastValue.(string), 10, integerBitSize); err != nil {
			return number, s.error(illegalInteger, s.lastValue)
		} else {
			s.lastValue = intValue
			return number, nil
		}
	} else if s.lastCharacter == ':' {
		if s.nextCharacter() && s.lastCharacter == '=' {
			if !s.nextCharacter() {
				return becomes, s.error(eofReached, nil)
			}

			return becomes, nil
		} else {
			return null, s.error(unexpectedCharacter, s.lastCharacter)
		}
	} else if token, ok := s.tokenMap[string(s.lastCharacter)]; ok {
		if token == less || token == greater {
			s.lastValue = string(s.lastCharacter)
		}

		if !s.nextCharacter() {
			if token == period {
				s.lastCharacter = ' '
			} else {
				return token, s.error(eofReached, nil)
			}
		}

		if (token == less || token == greater) && s.lastCharacter == '=' {
			s.lastValue = s.lastValue.(string) + string(s.lastCharacter)
			token = s.tokenMap[s.lastValue.(string)]

			if !s.nextCharacter() {
				return token, s.error(eofOperator, s.lastValue)
			}
		}

		return token, nil
	} else {
		return null, s.error(unexpectedCharacter, s.lastCharacter)
	}
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
	} else {
		return false
	}
}

func (s *scanner) error(code failure, value any) error {
	var message string

	if value != nil {
		message = fmt.Sprintf(s.errorMap[code], value)
	} else {
		message = s.errorMap[code]
	}

	return fmt.Errorf("error %v [%v,%v]: %v", code, s.line, s.column, message)
}

func main() {
	fmt.Println("PL/0 Compiler Version 0.1")
	fmt.Println("Copyright (c) 2024, Michael Petersen. All rights reserved.")

	if len(os.Args) < 2 {
		fmt.Println("Usage: pl0 <source file>")
		fmt.Println("error: no source file specified")
	} else {
		scanner := NewScanner()

		if err := scanner.LoadSource(os.Args[1]); err != nil {
			fmt.Println("error: source file not found")
		} else {
			fmt.Println("Compiling source file:", os.Args[1])

			for {
				token, err := scanner.GetTokenName()

				if err != nil {
					fmt.Println(err)
					return
				}

				switch token {
				case "identifier":
					fmt.Println(token, scanner.GetTokenValue().(string))

				case "number":
					fmt.Println(token, scanner.GetTokenValue().(int64))

				case "eof":
					fmt.Println("Compilation successful")
					return

				default:
					fmt.Println(token)
				}
			}
		}
	}
}
