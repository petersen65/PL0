package main

import (
	"fmt"
	"os"
	"strconv"
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
	lit = assembler(iota) // lit 0,a  :  load constant
	opr                   // opr 0,a  :  execute operation
	lod                   // lod l,a  :  load variable
	sto                   // sto l,a  :  store variable
	cal                   // cal l,a  :  call procedure
	inc                   // inc 0,a  :  increment t-register
	jmp                   // jmp 0,a  :  jump to a
	jpc                   // jpc 0,a  :  jump conditional to a
)

const (
	constant = object(iota)
	variable
	procedure
)

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
	token     int
	assembler int
	object    int

	alfa   [identifierLen]rune
	symset map[token]bool

	instruction struct {
		f assembler // function code
		l int       // level
		a int       // displacement address
	}

	scanner struct {
		sourceIndex   int
		sourceCode    []byte
		lastCharacter rune
		lastValue     any
		tokenMap      map[string]token
	}

	parser struct{}

	emitter struct{}
)

var (
	ch   rune  // last character read
	sym  token // last symbol read
	id   alfa  // last identifier read
	num  int   // last number read
	cc   int   // character count
	ll   int   // line length
	kk   int
	err  int
	cx   int      // code allocation index
	line [81]rune // line buffer
	a    alfa     // temporary string

	code                              [codeArrayMax]instruction
	word                              [reservedWords]alfa
	wsym                              [reservedWords]token
	ssym                              [128]token
	mnemonic                          [opr]alfa // mnemonic for operators
	declbegsys, statbegsys, facbegsys symset

	table [identifierTableLen]struct {
		name  alfa
		kind  object
		val   int
		level int
		adr   int
	}
)

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
	}
}

func (s *scanner) LoadSource(sourceFilePath string) error {
	if content, err := os.ReadFile(sourceFilePath); err != nil {
		return err
	} else {
		s.sourceIndex = 0
		s.sourceCode = content
		s.lastValue = nil

		if !s.nextCharacter() {
			return fmt.Errorf("error: unexpected end of file")
		} else {
			return nil
		}
	}
}

func (s *scanner) GetToken() (token, error) {
	for s.lastCharacter == ' ' || s.lastCharacter == '\t' || s.lastCharacter == '\n' || s.lastCharacter == '\r' {
		if !s.nextCharacter() {
			return eof, nil
		}
	}

	if s.lastCharacter >= 'a' && s.lastCharacter <= 'z' || s.lastCharacter >= 'A' && s.lastCharacter <= 'Z' {
		s.lastValue = ""

		for s.lastCharacter >= 'a' && s.lastCharacter <= 'z' || s.lastCharacter >= 'A' && s.lastCharacter <= 'Z' || s.lastCharacter >= '0' && s.lastCharacter <= '9' {
			s.lastValue = s.lastValue.(string) + string(s.lastCharacter)

			if !s.nextCharacter() {
				return eof, fmt.Errorf("error: unexpected end of file while reading identifier %s", s.lastValue)
			}
		}

		if token, ok := s.tokenMap[s.lastValue.(string)]; ok {
			return token, nil
		}

		if len(s.lastValue.(string)) > identifierMax {
			return identifier, fmt.Errorf("error: identifier %s too long", s.lastValue)
		}

		return identifier, nil
	} else if s.lastCharacter >= '0' && s.lastCharacter <= '9' {
		s.lastValue = ""

		for s.lastCharacter >= '0' && s.lastCharacter <= '9' {
			s.lastValue = s.lastValue.(string) + string(s.lastCharacter)

			if !s.nextCharacter() {
				return eof, fmt.Errorf("error: unexpected end of file while reading number %s", s.lastValue)
			}
		}

		if len(s.lastValue.(string)) > digitsMax {
			return number, fmt.Errorf("error: number %s too large", s.lastValue)
		}

		if intValue, err := strconv.ParseInt(s.lastValue.(string), 10, integerBitSize); err != nil {
			return number, fmt.Errorf("error: cannot parse number %s into integer value", s.lastValue)
		} else {
			s.lastValue = intValue
			return number, nil
		}
	} else if s.lastCharacter == ':' {
		if s.nextCharacter() && s.lastCharacter == '=' {
			if !s.nextCharacter() {
				return eof, fmt.Errorf("error: unexpected end of file")
			}

			return becomes, nil
		} else {
			return null, fmt.Errorf("syntax error: unexpected character %c", s.lastCharacter)
		}
	} else if token, ok := s.tokenMap[string(s.lastCharacter)]; ok {
		return token, nil
	} else {
		return null, fmt.Errorf("syntax error: unexpected character %c", s.lastCharacter)
	}
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

		return true
	} else {
		return false
	}
}

func main() {
	fmt.Println("PL0 Compiler Version 0.1")
	fmt.Println("Written in Go by Michael Petersen 2024")

	if len(os.Args) < 2 {
		fmt.Println("Usage: pl0 <source file>")
		fmt.Println("error: no source file specified")
	} else {
		scanner := NewScanner()

		if err := scanner.LoadSource(os.Args[1]); err != nil {
			fmt.Println("error: source file not found")
		} else {
			fmt.Println("Compiling source file")
		}
	}
}
