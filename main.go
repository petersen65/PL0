package main

import (
	"fmt"
	"os"
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
	identifierLen      = 10   // length of identifier
	addressMax         = 2047 // maximum address
	blockNestingMax    = 3    // maximum depth of block nesting
	codeArrayMax       = 200  // size of code array
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
		sourceIndex int
		sourceCode  []byte
		lastToken   token
		tokenMap    map[string]token
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
		s.lastToken = null
		s.sourceCode = content
		return nil
	}
}

func (s *scanner) GetToken() token {
	if s.sourceIndex < len(s.sourceCode) {
		if runeValue, width := utf8.DecodeRune(s.sourceCode[s.sourceIndex:]); runeValue == utf8.RuneError {
			s.lastToken = null
		} else {
			s.sourceIndex += width

			// Add your logic to generate token from runeValue
		}
	} else {
		s.lastToken = eof
	}

	return s.lastToken
}

func (s *scanner) nextRune() (rune, bool) {
	return 0, true
}

func main() {
	fmt.Println("PL0 Compiler Version 0.1")
	fmt.Println("Written in Go by Michael Petersen 2024")

	if len(os.Args) < 2 {
		fmt.Println("Usage: pl0 <source file>")
		fmt.Println("Error: no source file specified")
	} else {
		scanner := NewScanner()

		if err := scanner.LoadSource(os.Args[1]); err != nil {
			fmt.Println("Error: source file not found")
			return
		} else {
			fmt.Println("Source file loaded")
		}
	}
}
