// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package scanner

import (
	"strings"
	"unicode"
	"unicode/utf8"

	cor "github.com/petersen65/PL0/v2/core"
	pl0 "github.com/petersen65/PL0/v2/pl0"
)

// Last character of the source code that is read when the end of the file is reached.
const endOfFileCharacter = 0

// Implementation of the scanner.
type scanner struct {
	sourceIndex   int    // index of the current character in the source code byte slice
	sourceCode    []byte // source code to scan
	line, column  int    // current line and column where the scanner is positioned
	lastCharacter rune   // last UTF-8 character that was read
	lastValue     string // last identifier or number value that was read
	currentLine   []byte // current line of source code that is being scanned
}

var (
	// Map operator characters to their corresponding tokens.
	operators = map[string]cor.Token{
		"+":  pl0.Plus,
		"-":  pl0.Minus,
		"*":  pl0.Times,
		"/":  pl0.Divide,
		"=":  pl0.Equal,
		"#":  pl0.NotEqual,
		"<":  pl0.Less,
		"<=": pl0.LessEqual,
		">":  pl0.Greater,
		">=": pl0.GreaterEqual,
		"(":  pl0.LeftParenthesis,
		")":  pl0.RightParenthesis,
		":=": pl0.Becomes,
		",":  pl0.Comma,
		":":  pl0.Colon,
		";":  pl0.Semicolon,
		".":  pl0.ProgramEnd,
	}

	// Map statement characters to their corresponding tokens.
	statements = map[string]cor.Token{
		"?": pl0.Read,
		"!": pl0.Write,
	}

	// Map reserved words to their corresponding tokens.
	words = map[string]cor.Token{
		"odd":       pl0.OddWord,
		"begin":     pl0.BeginWord,
		"end":       pl0.EndWord,
		"if":        pl0.IfWord,
		"then":      pl0.ThenWord,
		"while":     pl0.WhileWord,
		"do":        pl0.DoWord,
		"call":      pl0.CallWord,
		"const":     pl0.ConstWord,
		"var":       pl0.VarWord,
		"procedure": pl0.ProcedureWord,
	}
)

// Return the interface of the scanner implementation.
func newScanner() Scanner {
	return &scanner{}
}

// Run the scanner to map the source code to its corresponding token stream.
func (s *scanner) Scan(content []byte) (cor.TokenStream, error) {
	s.sourceIndex = 0
	s.sourceCode = content
	s.line = 0
	s.column = 0
	s.lastValue = ""
	s.currentLine = make([]byte, 0)

	s.nextCharacter()
	return s.scan()
}

// Perform a character scan that supports whitespace, comments, identifiers, reserved words, numbers and operators.
func (s *scanner) scan() (cor.TokenStream, error) {
	tokenStream := make(cor.TokenStream, 0)

	for {
		for s.isWhitespace() || s.isComment() {
			if s.isWhitespace() {
				s.whitespace()
			}

			if s.isComment() {
				if err := s.comment(); err != nil {
					return tokenStream, err
				}
			}
		}

		if s.isEndOfContent() {
			return tokenStream, nil
		}

		token := s.getToken()

		tokenStream = append(tokenStream, cor.TokenDescription{
			Token:       token,
			TokenName:   token.String(),
			TokenValue:  s.lastValue,
			Line:        s.line,
			Column:      s.column,
			CurrentLine: s.currentLine,
		})
	}
}

// Return identifier, reserved word, number, or operator token.
func (s *scanner) getToken() cor.Token {
	s.lastValue = ""

	switch {
	case s.isIdentifierOrWord():
		return s.identifierOrWord()

	case s.isNumber():
		return s.number()

	default:
		return s.operatorOrStatement()
	}
}

// Read the next UTF-8 character from the source code and update the line and column counters.
func (s *scanner) nextCharacter() {
	if s.sourceIndex >= len(s.sourceCode) {
		s.lastCharacter = endOfFileCharacter
		return
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
}

// Peek the next UTF-8 character from the source code to check if it matches an expected character.
func (s *scanner) peekCharacter(expected rune) bool {
	if s.isEndOfContent() {
		return false
	}

	character, _ := utf8.DecodeRune(s.sourceCode[s.sourceIndex:])
	return character == expected
}

// Extract the current line of source code if the scanner is positioned on a new line.
func (s *scanner) setCurrentLine() {
	s.currentLine = make([]byte, 0)
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

// Check if the scanner has reached the end of the source code.
func (s *scanner) isEndOfContent() bool {
	return s.lastCharacter == endOfFileCharacter
}

// Check if the last character is a white space.
func (s *scanner) isWhitespace() bool {
	return unicode.IsSpace(s.lastCharacter)
}

// Skip white spaces until a non-white space character is found.
func (s *scanner) whitespace() {
	for unicode.IsSpace(s.lastCharacter) {
		s.nextCharacter()
	}
}

// Check if the last character is the start of a comment.
func (s *scanner) isComment() bool {
	return s.lastCharacter == '{' || s.lastCharacter == '(' && s.peekCharacter('*')
}

// Scan a comment that starts with '{' or '(*' and ends with '}' or '*)'.
func (s *scanner) comment() error {
	if s.lastCharacter == '{' {
		s.nextCharacter()

		for !s.isEndOfContent() && s.lastCharacter != '}' {
			s.nextCharacter()
		}

		if s.isEndOfContent() {
			return cor.NewLineColumnError(cor.Scanner, failureMap, cor.Error, eofComment, nil, s.line, s.column)
		}
	} else {
		s.nextCharacter()
		s.nextCharacter()

		for !s.isEndOfContent() && !(s.lastCharacter == '*' && s.peekCharacter(')')) {
			s.nextCharacter()
		}

		if s.isEndOfContent() {
			return cor.NewLineColumnError(cor.Scanner, failureMap, cor.Error, eofComment, nil, s.line, s.column)
		}

		s.nextCharacter()
	}

	s.nextCharacter()
	return nil
}

// Check if the last character is the start of an identifier or a reserved word.
func (s *scanner) isIdentifierOrWord() bool {
	return unicode.IsLetter(s.lastCharacter)
}

// Scan consecutive letters and digits to form an identifier token or a reserved word token.
func (s *scanner) identifierOrWord() cor.Token {
	var builder strings.Builder

	for unicode.IsLetter(s.lastCharacter) || unicode.IsDigit(s.lastCharacter) {
		builder.WriteRune(s.lastCharacter)
		s.nextCharacter()
	}

	if token, ok := words[builder.String()]; ok {
		return token
	}

	s.lastValue = builder.String()
	return pl0.Identifier
}

// Check if the last character is the start of a number.
func (s *scanner) isNumber() bool {
	return unicode.IsDigit(s.lastCharacter)
}

// Scan consecutive digits to form an unsigned number token.
func (s *scanner) number() cor.Token {
	var builder strings.Builder

	for unicode.IsDigit(s.lastCharacter) {
		builder.WriteRune(s.lastCharacter)
		s.nextCharacter()
	}

	s.lastValue = builder.String()
	return pl0.Number
}

// Scan operator or statement token and return an Unknown token if last character cannot be mapped to a token.
func (s *scanner) operatorOrStatement() cor.Token {
	if token, ok := operators[string(s.lastCharacter)]; ok {
		s.nextCharacter()

		switch {
		case token == pl0.Less && s.lastCharacter == '=':
			s.nextCharacter()
			token = pl0.LessEqual

		case token == pl0.Greater && s.lastCharacter == '=':
			s.nextCharacter()
			token = pl0.GreaterEqual

		case token == pl0.Colon && s.lastCharacter == '=':
			s.nextCharacter()
			token = pl0.Becomes
		}

		return token
	} else if token, ok := statements[string(s.lastCharacter)]; ok {
		s.nextCharacter()
		return token
	}

	s.nextCharacter()
	return pl0.Unknown
}
