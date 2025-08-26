// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package scanner

import (
	"strings"
	"unicode"
	"unicode/utf8"

	cor "github.com/petersen65/PL0/v2/core"
)

// Last character of the source code that is read when the end of the file is reached.
const endOfFileCharacter = 0

// Implementation of the scanner that contains a tokenizer and lexical analyzer.
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
		"+":  cor.Plus,
		"-":  cor.Minus,
		"*":  cor.Times,
		"/":  cor.Divide,
		"=":  cor.Equal,
		"#":  cor.NotEqual,
		"<":  cor.Less,
		"<=": cor.LessEqual,
		">":  cor.Greater,
		">=": cor.GreaterEqual,
		"(":  cor.LeftParenthesis,
		")":  cor.RightParenthesis,
		":=": cor.Becomes,
		",":  cor.Comma,
		":":  cor.Colon,
		";":  cor.Semicolon,
		".":  cor.ProgramEnd,
	}

	// Map statement characters to their corresponding tokens.
	statements = map[string]cor.Token{
		"?": cor.Read,
		"!": cor.Write,
	}

	// Map reserved words to their corresponding tokens.
	words = map[string]cor.Token{
		"odd":       cor.OddWord,
		"begin":     cor.BeginWord,
		"end":       cor.EndWord,
		"if":        cor.IfWord,
		"then":      cor.ThenWord,
		"while":     cor.WhileWord,
		"do":        cor.DoWord,
		"call":      cor.CallWord,
		"const":     cor.ConstWord,
		"var":       cor.VarWord,
		"procedure": cor.ProcedureWord,
	}
)

// Return the interface of the scanner implementation.
func newScanner() Scanner {
	return &scanner{}
}

// Run the scanner to map the source code to its corresponding token stream.
func (s *scanner) Scan(content []byte) (cor.TokenStream, error) {
	s.sourceIndex = 0
	s.sourceCode = cor.CreateSourceCode(content)
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

		token, characters := s.getToken()

		tokenStream = append(tokenStream, cor.TokenDescription{
			Token:       token,
			TokenName:   token.String(),
			TokenValue:  s.lastValue,
			Line:        s.line,
			Column:      s.column - characters,
			CurrentLine: s.currentLine,
		})
	}
}

// Return identifier, reserved word, number, operator token, and the number of consumed UTF-8 characters.
func (s *scanner) getToken() (cor.Token, int) {
	lastColumn := s.column
	s.lastValue = ""

	switch {
	case s.isIdentifierOrWord():
		return s.identifierOrWord(), s.column - lastColumn

	case s.isNumber():
		return s.number(), s.column - lastColumn

	default:
		return s.operatorOrStatement(), s.column - lastColumn
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
	return cor.Identifier
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
	return cor.Number
}

// Scan operator or statement token and return an Unknown token if last character cannot be mapped to a token.
func (s *scanner) operatorOrStatement() cor.Token {
	if token, ok := operators[string(s.lastCharacter)]; ok {
		s.nextCharacter()

		switch {
		case token == cor.Less && s.lastCharacter == '=':
			s.nextCharacter()
			token = cor.LessEqual

		case token == cor.Greater && s.lastCharacter == '=':
			s.nextCharacter()
			token = cor.GreaterEqual

		case token == cor.Colon && s.lastCharacter == '=':
			s.nextCharacter()
			token = cor.Becomes
		}

		return token
	} else if token, ok := statements[string(s.lastCharacter)]; ok {
		s.nextCharacter()
		return token
	}

	s.nextCharacter()
	return cor.Unknown
}
