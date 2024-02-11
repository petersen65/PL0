// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package scanner

import (
	"errors"
	"strings"
	"unicode"
	"unicode/utf8"
)

// Private implementation of the multi-pass PL/0 scanner.
type scanner struct {
	sourceIndex   int    // index of the current character in the source code byte slice
	sourceCode    []byte // source code to scan
	line, column  int    // current line and column where the scanner is positioned
	lastCharacter rune   // last UTF-8 character that was read
	lastValue     any    // last identifier or number value that was read
	currentLine   []byte // current line of source code that is being scanned
}

// UTF-8 characters that are mapped to their corresponding PL/0 token.
var tokenMap = map[string]Token{
	"+":         Plus,
	"-":         Minus,
	"*":         Times,
	"/":         Divide,
	"=":         Equal,
	"#":         NotEqual,
	"<":         Less,
	"<=":        LessEqual,
	">":         Greater,
	">=":        GreaterEqual,
	"(":         LeftParenthesis,
	")":         RightParenthesis,
	",":         Comma,
	":":         Colon,
	";":         Semicolon,
	".":         Period,
	":=":        Becomes,
	"?":         Read,
	"!":         Write,
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
}

// Return the public interface of the private scanner implementation.
func newScanner() Scanner {
	return &scanner{}
}

// Run the multi-pass PL/0 scanner to map the source code to its corresponding concrete syntax.
func (s *scanner) Scan(content []byte) (syntax ConcreteSyntax, err error) {
	defer func() {
		if p := recover(); p != nil {
			syntax = append(syntax, TokenDescription{
				Token:       Eof,
				TokenName:   TokenNames[Eof],
				TokenValue:  "",
				TokenType:   None,
				Line:        s.line,
				Column:      s.column,
				CurrentLine: s.currentLine,
			})

			err = p.(error)
		}
	}()

	s.reset(content)
	basicSyntax, errBasic := s.basicScan()
	fullSyntax, errFull := slidingScan(basicSyntax)
	syntax = fullSyntax
	err = errors.Join(errBasic, errFull)

	return
}

// Perform a basic scan that supports identifiers, unsigned non-valued numbers and operators.
func (s *scanner) basicScan() (ConcreteSyntax, error) {
	basicSyntax := make(ConcreteSyntax, 0)

	for {
		token, err := s.getToken()

		basicSyntax = append(basicSyntax, TokenDescription{
			Token:       token,
			TokenName:   TokenNames[token],
			TokenValue:  s.lastValue,
			TokenType:   None,
			Line:        s.line,
			Column:      s.column,
			CurrentLine: s.currentLine,
		})

		if err != nil {
			return basicSyntax, err
		}

		if s.isEndOfFile() {
			return basicSyntax, nil
		}
	}
}

// Reset the scanner to its initial state so that it can be reused.
func (s *scanner) reset(content []byte) {
	s.sourceIndex = 0
	s.sourceCode = content
	s.line = 0
	s.column = 0
	s.lastValue = nil
	s.currentLine = make([]byte, 0)
	s.nextCharacter()
}

// Return an identifier, number or operator token for the basic scan pass (unsigned numbers and single UTF-8 character operators).
func (s *scanner) getToken() (Token, error) {
	s.lastValue = ""

	switch {
	case s.isWhiteSpace():
		s.whitespace()
		return s.getToken()

	case s.isComment():
		s.comment()
		return s.getToken()

	case unicode.IsLetter(s.lastCharacter):
		return s.identifierOrWord()

	case unicode.IsDigit(s.lastCharacter):
		return s.number()

	default:
		return s.operator()
	}
}

// Read the next UTF-8 character from the source code and update the line and column counters.
func (s *scanner) nextCharacter() {
	if s.isEndOfFile() {
		panic(newError(eofReached, nil, s.line, s.column))
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

// Peek the next UTF-8 character from the source code to check if it matches the given character.
func (s *scanner) peekCharacter(next rune) bool {
	if s.isEndOfFile() {
		return false
	}

	character, _ := utf8.DecodeRune(s.sourceCode[s.sourceIndex:])
	return character == next
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
func (s *scanner) isEndOfFile() bool {
	return s.sourceIndex >= len(s.sourceCode)
}

// Check if the current character is a white space.
func (s *scanner) isWhiteSpace() bool {
	return unicode.IsSpace(s.lastCharacter)
}

// Skip white spaces until a non-white space character is found.
func (s *scanner) whitespace() {
	for !s.isEndOfFile() && s.isWhiteSpace() {
		s.nextCharacter()
	}
}

// Check if the current characters are the start of a comment.
func (s *scanner) isComment() bool {
	return s.lastCharacter == '{' || s.lastCharacter == '(' && s.peekCharacter('*')
}

// Scan a comment that starts with '{' or '(*' and ends with '}' or '*)'.
func (s *scanner) comment() {
	if s.lastCharacter == '{' {
		s.nextCharacter()

		for s.lastCharacter != '}' {
			s.nextCharacter()
		}
	} else {
		s.nextCharacter()
		s.nextCharacter()

		for !(s.lastCharacter == '*' && s.peekCharacter(')')) {
			s.nextCharacter()
		}

		s.nextCharacter()
	}

	if !s.isEndOfFile() {
		s.nextCharacter()
	}
}

// Scan consecutive letters and digits to form an identifier token or a reserved word token.
func (s *scanner) identifierOrWord() (Token, error) {
	var builder strings.Builder

	for unicode.IsLetter(s.lastCharacter) || unicode.IsDigit(s.lastCharacter) {
		builder.WriteRune(s.lastCharacter)
		s.nextCharacter()
	}

	if token, ok := tokenMap[builder.String()]; ok {
		return token, nil
	}

	if len(builder.String()) > identifierMax {
		return Identifier, newError(tooLongIdentifier, builder.String(), s.line, s.column)
	}

	s.lastValue = builder.String()
	return Identifier, nil
}

// Scan consecutive digits to form an unsigned number token.
func (s *scanner) number() (Token, error) {
	var builder strings.Builder

	for unicode.IsDigit(s.lastCharacter) {
		builder.WriteRune(s.lastCharacter)
		s.nextCharacter()
	}

	if len(builder.String()) > digitsMax {
		return Number, newError(tooLongNumber, builder.String(), s.line, s.column)
	}

	s.lastValue = builder.String()
	return Number, nil
}

// Scan an operator token or return an error if characters cannot be mapped to a token and hence is unexpected.
func (s *scanner) operator() (Token, error) {
	if token, ok := tokenMap[string(s.lastCharacter)]; ok {
		if !(token == Period && s.isEndOfFile()) {
			s.nextCharacter()

			switch {
			case token == Less && s.lastCharacter == '=':
				s.nextCharacter()
				token = LessEqual

			case token == Greater && s.lastCharacter == '=':
				s.nextCharacter()
				token = GreaterEqual

			case token == Colon && s.lastCharacter == '=':
				s.nextCharacter()
				token = Becomes
			}
		}

		return token, nil
	}

	return Null, newError(unexpectedCharacter, s.lastCharacter, s.line, s.column)
}
