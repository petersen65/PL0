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

// Last character of the source code that is read when the end of the file is reached.
const EndOfFileCharacter = 0

// Private implementation of the multi-pass PL/0 scanner.
type scanner struct {
	sourceIndex   int    // index of the current character in the source code byte slice
	sourceCode    []byte // source code to scan
	line, column  int    // current line and column where the scanner is positioned
	lastCharacter rune   // last UTF-8 character that was read
	lastValue     any    // last identifier or number value that was read
	currentLine   []byte // current line of source code that is being scanned
}

var (
	// Map operator characters to their corresponding tokens.
	operators = map[string]Token{
		"+":  Plus,
		"-":  Minus,
		"*":  Times,
		"/":  Divide,
		"=":  Equal,
		"#":  NotEqual,
		"<":  Less,
		"<=": LessEqual,
		">":  Greater,
		">=": GreaterEqual,
		"(":  LeftParenthesis,
		")":  RightParenthesis,
		":=": Becomes,
		",":  Comma,
		":":  Colon,
		";":  Semicolon,
		".":  ProgramEnd,
	}

	// Map statement characters to their corresponding tokens.
	statements = map[string]Token{
		"?": Read,
		"!": Write,
	}

	// Map reserved words to their corresponding tokens.
	words = map[string]Token{
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
)

// Return the public interface of the private scanner implementation.
func newScanner() Scanner {
	return &scanner{}
}

// Run the multi-pass PL/0 scanner to map the source code to its corresponding token stream.
func (s *scanner) Scan(content []byte) (TokenStream, error) {
	s.reset(content)
	tokenStream, errScan := s.scan()
	preParsed, errPreParse := experimental(tokenStream)
	return preParsed, errors.Join(errScan, errPreParse)
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

// Perform a character scan that supports identifiers, unsigned non-valued numbers and operators.
func (s *scanner) scan() (TokenStream, error) {
	tokenStream := make(TokenStream, 0)

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

		if token, err := s.getToken(); err != nil {
			return tokenStream, err
		} else {
			tokenStream = append(tokenStream, TokenDescription{
				Token:       token,
				TokenName:   TokenNames[token],
				TokenValue:  s.lastValue,
				DataType:    None,
				Line:        s.line,
				Column:      s.column,
				CurrentLine: s.currentLine,
			})
		}
	}
}

// Return an identifier, reserved word, number, or operator token.
func (s *scanner) getToken() (Token, error) {
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
		s.lastCharacter = EndOfFileCharacter
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
	return s.lastCharacter == EndOfFileCharacter
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
			return newError(eofComment, nil, s.line, s.column)
		}
	} else {
		s.nextCharacter()
		s.nextCharacter()

		for !s.isEndOfContent() && !(s.lastCharacter == '*' && s.peekCharacter(')')) {
			s.nextCharacter()
		}

		if s.isEndOfContent() {
			return newError(eofComment, nil, s.line, s.column)
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
func (s *scanner) identifierOrWord() (Token, error) {
	var builder strings.Builder

	for unicode.IsLetter(s.lastCharacter) || unicode.IsDigit(s.lastCharacter) {
		builder.WriteRune(s.lastCharacter)
		s.nextCharacter()
	}

	if token, ok := words[builder.String()]; ok {
		return token, nil
	}

	if len(builder.String()) > identifierMax {
		return Identifier, newError(tooLongIdentifier, builder.String(), s.line, s.column)
	}

	s.lastValue = builder.String()
	return Identifier, nil
}

// Check if the last character is the start of a number.
func (s *scanner) isNumber() bool {
	return unicode.IsDigit(s.lastCharacter)
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

// Scan operator or statement token and return an error if last character cannot be mapped to a token and hence is unexpected.
func (s *scanner) operatorOrStatement() (Token, error) {
	if token, ok := operators[string(s.lastCharacter)]; ok {
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

		return token, nil
	} else if token, ok := statements[string(s.lastCharacter)]; ok {
		s.nextCharacter()
		return token, nil
	}

	return Unknown, newError(unexpectedCharacter, s.lastCharacter, s.line, s.column)
}
