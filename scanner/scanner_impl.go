// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package scanner

import (
	"strings"
	"unicode"
	"unicode/utf8"

	eh "github.com/petersen65/pl0/v3/errors"
	tok "github.com/petersen65/pl0/v3/token"
)

// Number of Unicode code points per tabulator UTF-8 character.
const tabulatorSize = 4

// Maximum number of allowed UTF-8 decoding errors before source content decoding is aborted.
const maxDecodingErrors = 5

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
	operators = map[string]tok.Token{
		"+":  tok.Plus,
		"-":  tok.Minus,
		"*":  tok.Times,
		"/":  tok.Divide,
		"=":  tok.Equal,
		"#":  tok.NotEqual,
		"<":  tok.Less,
		"<=": tok.LessEqual,
		">":  tok.Greater,
		">=": tok.GreaterEqual,
		"(":  tok.LeftParenthesis,
		")":  tok.RightParenthesis,
		":=": tok.Becomes,
		",":  tok.Comma,
		":":  tok.Colon,
		";":  tok.Semicolon,
		".":  tok.ProgramEnd,
	}

	// Map statement characters to their corresponding tokens.
	statements = map[string]tok.Token{
		"?": tok.Read,
		"!": tok.Write,
	}

	// Map reserved words to their corresponding tokens.
	words = map[string]tok.Token{
		"odd":       tok.OddWord,
		"begin":     tok.BeginWord,
		"end":       tok.EndWord,
		"if":        tok.IfWord,
		"then":      tok.ThenWord,
		"while":     tok.WhileWord,
		"do":        tok.DoWord,
		"call":      tok.CallWord,
		"const":     tok.ConstWord,
		"var":       tok.VarWord,
		"procedure": tok.ProcedureWord,
	}
)

// Return the interface of the scanner implementation.
func newScanner() Scanner {
	return &scanner{}
}

// Run the scanner to map the source code to its corresponding token stream.
func (s *scanner) Scan(content []byte) (tok.TokenStream, error) {
	s.sourceIndex = 0
	s.sourceCode = createSourceCode(content)
	s.line = 0
	s.column = 0
	s.lastValue = ""
	s.currentLine = make([]byte, 0)

	s.nextCharacter()
	return s.scan()
}

// Perform a character scan that supports whitespace, comments, identifiers, reserved words, numbers and operators.
func (s *scanner) scan() (tok.TokenStream, error) {
	tokenStream := make(tok.TokenStream, 0)

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

		tokenStream = append(tokenStream, s.getToken())
	}
}

// Return the token description that includes the token type, token name, token value, line number, column number, and current line content.
func (s *scanner) getToken() tok.TokenDescription {
	// capture the current line, column, and current line content to get the starting position of the token
	tokenLine := s.line
	tokenColumn := s.column
	tokenCurrentLine := make([]byte, len(s.currentLine))
	copy(tokenCurrentLine, s.currentLine)
	token := tok.Unknown

	// reset the last value for identifiers and numbers
	s.lastValue = ""

	// retrieve the next token and forward the scanner to the next character after the token
	switch {
	case s.isIdentifierOrWord():
		token = s.identifierOrWord()

	case s.isNumber():
		token = s.number()

	default:
		token = s.operatorOrStatement()
	}

	// return the token description with the token and its starting position (line, column, current line content)
	return tok.TokenDescription{
		Token:       token,
		TokenName:   token.String(),
		TokenValue:  s.lastValue,
		Line:        tokenLine,
		Column:      tokenColumn,
		CurrentLine: tokenCurrentLine,
	}
}

// Read the next UTF-8 character from the source code and update line counter, column counter, and current line content.
func (s *scanner) nextCharacter() {
	if s.sourceIndex >= len(s.sourceCode) {
		s.lastCharacter = endOfFileCharacter
		return
	}

	character, width := utf8.DecodeRune(s.sourceCode[s.sourceIndex:])
	s.lastCharacter = character

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

		s.currentLine = utf8.AppendRune(s.currentLine, character)
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
			return eh.NewLineColumnError(eh.Scanner, failureMap, eh.Error, eofComment, nil, s.line, s.column)
		}
	} else {
		s.nextCharacter()
		s.nextCharacter()

		for !s.isEndOfContent() && !(s.lastCharacter == '*' && s.peekCharacter(')')) {
			s.nextCharacter()
		}

		if s.isEndOfContent() {
			return eh.NewLineColumnError(eh.Scanner, failureMap, eh.Error, eofComment, nil, s.line, s.column)
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
func (s *scanner) identifierOrWord() tok.Token {
	var builder strings.Builder

	for unicode.IsLetter(s.lastCharacter) || unicode.IsDigit(s.lastCharacter) {
		builder.WriteRune(s.lastCharacter)
		s.nextCharacter()
	}

	if token, ok := words[builder.String()]; ok {
		return token
	}

	s.lastValue = builder.String()
	return tok.Identifier
}

// Check if the last character is the start of a number.
func (s *scanner) isNumber() bool {
	return unicode.IsDigit(s.lastCharacter)
}

// Scan consecutive digits to form an unsigned number token.
func (s *scanner) number() tok.Token {
	var builder strings.Builder

	for unicode.IsDigit(s.lastCharacter) {
		builder.WriteRune(s.lastCharacter)
		s.nextCharacter()
	}

	s.lastValue = builder.String()
	return tok.Number
}

// Scan operator or statement token and return an Unknown token if last character cannot be mapped to a token.
func (s *scanner) operatorOrStatement() tok.Token {
	if token, ok := operators[string(s.lastCharacter)]; ok {
		s.nextCharacter()

		switch {
		case token == tok.Less && s.lastCharacter == '=':
			s.nextCharacter()
			token = tok.LessEqual

		case token == tok.Greater && s.lastCharacter == '=':
			s.nextCharacter()
			token = tok.GreaterEqual

		case token == tok.Colon && s.lastCharacter == '=':
			s.nextCharacter()
			token = tok.Becomes
		}

		return token
	} else if token, ok := statements[string(s.lastCharacter)]; ok {
		s.nextCharacter()
		return token
	}

	s.nextCharacter()
	return tok.Unknown
}

// Filter binary source content from all UTF-8 errors, replace all tabulators, and return the binary content as valid source code.
func createSourceCode(content []byte) []byte {
	var column, decodingErrors int
	sourceCode := make([]byte, 0, len(content))

	// iterate over the binary content and decode each UTF-8 character
	for i := 0; i < len(content); {
		// decode the next UTF-8 character from the source content
		codepoint, width := utf8.DecodeRune(content[i:])

		// check for decoding errors, replace tabulators, and only append valid Unicode code points to the source code
		switch codepoint {
		case utf8.RuneError:
			// replace decoding errors with a space UTF-8 character
			sourceCode = append(sourceCode, ' ')
			decodingErrors++
			column++

		case '\t':
			// calculate spaces needed to reach the next tabulator stop
			spacesToAdd := tabulatorSize - (column % tabulatorSize)

			// if the column is already at a tabulator stop, add a full tabulator size of spaces
			if spacesToAdd == 0 {
				spacesToAdd = tabulatorSize
			}

			// add the calculated number of spaces
			for j := 0; j < spacesToAdd; j++ {
				sourceCode = append(sourceCode, ' ')
			}

			column += spacesToAdd

		case '\r':
			// ignore carriage return characters

		case '\n':
			// append the newline UTF-8 character and reset the column counter
			sourceCode = append(sourceCode, content[i:i+width]...)
			column = 0

		default:
			// append the original UTF-8 bytes for the decoded rune
			sourceCode = append(sourceCode, content[i:i+width]...)
			column++
		}

		// increment the index by the byte size of the decoded rune
		i += width

		// abort decoding with empty source code if too many errors occurred
		if decodingErrors > maxDecodingErrors {
			return make([]byte, 0)
		}
	}

	return sourceCode
}
