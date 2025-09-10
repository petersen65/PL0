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
	errorHandler  eh.ErrorHandler // error handler that is used to handle errors that occurred during scanning
	sourceIndex   int             // index of the current character in the source code byte slice
	sourceCode    []byte          // source code to scan
	line, column  int             // current line and column where the scanner is positioned
	lastCharacter rune            // last UTF-8 character that was read
	lastValue     string          // last identifier or number value that was read
	currentLine   []byte          // current line of source code that is being scanned
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
func newScanner(errorHandler eh.ErrorHandler) Scanner {
	return &scanner{errorHandler: errorHandler}
}

// Run the scanner to map the source code to its corresponding token stream.
func (s *scanner) Scan(content []byte) tok.TokenStream {
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
func (s *scanner) scan() tok.TokenStream {
	tokenStream := make(tok.TokenStream, 0)

	for {
		for s.isWhitespace() || s.isComment() {
			if s.isWhitespace() {
				s.whitespace()
			}

			if s.isComment() {
				if errorToken := s.comment(); errorToken == tok.Error {
					return tokenStream
				}
			}
		}

		if s.isEndOfContent() {
			return tokenStream
		}

		tokenStream = append(tokenStream, s.getToken())
	}
}

// Return the token description that includes the token type, token name, token value, line number, column number, and current line content.
// Each time this function is called, the scanner is forwarded to the starting position of the next token.
func (s *scanner) getToken() tok.TokenDescription {
	// capture the current line, column, and current line content to get the starting position of the token
	line, column, currentLine := s.line, s.column, make([]byte, len(s.currentLine))
	copy(currentLine, s.currentLine)

	// fall back to Unknown token if no valid token could be scanned
	token := tok.Unknown

	// reset the last value for identifiers and numbers
	s.lastValue = ""

	// retrieve the next token and forward the scanner to the next character after the token
	switch {
	case s.isIdentifierOrWord():
		token = s.identifierOrWord()

	case s.isNumberLiteral():
		token = s.numberLiteral()

	case s.isStringLiteral():
		token = s.stringLiteral()

	case s.isCharacterLiteral():
		token = s.characterLiteral()

	default:
		token = s.operatorOrStatement()
	}

	// return the token description with the token and its starting position (line, column, current line content)
	return tok.TokenDescription{
		Token:       token,
		TokenName:   token.String(),
		TokenValue:  s.lastValue,
		Line:        line,
		Column:      column,
		CurrentLine: currentLine,
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

// Peek the next UTF-8 character from the source code to check if it is a digit.
func (s *scanner) peekDigit() bool {
	if s.isEndOfContent() {
		return false
	}

	character, _ := utf8.DecodeRune(s.sourceCode[s.sourceIndex:])
	return unicode.IsDigit(character)
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
func (s *scanner) comment() tok.Token {
	line, column := s.line, s.column

	if s.lastCharacter == '{' {
		s.nextCharacter()

		for !s.isEndOfContent() && s.lastCharacter != '}' {
			s.nextCharacter()
		}

		if s.isEndOfContent() {
			return s.appendError(eofComment, line, column)
		}
	} else {
		s.nextCharacter()
		s.nextCharacter()

		for !s.isEndOfContent() && !(s.lastCharacter == '*' && s.peekCharacter(')')) {
			s.nextCharacter()
		}

		if s.isEndOfContent() {
			return s.appendError(eofComment, line, column)
		}

		s.nextCharacter()
	}

	s.nextCharacter()
	return tok.Unknown
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

// Check if the last character is the beginning of a number literal.
func (s *scanner) isNumberLiteral() bool {
	return unicode.IsDigit(s.lastCharacter)
}

// Scan consecutive digits to form an integer or floating point numberLiteral literal.
func (s *scanner) numberLiteral() tok.Token {
	var isFloatingPoint bool
	var builder strings.Builder
	line, column := s.line, s.column

	// scan the integer part
	for unicode.IsDigit(s.lastCharacter) {
		builder.WriteRune(s.lastCharacter)
		s.nextCharacter()
	}

	// check for decimal point of floating point numbers
	if s.lastCharacter == '.' {
		// peek ahead to see if there's a digit after the decimal point
		if s.peekDigit() {
			isFloatingPoint = true
			builder.WriteRune(s.lastCharacter)
			s.nextCharacter()

			// scan the fractional part of the floating point number
			for unicode.IsDigit(s.lastCharacter) {
				builder.WriteRune(s.lastCharacter)
				s.nextCharacter()
			}
		}
	}

	// check for scientific notation of floating point numbers (exponent part 'e' or 'E')
	if s.lastCharacter == 'e' || s.lastCharacter == 'E' {
		// peek ahead to ensure valid scientific notation (exponent part 'e' or 'E' followed by optional '+/-' sign and digits)
		if s.peekValidScientificNotation() {
			isFloatingPoint = true
			builder.WriteRune(s.lastCharacter)
			s.nextCharacter()

			// optional '+/-' sign after exponent part 'e' or 'E'
			if s.lastCharacter == '+' || s.lastCharacter == '-' {
				builder.WriteRune(s.lastCharacter)
				s.nextCharacter()
			}

			// exponent digits are required for valid scientific notation
			for unicode.IsDigit(s.lastCharacter) {
				builder.WriteRune(s.lastCharacter)
				s.nextCharacter()
			}
		} else {
			// consume exponent part 'e' or 'E'
			builder.WriteRune(s.lastCharacter)
			s.nextCharacter()

			// consume optional '+/-' sign after exponent part 'e' or 'E'
			if s.lastCharacter == '+' || s.lastCharacter == '-' {
				builder.WriteRune(s.lastCharacter)
				s.nextCharacter()
			}

			// invalid scientific notation for floating point number
			return s.appendError(invalidScientificNotation, line, column, builder.String())
		}
	}

	// check for floating point suffixes (f, F, d, D)
	if s.lastCharacter == 'f' || s.lastCharacter == 'F' ||
		s.lastCharacter == 'd' || s.lastCharacter == 'D' {

		builder.WriteRune(s.lastCharacter)
		s.nextCharacter()

		// floating point suffix without decimal point or scientific notation
		if !isFloatingPoint {
			return s.appendError(floatingPointSuffixWithoutDecimalPoint, line, column, builder.String())
		}

		isFloatingPoint = true
	}

	s.lastValue = builder.String()

	// check for malformed patterns in floating point numbers
	if isFloatingPoint && isMalformedFloatingPointNumber(s.lastValue) {
		return s.appendError(malformedFloatingPointNumber, line, column, s.lastValue)
	}

	if isFloatingPoint {
		return tok.FloatingPoint
	}

	return tok.Integer
}

// Check if the last character is the beginning of a string literal.
func (s *scanner) isStringLiteral() bool {
	return s.lastCharacter == '"'
}

// Scan a string literal that begins and ends with a double quote (").
func (s *scanner) stringLiteral() tok.Token {
	var builder strings.Builder
	line, column := s.line, s.column

	s.nextCharacter()

	for !s.isEndOfContent() && s.lastCharacter != '"' {
		builder.WriteRune(s.lastCharacter)
		s.nextCharacter()
	}

	if s.isEndOfContent() {
		return s.appendError(eofString, line, column)
	}

	s.nextCharacter()
	s.lastValue = builder.String()
	return tok.String
}

// Check if the last character is the beginning of a character literal.
func (s *scanner) isCharacterLiteral() bool {
	return s.lastCharacter == '\''
}

// Scan a character literal that begins and ends with a single quote (').
func (s *scanner) characterLiteral() tok.Token {
	var builder strings.Builder
	line, column := s.line, s.column

	s.nextCharacter()

	for !s.isEndOfContent() && s.lastCharacter != '\'' {
		builder.WriteRune(s.lastCharacter)
		s.nextCharacter()
	}

	if s.isEndOfContent() {
		return s.appendError(eofCharacter, line, column)
	} else if builder.Len() == 0 {
		return s.appendError(characterLiteralEmpty, line, column)
	} else if builder.Len() > 1 {
		return s.appendError(characterLiteralContainsMultipleCharacters, line, column)
	}

	s.nextCharacter()
	s.lastValue = builder.String()
	return tok.Character
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

// Append an error from the scanner to the error handler's error list and return the error token.
func (s *scanner) appendError(code eh.Failure, line, column int, values ...any) tok.Token {
	s.errorHandler.AppendError(eh.NewLineColumnError(eh.Scanner, failureMap, eh.Error, code, line, column, values...))
	return tok.Error
}

// Peek ahead to check if a floating point scientific notation is valid (e/E followed by optional +/- and digits).
func (s *scanner) peekValidScientificNotation() bool {
	if s.isEndOfContent() {
		return false
	}

	peekIndex := s.sourceIndex

	// check for optional sign after exponent part 'e' or 'E'
	if character, width := utf8.DecodeRune(s.sourceCode[peekIndex:]); character == '+' || character == '-' {
		peekIndex += width
	}

	// must have at least one digit exponent part 'e' or 'E' (and optional sign)
	if peekIndex < len(s.sourceCode) {
		character, _ := utf8.DecodeRune(s.sourceCode[peekIndex:])
		return unicode.IsDigit(character)
	}

	return false
}

// Check if a floating point number is malformed (e.g., multiple decimal points, multiple 'e' or 'E', empty exponent after sign).
func isMalformedFloatingPointNumber(number string) bool {
	var decimalPointCount, exponentCount int

	// check for patterns like:
	//   - multiple decimal points: "3.14.15"
	//   - multiple 'e' or 'E': "1e2e3"
	//   - empty exponent after sign: "1e+"
	for i, char := range number {
		switch char {
		case '.':
			decimalPointCount++

			if decimalPointCount > 1 {
				return true
			}

		case 'e', 'E':
			exponentCount++

			if exponentCount > 1 {
				return true
			}

			// check if 'e'/'E' is at the end or followed only by sign
			if i == len(number)-1 {
				return true
			}

			if i == len(number)-2 && (number[i+1] == '+' || number[i+1] == '-') {
				return true
			}
		}
	}

	return false
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
