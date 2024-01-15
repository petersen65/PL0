// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package scanner

import (
	"unicode/utf8"
)

const integerBitSize = 64 // number of bits of a signed integer

type scanner struct {
	sourceIndex   int
	sourceCode    []byte
	line, column  int
	lastCharacter rune
	lastValue     any
	currentLine   []byte
	endOfFile     bool
}

func (s *scanner) scan(content []byte) (ConcreteSyntax, error) {
	if err := s.reset(content); err != nil {
		return make(ConcreteSyntax, 0), err
	}

	s.basicScan()
	s.slidingScan(nil)
	return nil, nil
}

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

		if token == Eof {
			return basicSyntax, nil
		}
	}
}

func (s *scanner) slidingScan(basicSyntax ConcreteSyntax) (ConcreteSyntax, error) {
	fullSyntax := make(ConcreteSyntax, len(basicSyntax))

	for i := 0; i < len(basicSyntax); i++ {
		switch basicSyntax[i].Token {
		case Colon:
			if s.try(i+1, basicSyntax) == Equal {
				i++
				fullSyntax = append(fullSyntax, s.apply(i, Becomes, basicSyntax))
			} else {
				fullSyntax = append(fullSyntax, basicSyntax[i])
			}

		case Less:
			if s.try(i+1, basicSyntax) == Equal {
				i++
				fullSyntax = append(fullSyntax, s.apply(i, LessEqual, basicSyntax))
			} else {
				fullSyntax = append(fullSyntax, basicSyntax[i])
			}

		case Greater:
			if s.try(i+1, basicSyntax) == Equal {
				i++
				fullSyntax = append(fullSyntax, s.apply(i, GreaterEqual, basicSyntax))
			} else {
				fullSyntax = append(fullSyntax, basicSyntax[i])
			}

		default:
			fullSyntax = append(fullSyntax, basicSyntax[i])
		}
	}

	/*
		if tokenDescription.Token == Number {
			tokenDescription.TokenType = Integer64
		}


		if int64Value, err := strconv.ParseInt(builder.String(), 10, integerBitSize); err != nil {
			return Number, s.error(illegalInteger, builder.String())
		} else {
			s.lastValue = int64Value
			return Number, nil
		}


		case s.lastCharacter == ':':
		return s.becomes()


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

		if token, ok := tokenMap[string(s.lastCharacter)]; ok {
			if token == Less || token == Greater {
				builder.WriteRune(s.lastCharacter)
			}

			if !s.nextCharacter() && token != Period {
				return token, s.error(eofReached, nil)
			}

			if (token == Less || token == Greater) && s.lastCharacter == '=' {
				builder.WriteRune(s.lastCharacter)
				token = tokenMap[builder.String()]

				if !s.nextCharacter() {
					return token, s.error(eofOperator, builder.String())
				}
			}

			return token, nil
		}

		return Null, s.error(unexpectedCharacter, s.lastCharacter)
		}
	*/

	return fullSyntax, nil
}

func (s *scanner) try(i int, basicSyntax ConcreteSyntax) Token {
	if i > 0 && i < len(basicSyntax) {
		return basicSyntax[i].Token
	}

	return Null
}

func (s *scanner) apply(i int, token Token, basicSyntax ConcreteSyntax) TokenDescription {
	return TokenDescription{
		Token:       token,
		TokenName:   TokenNames[token],
		TokenValue:  nil,
		TokenType:   None,
		Line:        basicSyntax[i].Line,
		Column:      basicSyntax[i].Column,
		CurrentLine: basicSyntax[i].CurrentLine,
	}
}

func (s *scanner) reset(content []byte) error {
	s.sourceIndex = 0
	s.sourceCode = content
	s.line = 0
	s.column = 0
	s.lastValue = nil
	s.currentLine = []byte{}
	s.endOfFile = false

	if len(content) == 0 || !s.nextCharacter() {
		return s.error(eofReached, nil)
	}

	return nil
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
