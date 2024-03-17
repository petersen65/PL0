// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package scanner implements the PL/0 scanner that performs a lexical analysis of the source code.
package scanner

import (
	"fmt"
	"io"
	"strings"

	tok "github.com/petersen65/PL0/token"
)

// Tokens that are supported by the PL/0 scanner.
const (
	Unknown = tok.Token(iota)
	Identifier
	Number
	Plus
	Minus
	Times
	Divide
	Equal
	NotEqual
	Less
	LessEqual
	Greater
	GreaterEqual
	LeftParenthesis
	RightParenthesis
	Comma
	Colon
	Semicolon
	ProgramEnd
	Becomes
	Read
	Write
	OddWord
	BeginWord
	EndWord
	IfWord
	ThenWord
	WhileWord
	DoWord
	CallWord
	ConstWord
	VarWord
	ProcedureWord
)

type (
	// Scanner is the public interface of the scanner implementation.
	Scanner interface {
		Scan(content []byte) (tok.TokenStream, error)
	}
)

var (
	// Empty is an empty token set.
	Empty = tok.Tokens{}

	// TokenNames maps tokens to their string representation.
	TokenNames = map[tok.Token]string{
		Unknown:          "unknown",
		Identifier:       "identifier",
		Number:           "number",
		Plus:             "plus",
		Minus:            "minus",
		Times:            "times",
		Divide:           "divide",
		Equal:            "equal",
		NotEqual:         "notEqual",
		Less:             "less",
		LessEqual:        "lessEqual",
		Greater:          "greater",
		GreaterEqual:     "greaterEqual",
		LeftParenthesis:  "leftParenthesis",
		RightParenthesis: "rightParenthesis",
		Comma:            "comma",
		Colon:            "colon",
		Semicolon:        "semicolon",
		ProgramEnd:       "programEnd",
		Becomes:          "becomes",
		Read:             "read",
		Write:            "write",
		OddWord:          "odd",
		BeginWord:        "begin",
		EndWord:          "end",
		IfWord:           "if",
		ThenWord:         "then",
		WhileWord:        "while",
		DoWord:           "do",
		CallWord:         "call",
		ConstWord:        "const",
		VarWord:          "var",
		ProcedureWord:    "procedure",
	}
)

// Return the public interface of the private scanner implementation.
func NewScanner() Scanner {
	return newScanner()
}

// Print the token stream of the scanner to the specified writer.
func PrintTokenStream(tokenStream tok.TokenStream, print io.Writer, bottom bool) {
	var start, previousLine int
	print.Write([]byte("Token Stream:"))

	if len(tokenStream) == 0 {
		print.Write([]byte("\n"))
		return
	}

	if bottom {
		lastLine := tokenStream[len(tokenStream)-1].Line

		for start = len(tokenStream) - 1; start >= 0 && tokenStream[start].Line == lastLine; start-- {
		}

		start++
	} else {
		start = 0
	}

	for i := start; i < len(tokenStream); i++ {
		td := tokenStream[i]

		if td.Line != previousLine {
			print.Write([]byte(fmt.Sprintf("\n%v: %v\n", td.Line, strings.TrimLeft(string(td.CurrentLine), " \t\n\r"))))
			previousLine = td.Line
		}

		print.Write([]byte(fmt.Sprintf("%v,%-5v %v %v\n", td.Line, td.Column, td.TokenName, td.TokenValue)))
	}
}
