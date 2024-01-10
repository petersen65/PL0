// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package parser

import (
	"strconv"

	scn "github.com/petersen65/PL0/scanner"
)

const integerBitSize = 64 // number of bits of a signed integer

func (p *parser) nextTokenDescription() bool {
	if p.concreteSyntaxIndex >= len(p.concreteSyntax) {
		if p.eof.Token == scn.Null {
			p.eof = scn.TokenDescription{
				Token:       scn.Eof,
				TokenName:   "eof",
				TokenValue:  "",
				Line:        p.lastTokenDescription.Line,
				Column:      p.lastTokenDescription.Column,
				CurrentLine: p.lastTokenDescription.CurrentLine,
			}

			p.lastTokenDescription = p.eof
		}

		return false
	}

	p.lastTokenDescription = p.concreteSyntax[p.concreteSyntaxIndex]
	p.concreteSyntaxIndex++
	return true
}

func (p *parser) lastToken() scn.Token {
	return p.lastTokenDescription.Token
}

func (p *parser) lastTokenName() string {
	return p.lastTokenDescription.TokenName
}

func (p *parser) lastTokenValue() string {
	return p.lastTokenDescription.TokenValue
}

func (p *parser) lastTokenNumber() int64 {
	intValue, _ := strconv.ParseInt(p.lastTokenDescription.TokenValue, 10, integerBitSize)
	return intValue
}
