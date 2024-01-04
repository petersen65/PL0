package parser

import scn "github.com/petersen65/PL0/scanner"

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
