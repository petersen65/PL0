package parser

import (
	emt "github.com/petersen65/PL0/emitter"
	scn "github.com/petersen65/PL0/scanner"
)

type (
	ErrorReport []Error

	Error struct {
		Err          error
		Line, Column int
		CurrentLine  []byte
	}

	Parser interface {
		Parse(concreteSyntax scn.ConcreteSyntax, emitter emt.Emitter) (ErrorReport, error)
	}
)

func NewParser() Parser {
	return &parser{}
}

func (p *parser) Parse(concreteSyntax scn.ConcreteSyntax, emitter emt.Emitter) (ErrorReport, error) {
	return p.parse(concreteSyntax, emitter)
}
