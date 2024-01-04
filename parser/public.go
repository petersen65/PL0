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
	if err := p.reset(concreteSyntax, emitter); err != nil {
		return p.errorReport, err
	}

	/*
		cc := 0;
		cx := 0;
		ll := 0;
		ch := ' ';
		kk := al;
	*/

	p.block(set(declarations, statements, scn.Period))

	if p.lastToken() != scn.Period {
		p.appendError(p.error(expectedPeriod, p.lastTokenDescription.TokenName))
	}

	if len(p.errorReport) > 0 {
		return p.errorReport, p.error(parsingErrors, len(p.errorReport))
	} else {
		return p.errorReport, nil
	}
}
