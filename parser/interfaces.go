package parser

import (
	"github.com/petersen65/PL0/emitter"
	"github.com/petersen65/PL0/scanner"
)

type (
	ErrorReport []Error

	Error struct {
		Err          error
		Message      string
		Line, Column int
		Source       []byte
	}
	
	Parser interface {
		Parse(concreteSyntax scanner.ConcreteSyntax, emitter emitter.Emitter) (ErrorReport, error)
	}
)
