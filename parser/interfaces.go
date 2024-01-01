package parser

import (
	"github.com/petersen65/PL0/emitter"
	"github.com/petersen65/PL0/scanner"
)

type (
	Report []Diagnostic

	Diagnostic struct {
		Err          error
		Message      string
		Line, Column int
		Source       []byte
	}
	
	Parser interface {
		Parse(s scanner.Report, e emitter.Emitter) (Report, error)
	}
)
