package parser

import (
	"github.com/petersen65/PL0/emitter"
	"github.com/petersen65/PL0/scanner"
)

type Parser interface {
	Parse(s scanner.Scanner, e emitter.Emitter) error
}
