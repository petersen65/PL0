package parser

import "github.com/petersen65/PL0/scanner"

type Parser interface {
	Parse(s scanner.Scanner) error
}
