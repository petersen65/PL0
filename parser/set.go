package parser

import scn "github.com/petersen65/PL0/scanner"

var (
	declarations = scn.Tokens{
		scn.ConstWord,
		scn.VarWord,
		scn.ProcedureWord,
	}

	statements = scn.Tokens{
		scn.BeginWord,
		scn.CallWord,
		scn.IfWord,
		scn.WhileWord,
	}

	factors = scn.Tokens{
		scn.Identifier,
		scn.Number,
		scn.LeftParenthesis,
	}
)

func set(tss ...scn.TokenSet) scn.Tokens {
	return scn.Set(tss...)
}

func (p *parser) rebase(code failure, expected, expanded scn.Tokens) {
	if !p.lastToken().In(expected) {
		p.appendError(p.error(code, p.lastToken()))

		for next := set(expected, expanded, scn.Eof); !p.lastToken().In(next); {
			p.nextTokenDescription()
		}
	}
}
