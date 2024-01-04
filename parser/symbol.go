package parser

type (
	entry int

	symbol struct {
		kind   entry
		level  int
		value  any
		offset uint64
	}
)

const (
	constant = entry(iota)
	variable
	procedure
)

func (p *parser) addSymbol(name string, kind entry, level int, value any) {
	s := symbol{
		kind:  kind,
		level: level,
		value: value,
	}

	if kind == variable {
		s.offset = p.varOffset
		p.varOffset++
	}

	p.symbolTable[name] = s
}

func (p *parser) findSymbol(name string) (symbol, bool) {
	s, ok := p.symbolTable[name]
	return s, ok
}
