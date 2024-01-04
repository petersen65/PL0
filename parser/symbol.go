package parser

import "fmt"

type (
	entry int
	table []symbol

	symbol struct {
		name   string
		kind   entry
		level  int
		value  any
		label  string
		offset uint64
	}
)

const (
	constant = entry(iota)
	variable
	procedure
)

func (p *parser) addConstant(name string, value any) {
	p.symbolTable = append(p.symbolTable, symbol{
		name:  name,
		kind:  constant,
		level: p.blockLevel,
		value: value,
	})
}

func (p *parser) addVariable(name string, offset *uint64) {
	p.symbolTable = append(p.symbolTable, symbol{
		name:   name,
		kind:   variable,
		level:  p.blockLevel,
		offset: *offset,
	})

	*offset++
}

func (p *parser) addProcedure(name string) {
	p.symbolTable = append(p.symbolTable, symbol{
		name:  name,
		kind:  procedure,
		level: p.blockLevel,
		label: fmt.Sprintf("_%v_%v", p.blockLevel, name),
	})
}

func (p *parser) removeLevel(level int) {
	filteredTable := make([]symbol, 0)

	for _, s := range p.symbolTable {
		if s.level != level {
			filteredTable = append(filteredTable, s)
		}
	}
	
	p.symbolTable = filteredTable
}

func (p *parser) findSymbol(name string) (symbol, bool) {
	for i := len(p.symbolTable) - 1; i >= 0; i-- {
		if p.symbolTable[i].name == name {
			return p.symbolTable[i], true
		}
	}

	return symbol{}, false
}

func (p *parser) findKind(kind entry) (symbol, bool) {
	for i := len(p.symbolTable) - 1; i >= 0; i-- {
		if p.symbolTable[i].kind == kind {
			return p.symbolTable[i], true
		}
	}

	return symbol{}, false
}
