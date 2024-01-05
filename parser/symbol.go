package parser

import "fmt"

const (
	constant = entry(iota)
	variable
	procedure
)

type (
	entry int

	symbol struct {
		name   string
		kind   entry
		level  int
		value  any
		label  string
		offset uint64
	}

	symbolTable struct {
		symbols []symbol
	}
)

func (s *symbolTable) addConstant(name string, level int, value any) {
	s.symbols = append(s.symbols, symbol{
		name:   name,
		kind:   constant,
		level:  level,
		value:  value,
	})
}

func (s *symbolTable) addVariable(name string, level int, offset *uint64) {
	s.symbols = append(s.symbols, symbol{
		name:   name,
		kind:   variable,
		level:  level,
		offset: *offset,
	})

	*offset++
}

func (s *symbolTable) addProcedure(name string, level int) {
	s.symbols = append(s.symbols, symbol{
		name:  name,
		kind:  procedure,
		level: level,
		label: fmt.Sprintf("_%v_%v", level, name),
	})
}

func (s *symbolTable) removeLevel(level int) {
	filteredTable := make([]symbol, 0)

	for _, s := range s.symbols {
		if s.level != level {
			filteredTable = append(filteredTable, s)
		}
	}

	s.symbols = filteredTable
}

func (s *symbolTable) findName(name string) (symbol, bool) {
	for i := len(s.symbols) - 1; i >= 0; i-- {
		if s.symbols[i].name == name {
			return s.symbols[i], true
		}
	}

	return symbol{}, false
}
