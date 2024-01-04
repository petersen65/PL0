package emitter

const (
	Lit = Operation(iota)
	Opr
	Lod
	Sto
	Cal
	Inc
	Jmp
	Jpc
)

type (
	Operation int
	CodeIndex uint64
	Address   uint64

	Emitter interface {
		Emit(operation Operation, level int, address Address) (CodeIndex, error)
	}
)

func NewEmitter() Emitter {
	return &emitter{
		code: make([]instruction, 0),
	}
}

func (e *emitter) Emit(operation Operation, level int, address Address) (CodeIndex, error) {
	if len(e.code) >= codeMax {
		return 0, e.error(instructionsExceeded, len(e.code))
	}

	e.code = append(e.code, instruction{
		operation: operation,
		level:     level,
		address:   address,
	})

	return CodeIndex(len(e.code) - 1), nil
}
