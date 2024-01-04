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

	Emitter interface {
		Emit(o Operation, level int, address uint64) error
	}
)

func NewEmitter() Emitter {
	return &emitter{
		code: make([]instruction, 0),
	}
}

func (e *emitter) Emit(o Operation, level int, address uint64) error {
	if len(e.code) >= codeMax {
		return e.error(instructionsExceeded, len(e.code))
	}

	e.code = append(e.code, instruction{
		operation: o,
		level:     level,
		address:   address,
	})

	return nil
}
