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
		UpdateAddress(codeIndex CodeIndex, address Address) error
		GetCurrentAddress() Address
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

func (e *emitter) UpdateAddress(codeIndex CodeIndex, address Address) error {
	if int(codeIndex) >= len(e.code) {
		return e.error(codeIndexOutOfRange, codeIndex)
	}

	e.code[codeIndex].address = address
	return nil
}

func (e *emitter) GetCurrentAddress() Address {
	return Address(len(e.code))
}
