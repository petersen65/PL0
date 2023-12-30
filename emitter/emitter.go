package emitter

type (
	instruction struct {
		operation Operation
		level     int
		address   uint64
	}

	emitter struct {
		code []instruction
	}
)

func NewEmitter() Emitter {
	return &emitter{
		code: make([]instruction, 0),
	}
}

func (e *emitter) Emit(o Operation, level int, address uint64) {
	e.code = append(e.code, instruction{
		operation: o,
		level:     level,
		address:   address,
	})
}
