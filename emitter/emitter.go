package emitter

import "fmt"

const (
	addressMax = 2047 // maximum address
	codeMax    = 200  // maximum number of instructions
)

const (
	_ = failure(iota + 3000)
	instructionsExceeded
)

type (
	failure int

	instruction struct {
		operation Operation
		level     int
		address   uint64
	}

	emitter struct {
		code     []instruction
		errorMap map[failure]string
	}
)

func NewEmitter() Emitter {
	return &emitter{
		code: make([]instruction, 0),
		errorMap: map[failure]string{
			instructionsExceeded: "maximum number of instructions reached: %v",
		},
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

func (e *emitter) error(code failure, value any) error {
	var message string

	if value != nil {
		message = fmt.Sprintf(e.errorMap[code], value)
	} else {
		message = e.errorMap[code]
	}

	return fmt.Errorf("emitter error %v: %v", code, message)
}
