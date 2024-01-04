package emitter

import "fmt"

const (
	codeMax = 200
)

const (
	_ = failure(iota + 3000)
	instructionsExceeded
)

var errorMap = map[failure]string{
	instructionsExceeded: "maximum number of instructions reached: %v",
}

func (e *emitter) error(code failure, value any) error {
	var message string

	if value != nil {
		message = fmt.Sprintf(errorMap[code], value)
	} else {
		message = errorMap[code]
	}

	return fmt.Errorf("emitter error %v: %v", code, message)
}
