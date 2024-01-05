package emitter

import "fmt"

const (
	codeSegmentMaxAddress = 200
)

const (
	_ = failure(iota + 3000)
	reachedCodeSegmentMaxAddress
	instructionAddressOutOfRange
)

type failure int

var errorMap = map[failure]string{
	reachedCodeSegmentMaxAddress: "reached code segment maximum address: %v",
	instructionAddressOutOfRange: "provided instruction address is out of range: %v",
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
