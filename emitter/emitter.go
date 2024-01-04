package emitter

type (
	failure int

	instruction struct {
		operation Operation
		level     int
		address   uint64
	}

	emitter struct {
		code []instruction
	}
)
