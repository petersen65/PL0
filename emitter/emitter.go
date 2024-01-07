package emitter

type (
	instruction struct {
		depth     int
		operation Operation
		argument  Address
	}

	emitter struct {
		codeSegment []instruction
	}
)

func (e *emitter) emitInstruction(declarationDepth int, operation Operation, argument Address) (Address, error) {
	if len(e.codeSegment) >= codeSegmentMaxAddress {
		return 0, e.error(reachedCodeSegmentMaxAddress, len(e.codeSegment))
	}

	e.codeSegment = append(e.codeSegment, instruction{
		depth:     declarationDepth,
		operation: operation,
		argument:  argument,
	})

	return Address(len(e.codeSegment) - 1), nil
}

func (e *emitter) updateInstructionArgument(instructionAddress, argument Address) error {
	if uint64(instructionAddress) >= uint64(len(e.codeSegment)) {
		return e.error(instructionAddressOutOfRange, instructionAddress)
	}

	e.codeSegment[instructionAddress].argument = argument
	return nil
}
