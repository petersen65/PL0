package emitter

type (
	instruction struct {
		level     int
		operation Operation
		argument  Address
	}

	emitter struct {
		codeSegment []instruction
	}
)

/*
   lit 0,a  :  load constant a
   opr 0,a  :  execute operation a
   lod l,a  :  load varible l,a
   sto l,a  :  store varible l,a
   cal l,a  :  call procedure a at level l
   inc 0,a  :  increment t-register by a
   jmp 0,a  :  jump to a
   jpc 0,a  :  jump conditional to a
*/

var mnemonics = map[Operation]string{
	Lit: "lit",
	Opr: "opr",
	Lod: "lod",
	Sto: "sto",
	Cal: "cal",
	Inc: "inc",
	Jmp: "jmp",
	Jpc: "jpc",
}

func (e *emitter) emitInstruction(level int, operation Operation, argument Address) (Address, error) {
	if len(e.codeSegment) >= codeSegmentMaxAddress {
		return 0, e.error(reachedCodeSegmentMaxAddress, len(e.codeSegment))
	}

	e.codeSegment = append(e.codeSegment, instruction{
		level:     level,
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
