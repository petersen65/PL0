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
	Address   uint64

	Emitter interface {
		EmitInstruction(level int, operation Operation, argument Address) (Address, error)
		UpdateInstructionArgument(instructionAddress, argument Address) error
		GetNextInstructionAddress() Address
	}
)

var (
	NullAddress    Address = 0
	ReturnOperator Address = 0
)

func NewEmitter() Emitter {
	return &emitter{
		codeSegment: make([]instruction, 0),
	}
}

func (e *emitter) EmitInstruction(level int, operation Operation, argument Address) (Address, error) {
	return e.emitInstruction(level, operation, argument)
}

func (e *emitter) UpdateInstructionArgument(instructionAddress, argument Address) error {
	return e.updateInstructionArgument(instructionAddress, argument)
}

func (e *emitter) GetNextInstructionAddress() Address {
	return Address(len(e.codeSegment))
}
