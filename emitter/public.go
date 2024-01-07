package emitter

const (
	Lit = Operation(iota)
	Opr
	Lod
	Sto
	Cal
	Ret
	Inc
	Jmp
	Jpc
)

type (
	Operation int
	Address   uint64

	Emitter interface {
		EmitInstruction(declarationDepth int, operation Operation, argument Address) (Address, error)
		UpdateInstructionArgument(instructionAddress, argument Address) error
		GetNextInstructionAddress() Address
	}
)

var NullAddress Address = 0

func NewEmitter() Emitter {
	return &emitter{
		codeSegment: make([]instruction, 0),
	}
}

func (e *emitter) EmitInstruction(declarationDepth int, operation Operation, argument Address) (Address, error) {
	return e.emitInstruction(declarationDepth, operation, argument)
}

func (e *emitter) UpdateInstructionArgument(instructionAddress, argument Address) error {
	return e.updateInstructionArgument(instructionAddress, argument)
}

func (e *emitter) GetNextInstructionAddress() Address {
	return Address(len(e.codeSegment))
}
