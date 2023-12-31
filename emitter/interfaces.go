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

	Emitter interface {
		Emit(o Operation, level int, address uint64) error
	}
)
