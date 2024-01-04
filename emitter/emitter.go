package emitter

type (
	instruction struct {
		operation Operation
		level     int
		address   Address
	}

	emitter struct {
		code []instruction
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
