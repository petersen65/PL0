package main

import "fmt"

const (
	null = symbol(iota)
	identifier
	number
	plus
	minus
	times
	devide
	equal
	notEqual
	less
	lessEqual
	greater
	greaterEqual
	leftParenthesis
	rightParenthesis
	comma
	semicolon
	period
	becomes
	oddSymbol
	beginSymbol
	endSymbol
	ifSymbol
	thenSymbol
	whileSymbol
	doSymbol
	callSymbol
	constSymbol
	varSymbol
	procedureSymbol
)

const (
	lit = instruction(iota) // lit 0,a  :  load constant
	opr                     // opr 0,a  :  execute operation
	lod                     // lod l,a  :  load variable
	sto                     // sto l,a  :  store variable
	cal                     // cal l,a  :  call procedure
	inc                     // inc 0,a  :  increment t-register
	jmp                     // jmp 0,a  :  jump to a
	jpc                     // jpc 0,a  :  jump conditional to a
)

const (
	reservedWords      = 11   // Number of reserved words
	identifierTableLen = 100  // Length of identifier table
	digitsMax          = 14   // Maximum number of digits in numbers
	identifierLen      = 10   // Length of identifier
	addressMax         = 2047 // Maximum address
	blockNestingMax    = 3    // Maximum depth of block nesting
	codeArrayMax       = 200  // Size of code array
)

type symbol int
type instruction int

/*
type alfa = packed array [1..al] of char;
    object = (constant,varible,proc);
    symset = set of symbol;
    fct = (lit,opr,lod,sto,cal,int,jmp,jpc);   {functions}
    instruction = packed record
                     f: fct;           {function code}
                     l: 0..levmax;     {level}
                     a: 0..amax        {displacement address}
                  end;
				  
var ch: char;         {last character read}
    sym: symbol;      {last symbol read}
    id: alfa;         {last identifier read}
    num: integer;     {last number read}
    cc: integer;      {character count}
    ll: integer;      {line length}
    kk, err: integer;
    cx: integer;      {code allocation index}
    line: array [1..81] of char;
    a: alfa;
    code: array [0..cxmax] of instruction;
    word: array [1..norw] of alfa;
    wsym: array [1..norw] of symbol;
    ssym: array [char] of symbol;
    mnemonic: array [fct] of
                 packed array [1..5] of char;
    declbegsys, statbegsys, facbegsys: symset;
    table: array [0..txmax] of
           record name: alfa;
              case kind: object of
              constant: (val: integer);
              varible, proc: (level, adr: integer)
           end;
*/

type scanner struct{}

type parser struct{}

type emitter struct{}

func main() {
	fmt.Println("PL0 Compiler Version 0.1")
	fmt.Println("Written in Go by Michael Petersen 2024")
}
