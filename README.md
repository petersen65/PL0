# PL/0 Compiler

This module provides a complete compiler for the programming language PL/0. It provides several packages that can be used independently of its command line interface.

* core: core features, token type-system, and error-handling mechanism used by all compiler components
* scanner: lexical analysis of PL/0 source code by converting input characters in UTF-8 encoding to a token stream table with token descriptions
* parser: syntax analysis of PL/0 token stream ensuring it adheres to the rules defined in the extended Backus-Naur form for the PL/0 language
* ast: abstract syntax composition for PL/0 token stream by creating a normalized representation of source code as in-memory abstract syntax tree (AST)
* analyzer: semantic analysis on the in-memory abstract syntax tree to validate identifier declarations and their usage
* generator: compiler phase for intermediate code generation by traversing the abstract syntax tree
* cfg: control flow graph (CFG) representation of intermediate code to enable advanced compiler optimization techniques
* emitter: assembly code generation compiler phase by iterating over the intermediate code
* compiler: driver for all compiler components, from scanning PL/0 source code to executing and printing resultant code

The reason for creating the compiler is that I have been interested in compiler construction since my computer science studies. Since I was already working with Niklaus Wirth's programming languages at the end of the 1990s, it made sense to build on what I had learned back then and start a compiler project with a modern programming language. I decided on the Go programming language because it is lean and available on all common operating systems. The compiler translates the programming language PL/0 from 1986 into Intel x86_64 assembly language. Why PL/0? I start with PL/0 because this language is very simple and reduced, so that its compiler can be written and understood by one person.

From now on, the PL/0 compiler is a personal hobby of mine, which I will continue to work on after its initial creation. My activities can be found below in the change log and in the planning sections. Interested students and developers are welcome to learn from my project how a compiler works and looks from the inside. I document the source code and structure the project for better traceability. Variable names are also slightly longer than usual so that the source code can be understood. The source code is also prepared for extensibility and encapsulation by using packages, public, and private implementation-patterns.

For Visual Studio Code, you should be able to run the compiler with F5. Please install the lastest Go version before that. You will be guided to a playground.pl0 file where you can try out PL/0 programming. Support for dev-containers is also provided, if you cannot install Go on your operating system. The PL/0 compiler was tested under Windows 11, Linux Ubuntu 24.10, and macOS Sequoia M4.

I test and check the source code for errors. You are welcome to tell me about errors and make suggestions. However, I can only do this in my private time.

## License

The complete compiler source code is licensed under Apache License v2.0.

## PL/0

PL/0 in its original version from 1976 is an educational programming language, that is similar to but much simpler than Pascal, a well-known general-purpose programming language. It serves as an example of how to construct a compiler. It was originally introduced in the book, Algorithms + Data Structures = Programs, by Niklaus Wirth in 1976. It featured quite limited language constructs: there are no real numbers, very few basic arithmetic operations and no control-flow constructs other than "if" and "while" blocks. While these limitations made writing real applications in this language impractical, it helped the compiler remain compact and simple.

Beginning with version 1.1.0 of my compiler, I will start to extend the 1986 version of PL/0 and hence name it PL/0 2025. The programming language PL/0 2025 will be aligned to Pascal syntax and language features.

## Grammar

In computer science theory, a context free grammar is formally defined as G = (S,N,T,P):
* S is the start symbol
* N is a set of non-terminal symbols
* T is a set of terminal symbols or words
* P is a set of productions or rewrite rules (P:N → N ∪ T)

Applied to the programming language PL/0, the grammar G is:
* S = program
* N = program, block, statement, condition, expression, term, factor
* T = identifier, number, "const", "var", "procedure", "call", "begin", "end", "if", "then", "while", "do", "odd"
* T = ".", ",", ";", ":=", "?", "!", "=", "#", "<", "<=", ">", ">=", "+", "-", "*", "/"
* P = the set of productions is described in the table below

The programming language PL/0 has the following productions (extended Backus-Naur form):

| Nonterminal symbol | Terminal or nonterminal symbols
|:-------------------|:-----------------------------------------------------------------------------------------
| program            | block "."
|                    | &nbsp;
| block              | ["const" identifier "=" ["+" \| "-"] number {"," identifier "=" ["+" \| "-"] number} ";"]
|                    | ["var" identifier {"," identifier} ";"]
|                    | {"procedure" identifier ";" block ";"}
|                    | statement
|                    | &nbsp;
| statement          | [identifier ":=" expression
|				 	 | &nbsp;\| "call" identifier
| 					 | &nbsp;\| "!" expression
|                    | &nbsp;\| "?" identifier 
|                    | &nbsp;\| "begin" statement {";" statement} "end"
|                    | &nbsp;\| "if" condition "then" statement
|                    | &nbsp;\| "while" condition "do" statement]
|                    | &nbsp;
| condition          | "odd" expression
|                    | &nbsp;&nbsp;\| expression ("=" \| "#" \| "<" \| "<=" \| ">" \| ">=") expression
|                    | &nbsp;
| expression         | term {( "+" \| "-") term}
| term               | factor {("*" \| "/") factor}
| factor             | ["+" \| "-"] identifier \| number \| "(" expression ")"
|                    | 

For PL/0, tokens define the set of accepting states of a finite automaton. There are four classes: 
* identifiers = identifier
* keywords = "const", "var", "procedure", "call", "begin", "end", "if", "then", "while", "do", "odd"
* operators and punctuation = ".", ",", ";", ":=", "?", "!", "=", "#", "<", "<=", ">", ">=", "+", "-", "*", "/"
* literals = number

## Features

The programming language PL/0 2025 supports the following features:

| Feature                       | Value
|:------------------------------|:------------------------------------------
| Identifier Encoding           | UTF-8
| Identifier Length             | unlimited
| Case Sensitivity              | yes
| Type System                   | no, only int64 numbers
| Numerical Expressions         | yes
| Statements                    | yes
| Variable Definition	        | var
| Constant Definition	        | const
| Variable Assignment	        | :=
| Block	                        | begin … end;
| Physical Equality             | =
| Physical Inequality           | #
| Comparison	                | <&nbsp;&nbsp;>&nbsp;&nbsp;<=&nbsp;&nbsp;>=
| Function Definition	        | procedure \<name\>; \<body\>;
| Function Call	                | call \<name\>;
| Sequence	                    | ;
| Read Number	                | ? \<stdin\>
| Write Number	                | ! \<stdout\>
| If Then	                    | if \<condition\> then \<true-block\>;
| Loop Forever	                | while 1 = 1 do \<loop-body\>;
| While Condition Do	        | while \<condition\> do \<loop-body\>;
| Program End                   | .
| Multi-Line Comments           | { } or (* *)
|                               | 

## Dynamic Links and Static Links

Every procedure or function call in PL/0 (or similar block-structured languages like Pascal or Modula) creates an activation record (also called a stack frame), which holds:

- local variables
- procedure or function parameters (if any)
- return address
- administrative data (links to other activation records)

The activation record is pushed onto the call stack during a function call, and cleaned up upon return.

### Dynamic Link: The Caller Chain

The dynamic link is a stack pointer (usually stored at RBP in AMD64) that references the activation record of the calling procedure, regardless of the program’s lexical structure. It:

- enables restoration of the previous base pointer RBP during a RET instruction
- supports dynamic call chains at runtime
- used to unwind the stack or print a backtrace

Think of it as: who called me? Example: if M calls A, and A calls B, then the dynamic link of B points to the activation record of A. The dynamic link of A points to the activation record of M. This is named the "caller chain" from M to B (or dynamic link chain or dynamic chain).

### Static Link: The Lexical Scope Chain

The static link is a pointer that refers to the activation record of the lexically enclosing procedure. It:

- enables access to non-local variables from parent activation records
- used to preserve lexical scoping rules at compilation time
- required for closures and proper variable binding

Think of it as: who is my lexical parent? Example: if M has 2 local procedures A and B, then the static link of B points to the activation record of M. The static link of A points to the activation record of M. If a procedure is the direct lexical parent of another procedure (like M and A) and the parent calls this other procedure, then the static link is equal to the dynamic link. If there is a chain of parent procedures for a given procedure, then this can be named "lexical scope chain" from M to B or M to A (or static link chain or static chain).

The static link does not change based on who calls a procedure, but where the procedure is defined in the source code. 

### Dynamic Link versus Static Link

The following table provides a comparision of dynamic and static links:

| Feature          | Dynamic Link                        | Static Link                                       |
|:-----------------|:------------------------------------|:--------------------------------------------------|
| points to        | caller's activation record          | lexically enclosing procedure’s frame             |
| used for         | stack unwinding, returns, backtrace | non-local variable access                         |
| changes on call  | yes (based on runtime call chain)   | no (based on lexical nesting at compilation time) |
| location (AMD64) | usually at RBP                      | in this project at RBP-8 (hidden local variable)  |

### Static Link Computation

The compiler determines how many lexical levels separate the current procedure's block from the variable's block — this is called the depth difference between variable use (procedure's block depth) and variable declaration (variable's block depth).

Example:

```pascal
procedure A;
  var x; 				(* block A: depth 1 *)
  procedure B;
    procedure D;
    begin 				(* block D: depth 3 *)
      x := 42;			(* depth difference blocks D and A: 2 *)
    end;
  begin 				(* block B: depth 2 *)
    call D;
  end;
  procedure C;			(* block C: depth 3 *)
  begin
	call B;
  end;
begin 					(* block A: depth 1 *)
  call B;
  call C;
end;
begin 					(* top level block MAIN: depth 0 *)
  call A;
end.
```

So, procedure D must walk 2 static links up to reach a variable in A. The depth difference between D and A is 2.

### Mental Model: "Two Chains"

Even though D is called by B, it belongs to A lexically — the static link of D must point to A, not B.

```
Runtime call tree (dynamic):
  MAIN → A → B → D ⏎
  	   → A → C → B → D ⏎
	   → A ⏎ 
  MAIN

Lexical block tree (static):
  MAIN
    └── A
        └── B
            └── D
        └── C
```

Summary:
- dynamic link tracks call history
- static link tracks lexical nesting
- static links allow nested procedures to behave as if they were declared at top-level
- essential for implementing lexically scoped languages (like PL/0, Pascal)

## System V AMD64 ABI (target for PL/0 Compiler)

The PL/0 compiler emits x86-64 assembly that conforms to the **System V AMD64 ABI** (the standard on Linux, BSD, and other Unix-like systems). This ensures that:
* functions interoperate correctly with C libraries (e.g. GCC-15’s libc)  
* arguments, return values, and register usage follow a well-defined convention  
* the stack is properly aligned for calls and SIMD operations (single instruction multiple data)

SIMD load and store instructions assume a 16-byte stack alignment.

### 1. Argument Passing

For a function call its arguments aren’t all pushed onto the stack.
* integer and pointer arguments: the first six are placed in registers RDI, RSI, RDX, RCX, R8, and R9
* floating-point and vector arguments: up to eight are passed in XMM0 through XMM7

Any additional arguments (beyond the sixth integer or eighth floating-point) are passed on the stack in right-to-left order.

### 2. Return Values

Function return values are passed according to their type and size.
* integers & pointers (≤ 64 bits): return in RAX  
* integers/pointers (65–128 bits): return in RAX:RDX (low:high)  
* float/double: return in XMM0  
* small structs (≤ 16 bytes): returned in RAX/RDX or XMM0/XMM1 depending on field types  
* larger structs/unions: returned via hidden pointer in RDI and the pointer itself in RAX

### 3. Caller- vs. Callee-Saved Registers

In the System V AMD64 ABI, registers are divided into two groups:

* caller-saved (volatile): the caller must save these if it needs their values preserved across a function call
* callee-saved (non-volatile): the called function must restore these before returning if it modifies them

The table below summarizes which registers belong to each category.

| Caller-Saved (volatile) | Callee-Saved (non-volatile) |
|:-----------------------:|:---------------------------:|
| RAX                     | RBX                         |
| RCX                     | RBP (if used)               |
| RDX                     | R12                         |
| RSI                     | R13                         |
| RDI                     | R14                         |
| R8                      | R15                         |
| R9                      |                             |
| R10                     |                             |
| R11                     |                             |

Caller-saved registers may be freely clobbered by a function; if the caller needs them afterward, it must save/restore them.
Callee-saved registers must be preserved by your function (push on entry, pop before return) if you modify them.
Register RSP must always be restored to its original value before returning.

### 4. Stack Alignment

Proper stack alignment is crucial under the System V AMD64 ABI, both to satisfy the calling convention and to enable efficient, aligned memory and SIMD operations. In particular, the stack pointer (RSP) must be a multiple of 16 bytes immediately before any call, and a standard function prologue arranges this as shown below.  

```x86asm
    # --- caller does a CALL to a function ---
    call   pl0_function
    
	# while in CALL, within callee
	# CPU pushed an 8-byte return address in its CALL instruction
    #   RSP_before_call before CALL → 	   %16 == 0
    #   RSP_within_call before prologue →  %16 == 8

pl0_function:
    # prologue
    push   rbp            # RSP ← RSP - 8  → now %16 == 0
    mov    rbp, rsp       # RBP = current RSP

    # allocate FRAME bytes (FRAME %16 == 0) for locals + static link + padding
    sub    rsp, FRAME     # RSP ← RSP - FRAME → still %16 == 0

    # new stack layout (stack grows downwards)
    #    [ RBP +16 ]   ← caller’s stack before CALL
    #    [ RBP + 8 ]   ← return address of the caller
    #    [ RBP + 0 ]   ← saved RBP of the caller (called dynamic link)
    #    [ RBP - 8 ]   ← static link to lexical parent
    #    [ RBP - 16 ]  ← first local variable aligned according its bit size
    #    ...
    #    [ RBP - FRAME + 8]  ← optional padding ensuring RSP - FRAME → %16 == 0
    #    [ RBP - FRAME ]   	 ← last byte of FRAME

    # ... the code, possibly making further CALLs ...
    # since RSP %16 == 0 at each CALL, the ABI alignment rules hold

    # epilogue
    mov    rsp, rbp       # undo sub rsp, FRAME bytes allocation
    pop    rbp            # restore caller´s RBP; RSP ← RSP + 8 → %16 == 8
    ret                   # pop return address → RIP; back in caller
```

The assembly code above omits static link creation and initialization for clarity.

## Change Log

* Feb 9, 2024 - [v1.0.0](https://github.com/petersen65/pl0/releases/tag/v1.0.0)
	* initial version of scanner, parser, emitter, emulator, and compiler
	* generation and emulation of IL/0 code from PL/0 source code
	* two phase scanner: basic and sliding scanning to cover more complex scenarios with multiple characters per token
	* single phase parser: recursive descent parser directly calls emitter while analyzing token stream
	* intel like emulator: the generated IL/0 code can be executed with the emulator that looks a little bit like a x86_64 cpu

* Mar 3 2024 - [v1.1.0](https://github.com/petersen65/pl0/releases/tag/v1.1.0)
	* bug fixes, improved documentation
	* comments for the programming language PL/0
	* overhaul of the scanner due to non-valid concepts and scanning errors related to end of file conditions
	* replace misused wording "concrete syntax" with "token stream"
	* change PL/0 arithmetic expression grammar to support signed factors and signed constants

* Mar 29 2024 - [v2.0.0](https://github.com/petersen65/pl0/releases/tag/v2.0.0)
	* make core engine more mature
	* build scripts with Git commit hash inclusion into the compiler version output
	* migration to an abstract syntax tree and code generator as interface between parser and emitter
	* introduce first semantic analysis compiler phase (symbol declaration check, symbol usage check)
	* integrate error handling accross all compiler components and enable error messages with source code markers
	* draft introduction of a translation unit for 1 PL/0 source file
	* new compiler driver controls the new multi-phase compiler: lexical analysis, syntax analysis, semantic analysis, code generation
	* new and more powerful command line interface that can grow in the future
	* export of all intermediate representations of the compiler in Json, Text, and Binary format
	* new compiler driver now manages the directory tree for all build targets, including normalization and purge

* April 14 2024 - [v2.1.0](https://github.com/petersen65/pl0/releases/tag/v2.1.0)
	* provide new emulator backend for the existing compiler frontend and introduce intermediate language
	* new package implements an intermediate code that provides an additional intermediate representation on top of the abstract syntax tree
	* new emulation engine that JIT-compiles intermediate code into pseudo-assembler code which the emulator can execute

* May 5 2024 - [v2.2.0](https://github.com/petersen65/pl0/releases/tag/v2.2.0)
	* complete abstract syntax tree token handler integration beyond identifier declarations and usage
	* integrate intermediate language with token handler so that errors can be linked to source code locations
	* improve emulator target pseudo-assembler code to be more Intel CPU like with more primitives and downward growing stack

* March 1 2025 - [v2.3.0](https://github.com/petersen65/pl0/releases/tag/v2.3.0)
	* control graph representation (CFG) of intermediate code as basic blocks (draft)
	* refactoring of packages code and emulator into packages generator, emitter, and emulator
	* removal of jit compiler from emulator and implementation of new emitter package that was refactored out of the jit compiler
	* introduction of concepts "intermediate code unit" and "assembly code unit" defined as output from generator and emitter phases

## Planning

* 2025, enhance programming language and generate assembler
	* support for Intel x86_64 assembler generation and removal of integrated emulator (nasm, clib-linkage)
	* revisit and review current JSON marshalling and text output implementation, add Marshal/Unmarshal functions to interfaces
	* check if panic calls in the 'code' module should include the tokenstream in its error messages
	* implemention of control flow graph (CFG) with lifeness and use information for variables
	* implementation of readln and writeln functions in new PL/0 standard library which itself is written in C23
	* design or redesign of public APIs for all packages of the compiler (rethink public/private visibility)
	* unit tests for all public APIs
	* integrate Pascal-like scanner and parser into the PL/0 scanner and parser (type system, procedure parameters)
	* implement analyzers and optimizers documented in compiler construction literature (code flow and data flow analysis, context flow graph, DAG)
	* implement constant folding based on abstract syntax tree
	* implement closure support for the compiler's intermediate language
