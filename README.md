# PL/0 Compiler

This module provides a complete compiler for the programming language PL/0. It provides several packages that can be used independently of its command line interface.

* scanner: lexical analysis of PL/0 source code by converting input characters in UTF-8 encoding to a token stream table with token descriptions
* parser: syntax analysis of PL/0 token stream ensuring it adheres to the rules defined in the extended Backus-Naur form for the PL/0 language
* ast: abstract syntax composition for PL/0 token stream by creating a normalized representation of source code as in-memory abstract syntax tree 
* generator: compiler pass for code generation by traversing the abstract syntax tree to drive the IL/0 emitter
* emitter: binary output of IL/0 intermediate language, called by code generator during abstract syntax tree traversal
* emulator: execution of IL/0 intermediate language instructions by running a process on a virtual cpu with stack and registers
* core: core features, token type-system, and error-handling mechanism used by all compiler components
* compiler: driver for all compiler components, from scanning PL/0 source code to executing and printing resultant IL/0 code

The reason for creating the compiler is that I have been interested in compiler construction since my computer science studies. Since I was already working with Niklaus Wirth's programming languages at the end of the 1990s, it made sense to build on what I had learned back then and start a compiler project with a modern programming language. I decided on the Go programming language because it is lean and available on all common operating systems. The compiler translates the programming language PL/0 from 1986 into a so-called Intermediate Language IL/0, for which an emulator is part of the project. Why PL/0? I start with PL/0 because this language is very simple and reduced, so that its compiler can be written and understood by one person.

From now on, the PL/0 compiler is a personal hobby of mine, which I will continue to work on after its initial creation. My activities can be found below in the change log and in the planning sections. Interested students and developers are welcome to learn from my project how a compiler works and looks from the inside. I document the source code and structure the project for better traceability. Variable names are also slightly longer than usual so that the source code can be understood. The source code is also prepared for extensibility and encapsulation by using packages, public, and private implementation-patterns.

For Visual Studio Code, you should be able to run the compiler with F5. Please install the lastest Go version before that. You will be guided to a playground.pl0 file where you can try out PL/0 programming. Support for dev-containers is also provided, if you cannot install Go on your operating system. The PL/0 compiler was tested under Windows 11, Linux Ubuntu x86_64, and macOS Sonoma M2.

I test and check the source code for errors. You are welcome to tell me about errors and make suggestions. However, I can only do this in my private time.

## License

The complete compiler source code is licensed under Apache License v2.0.

## PL/0

PL/0 in its original version from 1976 is an educational programming language, that is similar to but much simpler than Pascal, a well-known general-purpose programming language. It serves as an example of how to construct a compiler. It was originally introduced in the book, Algorithms + Data Structures = Programs, by Niklaus Wirth in 1976. It featured quite limited language constructs: there are no real numbers, very few basic arithmetic operations and no control-flow constructs other than "if" and "while" blocks. While these limitations made writing real applications in this language impractical, it helped the compiler remain compact and simple.

Beginning with version 1.1.0 of my compiler, I will start to extend the 1986 version of PL/0 and hence name it PL/0 2024. The programming language PL/0 2024 will be aligned to Pascal syntax and language features.

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

| nonterminal symbol | terminal or nonterminal symbols
|--------------------|------------------------------------------------------------------------------------------
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

The programming language PL/0 2024 supports the following features:

| Feature                       | Value
|-------------------------------|-------------------------------------------
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

## Change Log

* Feb 9th, 2024 - [v1.0.0](https://github.com/petersen65/pl0/releases/tag/v1.0.0)
	* initial version of scanner, parser, emitter, emulator, and compiler
	* generation and emulation of IL/0 code from PL/0 source code
	* two pass scanner: basic and sliding scanning to cover more complex scenarios with multiple characters per token
	* single pass parser: recursive descent parser directly calls emitter while analyzing token stream
	* intel like emulator: the generated IL/0 code can be executed with the emulator that looks a little bit like a x86_64 cpu

* Mar 3rd 2024 - [v1.1.0](https://github.com/petersen65/pl0/releases/tag/v1.1.0)
	* bug fixes, improved documentation
	* comments for the programming language PL/0
	* overhaul of the scanner due to non-valid concepts and scanning errors related to end of file conditions
	* replace misused wording "concrete syntax" with "token stream"
	* change PL/0 arithmetic expression grammar to support signed factors and signed constants

* Mar 29 2024 - [v2.0.0](https://github.com/petersen65/pl0/releases/tag/v2.0.0)
	* make core engine more mature
	* build scripts with Git commit hash inclusion into the compiler version output
	* migration to an abstract syntax tree and code generator as interface between parser and emitter
	* introduce first semantic analysis compiler pass (symbol declaration check, symbol usage check)
	* integrate error handling accross all compiler components and enable error messages with source code markers
	* draft introduction of a translation unit for 1 PL/0 source file
	* new compiler driver controls the new multi-pass compiler: lexical analysis, syntax analysis, semantic analysis, code generation
	* new and more powerful command line interface that can grow in the future
	* export of all intermediate representations of the compiler in Json, Text, and Binary format
	* new compiler driver now manages the directory tree for all build targets, including normalization and purge

## Planning

* Q2 2024, Compiler version 2.1.0 2024, provide assembler backend for the existing compiler frontend
	* assembler package that emits x86_64 assembler mnemonics for 'nasm' and links the assembler output with libc

* H2 2024, Compiler version 3.0.0 2024, enhance programming language and generate assembler
	* integrate Pascal scanner and parser into the PL/0 scanner and parser
	* migrate the PL/0 project to a Pascal II project which allows to attach interfaces to user defined datatypes and records
	* think about generating C code for GCC and use GCC as backend for PL/0
	* support for Intel x86_64 assembler generation (e.g. nasm, gcc asm, clib-linkage, bare metal target based on uefi)
	* implement optimizers like constant folding
