# PL/0 Compiler

This module provides a complete compiler for the programming language PL0. It provides several packages that can be used independently of its command line interface.

* scanner: lexical analysis of PL/0 syntax, converting input characters in UTF-8 encoding to a concrete syntax table with token descriptions
* parser: syntax analysis of PL/0 code, ensuring it adheres to the rules defined in the extended Backus-Naur form for the language
* emitter: code generation of IL/0 intermediate language code, called by recursive descent parser during syntax analysis of PL/0 code
* emulator: execution of the IL/0 intermediate language code produced by the emitter, runs process on virtual cpu with stack and registers
* compiler: compiler driver for scanning, parsing, emitting, printing, and emulating from source code to the resultant IL/0 code

The reason for creating the compiler is that I have been interested in compiler construction since my computer science studies. Since I was already working with Niklaus Wirth's programming languages at the end of the 1990s, it made sense to build on what I had learned back then and start a compiler project with a modern programming language. I decided on the Go programming language because it is lean and available on all common operating systems. Not all features of Go were used (e.g. goroutines or channels) so that the compiler can be easily ported to other programming languages. The compiler translates the programming language PL/0 from 1986 into a so-called Intermediate Language IL/0, for which an emulator is part of the project. Why PL/0? I start with PL/0 because this language is very simple and reduced, so that its compiler can be written and understood by one person.

From now on, the PL/0 compiler is a personal hobby of mine, which I will continue to work on after its initial creation. My activities can be found below in the change log and in the planning sections. Interested students and developers are welcome to learn from my project how a compiler works and looks from the inside. I document the source code and structure the project for better traceability. Variable names are also slightly longer than usual so that the source code can be understood. The source code is also prepared for extensibility and encapsulation by using packages, public, and private implementation-patterns.

I test and check the source code for errors. You are welcome to tell me about errors and make suggestions. However, I can only do this in my private time.

## License

The complete compiler source code is licensed under Apache License v2.0.

## PL/0

PL/0 is a programming language, intended as an educational programming language, that is similar to but much simpler than Pascal, a general-purpose programming language. It serves as an example of how to construct a compiler. It was originally introduced in the book, Algorithms + Data Structures = Programs, by Niklaus Wirth in 1976. It features quite limited language constructs: there are no real numbers, very few basic arithmetic operations and no control-flow constructs other than "if" and "while" blocks. While these limitations make writing real applications in this language impractical, it helps the compiler remain compact and simple.

Tokens form the vocabulary of the PL/0 language. There are four classes: 

* identifiers
* keywords
* operators and punctuation
* literals

## Syntax

The syntax of PL/0 (1986 version) is described in extended Backus-Naur form with the following productions:

| nonterminal symbol | terminal or nonterminal symbols
|--------------------|----------------------------------------------------------------------------------
| program            | block "." ;
|                    | &nbsp;
| block              | [ "const" ident "=" number {"," ident "=" number} ";"]
|                    | [ "var" ident {"," ident} ";"]
|                    | { "procedure" ident ";" block ";" } statement ;
|                    | &nbsp;
| statement          | [ ident ":=" expression | "call" ident
|                    | &nbsp;&nbsp;\| "?" ident | "!" expression
|                    | &nbsp;&nbsp;\| "begin" statement {";" statement } "end"
|                    | &nbsp;&nbsp;\| "if" condition "then" statement
|                    | &nbsp;&nbsp;\| "while" condition "do" statement ] ;
|                    | &nbsp;
| condition          | "odd" expression
|                    | &nbsp;&nbsp;\| expression ("=" \| "#" \| "<" \| "<=" \| ">" \| ">=") expression ;
|                    | &nbsp;
| expression         | ["+" \| "-"] term { ( "+" \| "-") term} ;
| term               | factor {("*" \| "/") factor} ;
| factor             | ident \| number \| "(" expression ")" ;
|                    | 

## Features

The programming language PL/0 (1986 version) supports the following features:

| Feature                       | Value
|-------------------------------|-------------------------------------------
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
| Read Number	                | stdin
| Write Number	                | stdout
| If Then	                    | if \<condition\> then \<true-block\>;
| Loop Forever	                | while 1 = 1 do \<loop-body\>;
| While Condition Do	        | while \<condition\> do \<loop-body\>;
| Program End                   | .
|                               | 

## Change Log

* Mar 1st, 2024 - [v1.0.0](https://github.com/petersen65/pl0/releases/tag/v1.0.0)
	* initial version of scanner, parser, emitter, emulator, and compiler
	* generation and emulation of IL/0 code from PL/0 source code
	* two pass scanner: basic and sliding scanning to cover more complex scenarios with multiple characters per token
	* register allocation algorithm enables expressions to use registers instead of a stack
	* recursions with local scoped procedures tested

## Planning

* H1 2024, Compiler version 1.1 2024, make core engine more mature
	* migration of memory location evaluations to emitter
	* comments for the source code
	* migration to abstract syntax trees as interface between parser and emitter
	* support for Intel x86_64 assembler generation based on NASM project

* H2 2024, Compiler version 1.2 2024, enhance programming language
	* data types for variables, constants and parameters, start with integer only
	* parameter for procedures
	* boolean, real, string datatypes
	* improved features for stdin and stdout
	* optimizations like constant folding
