# PL/0 Compiler

This module provides a complete compiler for the programming language PL0. It provides several packages that can be used independently of its command line interface.

* scanner: lexical analysis of PL/0 syntax, converting input characters in UTF-8 encoding to a concrete syntax table with token descriptions
* parser: syntax analysis of PL/0 code, ensuring it adheres to the rules defined in the extended Backus-Naur form for the language
* emitter: code generation of IL/0 intermediate language code, called by recursive descent parser during syntax analysis of PL/0 code
* emulator: execution of the IL/0 intermediate language code produced by the emitter, runs process on virtual cpu with stack and registers
* compiler: compiler driver for scanning, parsing, emitting, printing, and emulating from source code to the resultant IL/0 code

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

The programming language PL/0 supports the following features:

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
| Sequence	                | ;
| Read Number	                | stdin
| Write Number	                | stdout
| If Then	                | if \<condition\> then \<true-block\>;
| Loop Forever	                | while 1 = 1 do \<loop-body\>;
| While Condition Do	        | while \<condition\> do \<loop-body\>;
| Program End                   | .
|                               | 

## Change Log

* Feb 1st, 2024 - [v1.0.0](https://github.com/petersen65/pl0/releases/tag/v1.0.0)
	* initial version of scanner, parser, emitter, emulator, and compiler
	* generation and emulation of IL/0 code from PL/0 source code
	* two phase scanner: basic and sliding phases to cover more complex scenarios with multiple characters per token
