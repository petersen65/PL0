# PL/0

PL/0 is a programming language, intended as an educational programming language, that is similar to but much simpler than Pascal, a general-purpose programming language. It serves as an example of how to construct a compiler. It was originally introduced in the book, Algorithms + Data Structures = Programs, by Niklaus Wirth in 1976. It features quite limited language constructs: there are no real numbers, very few basic arithmetic operations and no control-flow constructs other than "if" and "while" blocks. While these limitations make writing real applications in this language impractical, it helps the compiler remain compact and simple.

Tokens form the vocabulary of the PL/0 language. There are four classes: identifiers, keywords, operators and punctuation, and literals

## Programming Language PL/0 Compiler

The syntax of PL/0 (1975 version) described in extended Backus-Naur form

program = block "." ;

block = [ "const" ident "=" number {"," ident "=" number} ";"]
        [ "var" ident {"," ident} ";"]
        { "procedure" ident ";" block ";" } statement ;

statement = [ ident ":=" expression | "call" ident 
              | "?" ident | "!" expression 
              | "begin" statement {";" statement } "end" 
              | "if" condition "then" statement 
              | "while" condition "do" statement ];

condition = "odd" expression |
            expression ("="|"#"|"<"|"<="|">"|">=") expression ;

expression = [ "+"|"-"] term { ("+"|"-") term};

term = factor {("*"|"/") factor};

factor = ident | number | "(" expression ")";

## Elements Of Syntax

Case-sensitivity                yes

Variable assignment	        :=

Variable declaration	        var

Block	                        begin … end

Physical (shallow) equality	=

Physical (shallow) inequality	#

Comparison	                < >

Function definition	        procedure <name>; <body>;

Function call	                call <name>

Sequence	                ;

If – then	                if <condition> then <trueBlock>

Loop forever	                while 1 = 1 do <loopBody>

While condition do	        while do <loopBody>
