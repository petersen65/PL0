# PL0

## Programming Language PL0 Compiler

The syntax of PL/0 (1975 version) described in extended Backus-Naur form

program = block .
 
block = [ const ident = number {, ident = number} ;]
        [ var ident {, ident} ;]
        { procedure ident ; block ; } statement .
 
statement = [ ident := expression | call ident 
              | ? ident | ! expression 
              | begin statement {; statement } end 
              | if condition then statement 
              | while condition do statement ].
 
condition = odd expression |
            expression (=|#|&amp;lt;|&amp;lt;=|&amp;gt;|&amp;gt;=) expression .
 
expression = [ +|-] term { (+|-) term}.
 
term = factor {(*|/) factor}.
 
factor = ident | number | ( expression )

## Elements of syntax

Case-sensitivity	yes
Variable assignment	:=
Variable declaration	var
Block	begin … end
Physical (shallow) equality	=
Physical (shallow) inequality	#
Comparison	< >
Function definition	procedure <name>; <body>;
Function call	call <name>
Sequence	;
If – then	if <condition> then <trueBlock>
Loop forever	while 1 = 1 do <loopBody>
While condition do	while do <loopBody>
