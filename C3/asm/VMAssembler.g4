grammar VMAssembler;

/*
 * Parser rules
 */
 
prog
   : decls instrs EOF
   ;
   
decls
   : (GLBLS INT NL)?
   ;
    
instrs
   : (instr NL | NL)*
   ; 
   
instr
   : HALT
   | ILOADG  INT
   | ISTOREG INT
   | IPUSHC  INT
   | IPOP
   | ISWAP
   | IDUP
   | IADD
   | ISUB
   | IMULT
   | IDIV
   | IMOD
   | INEG
   | IPRINT  INT
   ;



/*
 * Lexer rules
 */
 
GLBLS   : 'GLOBALS';
 
HALT    : 'HALT';
ILOADG  : 'ILOADG';
ISTOREG : 'ISTOREG';
IPUSHC  : 'IPUSHC';
IPOP    : 'IPOP';
ISWAP   : 'ISWAP';
IDUP    : 'IDUP';
IADD    : 'IADD';
ISUB    : 'ISUB';
IMULT   : 'IMULT';
IDIV    : 'IDIV';
IMOD    : 'IMOD';
INEG    : 'INEG';
IPRINT  : 'IPRINT';

IDENT   : [a-z][a-z0-9]*;

INT     : [0-9]+;
STRING  : ('"' (~('"'))* '"');

NL      : '\r'? '\n';

WS      : [ \t]+ ->  skip;
COMMENT : '//' ~[\r\n]* '\r'? '\n' -> skip;
