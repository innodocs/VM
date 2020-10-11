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
   : ((label NL?)? instr NL | NL)*
   ; 

label
   : LABEL COLON
   ;
    
instr
   : branch_instr
   | HALT
   | ILOADG    INT
   | ISTOREG   INT
   | IPUSHC    INT
   | IPOP
   | ISWAP
   | IDUP
   | IADD
   | ISUB
   | IMULT
   | IDIV
   | IMOD
   | IPOW
   | IAND
   | IOR
   | ICMP
   | ICMPEQ
   | ICMPNE
   | ICMPLT
   | ICMPGE
   | ICMPGT
   | ICMPLE
   | INEG
   | IPRINT    INT
   ;
   
branch_instr
   : GOTO      LABEL  # BranchInstr
   | IF_ICMPEQ LABEL  # BranchInstr
   | IF_ICMPNE LABEL  # BranchInstr
   | IF_ICMPLT LABEL  # BranchInstr
   | IF_ICMPGE LABEL  # BranchInstr
   | IF_ICMPGT LABEL  # BranchInstr
   | IF_ICMPLE LABEL  # BranchInstr
   | IF_IEQ    LABEL  # BranchInstr
   | IF_INE    LABEL  # BranchInstr
   ;   


/*
 * Lexer rules
 */

GLBLS     : 'GLOBALS';
 
HALT      : 'HALT';

// load/save globals
ILOADG    : 'ILOADG';
ISTOREG   : 'ISTOREG';

// stack ops
IPUSHC    : 'IPUSHC';
IPOP      : 'IPOP';
ISWAP     : 'ISWAP';
IDUP      : 'IDUP';

// branch ops
GOTO      : 'GOTO';
IF_ICMPEQ : 'IF_ICMPEQ';
IF_ICMPNE : 'IF_ICMPNE';
IF_ICMPLT : 'IF_ICMPLT';
IF_ICMPGE : 'IF_ICMPGE';
IF_ICMPGT : 'IF_ICMPGT';
IF_ICMPLE : 'IF_ICMPLE';
IF_IEQ    : 'IF_IEQ';
IF_INE    : 'IF_INE';

// arithmetic ops
IADD    : 'IADD';
ISUB    : 'ISUB';
IMULT   : 'IMULT';
IDIV    : 'IDIV';
IMOD    : 'IMOD';
IPOW    : 'IPOW';

// logical ops
IAND    : 'IAND';
IOR     : 'IOR';

// comparison ops
ICMP    : 'ICMP';
ICMPEQ  : 'ICMPEQ';
ICMPNE  : 'ICMPNE';
ICMPLT  : 'ICMPLT';
ICMPGE  : 'ICMPGE';
ICMPGT  : 'ICMPGT';
ICMPLE  : 'ICMPLE';

// unary ops
INEG    : 'INEG';
INOT    : 'INOT';

// built-in functions
IPRINT  : 'IPRINT';

LABEL   : ID;
COLON   : ':';

ID      : ID_LETTER (ID_LETTER | DIGIT)*;
INT     : DIGIT+;

fragment DIGIT: [0-9];
fragment ID_LETTER : 'a'..'z'|'A'..'Z'|'_' ;

NL      : '\r'? '\n';

WS      : [ \t]+ ->  skip;
COMMENT : '//' ~[\r\n]* '\r'? '\n' -> skip;
