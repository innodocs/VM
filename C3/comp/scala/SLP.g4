grammar SLP;

/*
 * Parser rules
 *
 * for documentation on labeling rule alternatives,
 * see #ANTLR-Manual, pg 118
 */
 
prog returns [Object ast]
   : stm EOF
   ;
 
stm returns [Object /* Absyn.Stm */ ast]
   : stm SEMICOLON             # SingleStm
   | stm SEMICOLON stm         # CompoundStm
   | ID ASSIGN exp             # AssignStm
   | PRINT LPAREN exps RPAREN  # PrintStm
   ;
 
exps returns [Object /* List[Absyn.Exp] */ ast]
   : exp
   | exp COMMA exps
   ;

exp returns [Object /* Absyn.Exp */ ast]
   : exp TIMES exp             # OpExp
   | exp DIV exp               # OpExp
   | exp PLUS exp              # OpExp
   | exp MINUS exp             # OpExp
   | stm COMMA exp             # EseqExp
   | LPAREN exp RPAREN         # ParenExp
   | INT                       # NumExp
   | ID                        # IdExp
   ;


/*
 * Lexer rules
 */

ASSIGN   : ':=';
DIV      : '/';
TIMES    : '*';
MINUS    : '-';
PLUS     : '+';
LPAREN   : '(';
RPAREN   : ')'; 
SEMICOLON: ';';
COMMA    : ',';

PRINT    : 'print';
ID       : ID_LETTER (ID_LETTER | DIGIT)*;

INT      : DIGIT+;

fragment DIGIT: [0-9];
fragment ID_LETTER : 'a'..'z'|'A'..'Z'|'_' ;

WS       : [ \t\r\n]+ ->  skip;
COMMENT  : '#' .*? '\r'? '\n' -> skip ; // Match "#" any '\n

