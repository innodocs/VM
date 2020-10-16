grammar GAP;

/*
 * Parser rules
 *
 * for documentation on labeling rule alternatives,
 * see #ANTLR-Manual, pg 118
 *
 * for documentation on the GAP syntax,
 * see the GAP Reference, https://www.gap-system.org/Doc/manuals.html
 */
 
prog returns [/* Absyn.Stm */Object ast]
   : stms EOF
   ;

stms returns [/* Absyn.Stm */Object ast] 
   : stm SEMICOLON stms?          # SeqStm
   | SEMICOLON stms?              # EmptyStm
   ;
    
stm returns [/* Absyn.Stm */Object ast]
   : IF exp THEN stms (ELSE stms)? FI # IfStm
   | WHILE exp DO stms OD             # WhileStm
   | FOR ID IN rngExp DO stms OD      # ForStm
   | REPEAT stms UNTIL exp            # RepeatStm
   | ID ASSIGN exp                    # AssignStm
   | PRINT LPAREN exps RPAREN         # PrintStm
   ;
    
exps returns [/* List[Absyn.Exp] */Object ast]
   : exp
   | exp COMMA exps
   ;

exp returns [/* Absyn.Exp */Object ast]
   : exp POW<assoc=right> exp     # BinOpExp
   | (PLUS | MINUS) exp           # UnOpExp
   | exp (TIMES | DIV | MOD) exp  # BinOpExp
   | exp (PLUS | MINUS) exp       # BinOpExp
   | exp (EQ | NEQ | LT | LE
        | GT | GE) exp            # BinOpExp
   | NOT exp                      # UnOpExp
   | exp AND exp                  # BinOpExp
   | exp OR exp                   # BinOpExp   
   | LPAREN exp RPAREN            # ParenExp
   | ID                           # VarExp
   | INT                          # NumExp
   ;

rngExp returns [/* Absyn.Exp */Object ast]
   : LBRACK INT DOTDOT INT RBRACK # RangeExp
   ;

/*
 * Lexer rules
 */
ASSIGN   : ':=';
LPAREN   : '(';
RPAREN   : ')';
LBRACK   : '[';
RBRACK   : ']';
SEMICOLON: ';';
COMMA    : ',';
DOTDOT   : '..';

PLUS     : '+';
MINUS    : '-';
TIMES    : '*';
DIV      : '/';
MOD      : 'mod';
POW      : '^';

EQ       : '=';
NEQ      : '<>';
LT       : '<';
LE       : '<=';
GT       : '>';
GE       : '>=';

AND      : 'and';
OR       : 'or';
NOT      : 'not';

IF       : 'if';
THEN     : 'then';
ELSE     : 'else';
FI       : 'fi';

WHILE    : 'while';
DO       : 'do';
OD       : 'od';
FOR      : 'for';
IN       : 'in';
REPEAT   : 'repeat';
UNTIL    : 'until';

PRINT    : 'Print';
ID       : ID_LETTER (ID_LETTER | DIGIT)*;

INT      : DIGIT+;

fragment DIGIT: [0-9];
fragment ID_LETTER : 'a'..'z'|'A'..'Z'|'_' ;

WS       : [ \t\r\n]+ ->  skip;
COMMENT  : '#' .*? '\r'? '\n' -> skip ; // Match "#" any '\n
