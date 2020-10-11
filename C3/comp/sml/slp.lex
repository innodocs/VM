type pos = int

type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end


%%
%header (functor SLPLexFun(structure Tokens : SLP_TOKENS));

digits = [0-9];
ws     = [\ \t\n];


%%
\n	    		=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
":="			  => (Tokens.ASSIGN(yypos,yypos+2));
"/"     		=> (Tokens.DIV(yypos,yypos+1));
"*"     		=> (Tokens.TIMES(yypos,yypos+1));
"-"     		=> (Tokens.MINUS(yypos,yypos+1));
"+"     		=> (Tokens.PLUS(yypos,yypos+1));
"("	    		=> (Tokens.LPAREN(yypos,yypos+1));
")"	    		=> (Tokens.RPAREN(yypos,yypos+1));
";"	    		=> (Tokens.SEMICOLON(yypos,yypos+1));
","	    		=> (Tokens.COMMA(yypos,yypos+1));
{digits}+  	=> (Tokens.INT(valOf(Int.fromString yytext),yypos,yypos+size yytext));
[a-z][a-z0-9]* => (if yytext = "print" 
                       then Tokens.PRINT(yypos, yypos+size yytext)
                       else Tokens.ID(yytext, yypos, yypos+size yytext));
{ws}+       => (continue());
.       		=> (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
