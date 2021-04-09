(*
**  gap.lex
**  vm-comp
**
**  Created by Ovidiu Podisor on 03/05/19.
**  Copyright © 2019-2021 innodocs. All rights reserved.
*)

type pos = int

type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

fun esc(yytext, yypos) =
  PrettyPrint.parseString (yytext, fn s => ErrorMsg.error yypos s)


%%
%header (functor GAPLexFun(structure Tokens : GAP_TOKENS));

%s G STRING COMMENT;

id_letter = [a-z] | [A-Z] | "_";
digit     = [0-9];
ws        = [\ \t\r\n];
eol       = "\n";


%%
<INITIAL>{ws}* => (lineNum := 1; YYBEGIN G; continue ());

<G>\n         => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<G>":="       => (Tokens.ASSIGN(yypos,yypos+2));
<G>"("        => (Tokens.LPAREN(yypos,yypos+1));
<G>")"        => (Tokens.RPAREN(yypos,yypos+1));
<G>"["        => (Tokens.LBRACK(yypos,yypos+1));
<G>"]"        => (Tokens.RBRACK(yypos,yypos+1));
<G>";"        => (Tokens.SEMICOLON(yypos,yypos+1));
<G>","        => (Tokens.COMMA(yypos,yypos+1));
<G>".."       => (Tokens.DOTDOT(yypos,yypos+2));

<G>"+"        => (Tokens.PLUS(yypos,yypos+1));
<G>"-"        => (Tokens.MINUS(yypos,yypos+1));
<G>"*"        => (Tokens.TIMES(yypos,yypos+1));
<G>"/"        => (Tokens.DIV(yypos,yypos+1));
<G>"mod"      => (Tokens.MOD(yypos,yypos+3));
<G>"^"        => (Tokens.POW(yypos,yypos+1));

<G>"="        => (Tokens.EQ(yypos,yypos+1));
<G>"<>"       => (Tokens.NEQ(yypos,yypos+2));
<G>"<"        => (Tokens.LT(yypos,yypos+1));
<G>"<="       => (Tokens.LE(yypos,yypos+2));
<G>">"        => (Tokens.GT(yypos,yypos+1));
<G>">="       => (Tokens.GE(yypos,yypos+2));

<G>"and"      => (Tokens.AND(yypos,yypos+3));
<G>"or"       => (Tokens.OR(yypos,yypos+2));
<G>"not"      => (Tokens.NOT(yypos,yypos+3));

<G>"if"       => (Tokens.IF(yypos,yypos+2));
<G>"then"     => (Tokens.THEN(yypos,yypos+4));
<G>"else"     => (Tokens.ELSE(yypos,yypos+4));
<G>"fi"       => (Tokens.FI(yypos,yypos+2));

<G>"while"    => (Tokens.WHILE(yypos,yypos+5));
<G>"do"       => (Tokens.DO(yypos,yypos+2));
<G>"od"       => (Tokens.OD(yypos,yypos+2));

<G>"for"      => (Tokens.FOR(yypos,yypos+3));
<G>"in"       => (Tokens.IN(yypos,yypos+2));

<G>"repeat"   => (Tokens.REPEAT(yypos,yypos+6));
<G>"until"    => (Tokens.UNTIL(yypos,yypos+5));

<G>{digit}+   => (Tokens.INT(valOf(Int.fromString yytext),yypos,yypos+size yytext));
<G>{id_letter}({id_letter}|{digit})* => (if yytext = "Print" 
                       then Tokens.PRINT(yypos, yypos+size yytext)
                       else Tokens.ID(yytext, yypos, yypos+size yytext));
<G>{ws}+      => (continue());

<G>"\""       => (YYBEGIN STRING; continue());
<STRING>"\""  => (YYBEGIN G; continue());
<STRING>([^\"])* => ((fn () => let val text = esc (yytext, yypos) in
                       Tokens.STRING(text, yypos, yypos + size text)
                     end)());

<G>"#"        => (YYBEGIN COMMENT; continue());
<COMMENT>{eol}=> (lineNum := !lineNum+1; linePos := yypos :: !linePos;
                  YYBEGIN G; continue());
<COMMENT>.    => (continue());

<G>.          => (print ("illegal character " ^ yytext); ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
