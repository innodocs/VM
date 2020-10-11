(*
**  parser.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 01/30/19.
**  Copyright © 2019 innodocs. All rights reserved.
*)


structure Parser : sig
    val parse : string -> Absyn.stm
end = struct

  structure SLPLrVals = SLPLrValsFun(structure Token = LrParser.Token)
  structure Lex = SLPLexFun(structure Tokens = SLPLrVals.Tokens)
  structure SLPP = Join(structure ParserData = SLPLrVals.ParserData
			structure Lex=Lex
			structure LrParser = LrParser)
			
  fun parse filename = let
    val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
	  val file = TextIO.openIn filename
	  fun get _ = TextIO.input file
	  fun parseerror(s,p1,p2) = ErrorMsg.error p1 s
	  val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
	  val (absyn, _) = SLPP.parse(30,lexer,parseerror,())
  in
    TextIO.closeIn file;
	  absyn
  end handle LrParser.ParseError => raise ErrorMsg.Error

end (* structure Parser *)
