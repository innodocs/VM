(*
**  parser.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 01/31/19.
**  Copyright © 2019-2021 innodocs. All rights reserved.
*)


structure Parser : sig
    val parse : string -> Absyn.stm
end = struct

  structure GAPLrVals = GAPLrValsFun(structure Token = LrParser.Token)
  structure Lex = GAPLexFun(structure Tokens = GAPLrVals.Tokens)
  structure GAPP = Join(structure ParserData = GAPLrVals.ParserData
			structure Lex=Lex
			structure LrParser = LrParser)
			
  fun parse filename = let
    val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
	  val file = TextIO.openIn filename
	  fun get _ = TextIO.input file
	  fun parseerror(s,p1,p2) = ErrorMsg.error p1 s
	  val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
	  val (absyn, _) = GAPP.parse(30,lexer,parseerror,())
  in
    TextIO.closeIn file;
	  absyn
  end handle LrParser.ParseError => raise ErrorMsg.Error

end (* structure Parser *)
