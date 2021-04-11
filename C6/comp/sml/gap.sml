(*
**  gap.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 03/05/19.
**  Copyright © 2019-2021 innodocs. All rights reserved.
*)

structure GAP: sig
     val print   : string -> unit
     val eval    : string -> Eval.env
     val compile : string -> string -> unit
     val asm     : string -> string -> unit
  end = struct

  structure A  = Absyn
  structure S  = Symbol
  structure P  = Parser
  structure C  = Comp
  structure T  = Type
  structure PP = PrettyPrint
  structure E  = Eval

  fun errorHandler(e, cat: string, v: 'a) =
      case e of
       Type.Error(s, p) => (ErrorMsg.error p (" " ^ cat ^ " error: " ^ s); v)
     | _                => (ErrorMsg.error 0 (" " ^ cat ^ " error"); v)

  fun print gapFilename = let
    val ast = P.parse gapFilename
  in
    PP.print ast
  end handle e => errorHandler(e, "pretty print", ())

  fun eval gapFilename = let
    val ast = P.parse gapFilename
  in
    E.eval ast
  end handle e => errorHandler(e, "evaluation", S.empty)

  fun compile gapFilename vmFilename = let
    val ast = P.parse gapFilename
  in
    C.compile ast vmFilename
  end handle e => errorHandler(e, "compilation", ())
  
  fun asm gapFilename asmFilename = let
    val ast = P.parse gapFilename
  in
    C.asm ast asmFilename
  end handle e => errorHandler(e, "assembly", ())

end (* structure GAP *)
