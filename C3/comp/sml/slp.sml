(*
**  slp.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 01/30/19.
**  Copyright © 2019 innodocs. All rights reserved.
*)


structure SLP: sig
     val print   : string -> unit
     val eval    : string -> (Absyn.id * int) list
     val compile : string -> string -> unit
     val asm     : string -> string -> unit
  end = struct

  structure A = Absyn
  structure P = Parser
  structure C = Comp
  
  fun print slpFilename = let
    val ast = P.parse slpFilename
  in
    C.print ast
  end
  
  fun eval slpFilename = let
    val ast = P.parse slpFilename
  in
    C.eval ast
  end  
  
  fun compile slpFilename vmFilename = let
    val ast = P.parse slpFilename
  in
    C.compile ast vmFilename
  end 
  
  fun asm slpFilename asmFilename = let
    val ast = P.parse slpFilename
  in
    C.asm ast asmFilename
  end   

end (* structure SLP *)

