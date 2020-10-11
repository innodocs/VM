(*
**  gap.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 01/31/19.
**  Copyright © 2019 innodocs. All rights reserved.
*)


structure GAP: sig
     val print   : string -> unit
     val eval    : string -> (Absyn.id * int) list
     val compile : string -> string -> unit
     val asm     : string -> string -> unit
  end = struct

  structure A = Absyn
  structure P = Parser
  structure C = Comp
  
  fun print gapFilename = let
    val ast = P.parse gapFilename
  in
    C.print ast
  end
  
  fun eval gapFilename = let
    val ast = P.parse gapFilename
  in
    C.eval ast
  end  
  
  fun compile gapFilename vmFilename = let
    val ast = P.parse gapFilename
  in
    C.compile ast vmFilename
  end 
  
  fun asm gapFilename asmFilename = let
    val ast = P.parse gapFilename
  in
    C.asm ast asmFilename
  end   

end (* structure GAP *)
