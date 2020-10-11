(*
**  test.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 01/30/19.
**  Copyright © 2019 innodocs. All rights reserved.
*)

structure Test = struct

local
  open Absyn
in

val prog1 = 
  CompoundStm(
    AssignStm(
      "a",
      OpExp(
        NumExp 5,
        Plus,
        NumExp 3)),
    CompoundStm(
      AssignStm(
        "b",
        EseqExp(
          PrintStm[
            IdExp "a",
            OpExp(
              IdExp "a",
              Minus,
              NumExp 1)],
            OpExp(
              NumExp 10,
              Times,
              IdExp "a"))),
      PrintStm[
        IdExp "b"]));

val prog2 =
  CompoundStm(
	  AssignStm(
	    "a",
	    OpExp(
	      OpExp(
	        OpExp(
	          NumExp 1,
	          Plus,
	          NumExp 2),
	        Plus,
	        OpExp(
	          NumExp 3,
	          Plus,
	          NumExp 4)),
	      Plus,
	      OpExp(
	        OpExp(
	          NumExp 5,
	          Plus,
	          NumExp 6),
	        Plus,
	        OpExp(
	          NumExp 7,
	          Plus,
	          NumExp 8)))),
	 PrintStm[
        IdExp "a"]);


val prog3 =
  CompoundStm(
	  AssignStm(
	    "a",
	    OpExp(
	      OpExp(
	        OpExp(
	          NumExp 1,
	          Plus,
	          NumExp 2),
	        Plus,
	        OpExp(
	          NumExp 3,
	          Plus,
	          NumExp 4)),
	      Times,
	      OpExp(
	        OpExp(
	          NumExp 5,
	          Plus,
	          NumExp 6),
	        Plus,
	        OpExp(
	          NumExp 7,
	          Plus,
	          NumExp 8)))),
	 PrintStm[
        IdExp "a"])
     
fun run() = let
  val progs = [(prog1, "prog1"), (prog2, "prog2"), (prog3, "prog3")]
  val files = ["test1", "test2", "test3"];
in
  map (fn p => let val fp = "../../test/" ^ (#2 p) in
         (print ("\n" ^ (#2 p) ^ ":\n");
          Comp.print (#1 p);
          Comp.eval (#1 p);
          Comp.compile (#1 p) (fp ^ ".vm");
          Comp.asm (#1 p) (fp ^ ".asm");
          print("--------------\n"))
        end)
      progs;
      
  map (fn f => let val fp = "../../test/" ^ f in
         (print ("\n" ^ f ^ ":\n");
          SLP.print (fp ^ ".slp");
          SLP.eval (fp ^ ".slp");
          SLP.compile (fp ^ ".slp") (fp ^ ".vm");
          SLP.asm (fp ^ ".slp") (fp ^ ".asm");
          print("--------------\n"))
        end)
      files;
  ()
end

end (* local *)
end (* Test *)
