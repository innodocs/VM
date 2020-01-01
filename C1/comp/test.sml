(*
**  test.sml
**  vm
**
**  Created by Ovidiu Podisor on 01/30/19.
**  Copyright © 2019 innodocs. All rights reserved.
*)

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

end
