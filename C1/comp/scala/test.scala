/*
**  test.scala
**  vm-comp
**
**  Created by Ovidiu Podisor on 02/15/20.
**  Copyright © 2020 innodocs. All rights reserved.
*/


object Test {

/*
 local
   open Absyn
 in
*/
import Absyn._

val prog1 = 
  CompoundStm(
    AssignStm(
      "a",
      OpExp(
        NumExp(5),
        Plus,
        NumExp(3))),
    CompoundStm(
      AssignStm(
        "b",
        EseqExp(
          PrintStm(List(
            IdExp("a"),
            OpExp(
              IdExp("a"),
              Minus,
              NumExp(1)))),
          OpExp(
            NumExp(10),
            Times,
            IdExp("a")))),
      PrintStm(List(
        IdExp("b")))));
      

val prog2 =
  CompoundStm(
	  AssignStm(
	    "a",
	    OpExp(
	      OpExp(
	        OpExp(
	          NumExp(1),
	          Plus,
	          NumExp(2)),
	        Plus,
	        OpExp(
	          NumExp(3),
	          Plus,
	          NumExp(4))),
	      Plus,
	      OpExp(
	        OpExp(
	          NumExp(5),
	          Plus,
	          NumExp(6)),
	        Plus,
	        OpExp(
	          NumExp(7),
	          Plus,
	          NumExp(8))))),
	 PrintStm(List(
        IdExp("a"))))


val prog3 =
  CompoundStm(
	  AssignStm(
	    "a",
	    OpExp(
	      OpExp(
	        OpExp(
	          NumExp(1),
	          Plus,
	          NumExp(2)),
	        Plus,
	        OpExp(
	          NumExp(3),
	          Plus,
	          NumExp(4))),
	      Times,
	      OpExp(
	        OpExp(
	          NumExp(5),
	          Plus,
	          NumExp(6)),
	        Plus,
	        OpExp(
	          NumExp(7),
	          Plus,
	          NumExp(8))))),
	 PrintStm(List(
        IdExp("a"))))

        
def run() = {
  val progs = List((prog1, "prog1"), (prog2, "prog2"), (prog3, "prog3"));
  progs.foreach(p => {
      Comp.print(p._1) 
      Comp.eval(p._1)
      Comp.compile(p._1)("../../test/" + p._2 + ".vm")
      println("--------------")
  })
}

def main(args: Array[String]): Unit = {
  run()
}


}
/*
 end
*/