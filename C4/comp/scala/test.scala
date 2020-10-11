/*
**  Test.scala
**  vm
**
**  Created by Ovidiu Podisor on 02/28/20.
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
  SeqStm(List(
    AssignStm(
      SimpleVar("a"),
      BinOpExp(
        NumExp(5),
        PlusOp,
        NumExp(3))),
    AssignStm(
      SimpleVar("b"),
      BinOpExp(
        NumExp(10),
        TimesOp,
        VarExp(SimpleVar("a")))),
    PrintStm(List(
      VarExp(SimpleVar("a")),
      BinOpExp(
        VarExp(SimpleVar("a")),
        MinusOp,
        NumExp(1)),
      BinOpExp(
        NumExp(10),
        TimesOp,
        VarExp(SimpleVar("a"))))),
    PrintStm(List(
        VarExp(SimpleVar("b"))))))

val prog2 =
  SeqStm(List(
	  AssignStm(
	    SimpleVar("a"),
	    BinOpExp(
	      BinOpExp(
	        BinOpExp(
	          NumExp(1),
	          PlusOp,
	          NumExp(2)),
	        PlusOp,
	        BinOpExp(
	          NumExp(3),
	          PlusOp,
	          NumExp(4))),
	      PlusOp,
	      BinOpExp(
	        BinOpExp(
	          NumExp(5),
	          PlusOp,
	          NumExp(6)),
	        PlusOp,
	        BinOpExp(
	          NumExp(7),
	          PlusOp,
	          NumExp(8))))),
    PrintStm(List(
      VarExp(SimpleVar("a"))))))

val prog3 =
  SeqStm(List(
	  AssignStm(
	    SimpleVar("a"),
	    BinOpExp(
	      BinOpExp(
	        BinOpExp(
	          NumExp(1),
	          PlusOp,
	          NumExp(2)),
	        PlusOp,
	        BinOpExp(
	          NumExp(3),
	          PlusOp,
	          NumExp(4))),
	      TimesOp,
	      BinOpExp(
	        BinOpExp(
	          NumExp(5),
	          PlusOp,
	          NumExp(6)),
	        PlusOp,
	        BinOpExp(
	          NumExp(7),
	          PlusOp,
	          NumExp(8))))),
    PrintStm(List(
      VarExp(SimpleVar("a"))))))
        
val prog4 = 
  SeqStm(List(
    AssignStm(
      SimpleVar("a"),
      BinOpExp(
        NumExp(5),
        PlusOp,
        NumExp(3))),
    AssignStm(
      SimpleVar("b"),
      BinOpExp(
        NumExp(4),
        MinusOp,
        NumExp(2))),
    IfStm(
      BinOpExp(
        VarExp(SimpleVar("a")),
        LtOp,
        VarExp(SimpleVar("b"))),
      SeqStm(List(
        PrintStm(List(
          VarExp(SimpleVar("a")))))),
      Some(SeqStm(List(
        PrintStm(List(
          VarExp(SimpleVar("b"))))))))))

def run() = {
  val progs = List((prog1, "prog1"), (prog2, "prog2"),
                (prog3, "prog3"), (prog4, "prog4"));
  progs.foreach(p => {
      val fp = "../../test/" + p._2
      println("\n" + p._2 + ":"); Comp.print(p._1) 
      println("\n>>> eval:\n");     Comp.eval(p._1)
      Comp.compile(p._1)(fp + ".vm")
      Comp.asm(p._1)(fp + ".asm")
      println("--------------")
  })
  
  val files = List("test1", "test2", "test3", "test4");
  files.foreach(f => {
    val fp = "../../test/" + f
    println("\n" + f + ":"); GAP.print(fp + ".gap")
    println("\n>>> eval:\n");  GAP.eval(fp + ".gap")
    GAP.compile(fp + ".gap") (fp + ".vm")
    GAP.asm(fp + ".gap") (fp + ".asm")
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