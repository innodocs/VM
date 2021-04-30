/*
**  Test.scala
**  vm
**
**  Created by Ovidiu Podisor on 02/28/20.
**  Copyright © 2019-2021 innodocs. All rights reserved.
*/

object Test {
/*
local
 open Absyn
in
*/
import Absyn._
val sym = Symbol.symbol _

val prog1 = 
  SeqStm(List(
    AssignStm(
      SimpleVar(sym("a")),
      BinOpExp(
        IntExp(5),
        PlusOp,
        IntExp(3))),
    AssignStm(
      SimpleVar(sym("b")),
      BinOpExp(
        IntExp(10),
        TimesOp,
        VarExp(SimpleVar(sym("a"))))),
    PrintStm(List(
      VarExp(SimpleVar(sym("a"))),
      BinOpExp(
        VarExp(SimpleVar(sym("a"))),
        MinusOp,
        IntExp(1)),
      BinOpExp(
        IntExp(10),
        TimesOp,
        VarExp(SimpleVar(sym("a")))))),
    PrintStm(List(
        VarExp(SimpleVar(sym("b")))))))

val prog2 =
  SeqStm(List(
	  AssignStm(
	    SimpleVar(sym("a")),
	    BinOpExp(
	      BinOpExp(
	        BinOpExp(
	          IntExp(1),
	          PlusOp,
	          IntExp(2)),
	        PlusOp,
	        BinOpExp(
	          IntExp(3),
	          PlusOp,
	          IntExp(4))),
	      PlusOp,
	      BinOpExp(
	        BinOpExp(
	          IntExp(5),
	          PlusOp,
	          IntExp(6)),
	        PlusOp,
	        BinOpExp(
	          IntExp(7),
	          PlusOp,
	          IntExp(8))))),
    PrintStm(List(
      VarExp(SimpleVar(sym("a")))))))

val prog3 =
  SeqStm(List(
	  AssignStm(
	    SimpleVar(sym("a")),
	    BinOpExp(
	      BinOpExp(
	        BinOpExp(
	          IntExp(1),
	          PlusOp,
	          IntExp(2)),
	        PlusOp,
	        BinOpExp(
	          IntExp(3),
	          PlusOp,
	          IntExp(4))),
	      TimesOp,
	      BinOpExp(
	        BinOpExp(
	          IntExp(5),
	          PlusOp,
	          IntExp(6)),
	        PlusOp,
	        BinOpExp(
	          IntExp(7),
	          PlusOp,
	          IntExp(8))))),
    PrintStm(List(
      VarExp(SimpleVar(sym("a")))))))
        
val prog4 = 
  SeqStm(List(
    AssignStm(
      SimpleVar(sym("a")),
      BinOpExp(
        IntExp(5),
        PlusOp,
        IntExp(3))),
    AssignStm(
      SimpleVar(sym("b")),
      BinOpExp(
        IntExp(4),
        MinusOp,
        IntExp(2))),
    IfStm(
      BinOpExp(
        VarExp(SimpleVar(sym("a"))),
        LtOp,
        VarExp(SimpleVar(sym("b")))),
      SeqStm(List(
        PrintStm(List(
          VarExp(SimpleVar(sym("a"))))))),
      Some(SeqStm(List(
        PrintStm(List(
          VarExp(SimpleVar(sym("b")))))))))))


def run() = {

  val progs = List((prog1, "prog-01"), (prog2, "prog-02"), (prog3, "prog-03"), (prog4, "prog-04"));
  progs.foreach(p => {
      val fp = "../../test/gen/" + p._2
      println("\n" + p._2 + ":"); PrettyPrint.print(p._1)
      println("\n>>> eval:\n");   Eval.eval(p._1)
      Comp.compile(p._1)(fp + "-scala.vm")
      Comp.asm(p._1)(fp + "-scala.asm")
      println("--------------")
  })

  val files = List("test-01", "test-02", "test-03", "test-04", "test-05", "test-06");
  files.foreach(f => {
    val fp = "../../test/gap/" + f
    println("\n" + f + ":");  GAP.print(fp + ".gap")
    println("\n>>> eval:\n"); GAP.eval(fp + ".gap")
    GAP.compile(fp + ".gap") (fp + "-scala.vm")
    GAP.asm(fp + ".gap") (fp + "-scala.asm")
    println("--------------")
  })
}

def main(args: Array[String]): Unit = {
  run()
}

/* end (* structure Test *) */
}
