/*
**  GAP.scala
**  vm-comp
**
**  Created by Ovidiu Podisor on 03/15/20.
**  Copyright © 2019-2021 innodocs. All rights reserved.
*/

import java.io.{IOException, PrintStream}

/*
structure GAP: sig
     val print   : string -> unit
     val eval    : string -> (Absyn.id * int) list
     val compile : string -> string -> unit
     val asm     : string -> string -> unit
  end = struct
*/

sealed trait GAP_SIG {
   val print   : String => Unit
   val eval    : String => Unit
   val compile : String => String => Unit
   val asm     : String => String => Unit
}

object GAP extends GAP_SIG {

  val A  = Absyn
  val T  = Type
  val P  = Parser
  val C  = Comp
  val PP = PrettyPrint
  val E  = Eval
  
  override val print : String => Unit = gapFileName => {
    val ast = P.parse(gapFileName)
    PP.print(ast)
  }
 
  override val eval : String => Unit = gapFileName => {
    val ast = P.parse(gapFileName)
    E.eval(ast)
  }  
 
  override val compile : String => String => Unit = gapFileName => {vmFileName => {
    try {
      val ast = P.parse(gapFileName)
      C.compile(ast)(vmFileName)
    } catch {
      case e: T.Error => e match { case T.Error(s, p) => System.err.println(s) }
    }
  }}
  
  override val asm : String => String => Unit = gapFileName => { asmFileName => {
    try {
      val ast = P.parse(gapFileName)
      C.asm(ast)(asmFileName)
    } catch {
      case e: T.Error => e match { case T.Error(s, p) => System.err.println(s) }
    }
  }}
  
	private def error(msg: String): Unit = {
		System.err.println("\n" + msg + "\n");
		System.exit(1);
	}

	def main(args: Array[String]): Unit = {

		if (args.length < 2)
			error("\tusage: vm-comp <gap file> <vm file>");
		val inFileName = args(0);
		val outFileName = args(1);
		
		compile(inFileName)(outFileName);
	}

/*end (* structure GAP *) */
}

