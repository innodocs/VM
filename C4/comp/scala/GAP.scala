/*
**  GAP.scala
**  vm-comp
**
**  Created by Ovidiu Podisor on 02/28/20.
**  Copyright © 2020 innodocs. All rights reserved.
*/

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

  val A = Absyn
  val P = Parser
  val C = Comp
  
  override val print : String => Unit = gapFileName => {
    val ast = P.parse(gapFileName)
    C.print(ast)
  }
 
  override val eval : String => Unit = gapFileName => {
    val ast = P.parse(gapFileName)
    C.eval(ast)
  }  
 
  override val compile : String => String => Unit = gapFileName => {vmFileName => {
    val ast = P.parse(gapFileName)
    C.compile(ast)(vmFileName)
  }}
  
  override val asm : String => String => Unit = gapFileName => { asmFileName => {
    val ast = P.parse(gapFileName)
    C.asm(ast)(asmFileName)
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

/*end (* structure GAP *)*/
}

