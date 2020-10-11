/*
**  absyn.scala
**  vm
**
**  Created by Ovidiu Podisor on 02/15/20.
**  Copyright © 2020 innodocs. All rights reserved.
*/

/*
 structure Absyn = struct
 */
object Absyn { 

	type Id = String
	
	/*
	 datatype binop = Plus | Minus | Times | Div
	 */
  sealed trait BinOp
  case object Plus  extends BinOp
  case object Minus extends BinOp
  case object Times extends BinOp
  case object Div   extends BinOp
  
	/*
	 datatype stm = CompoundStm of stm * stm
    	          | AssignStm of id * exp
        	      | PrintStm of exp list
  */
	sealed abstract class Stm
  case class CompoundStm(s1: Stm, s2: Stm) extends Stm
  case class AssignStm(id: Id, exp: Exp) extends Stm
  case class PrintStm(es: List[Exp]) extends Stm
	
  /*
	 and exp = IdExp of id
           | NumExp of int
  	       | OpExp of exp * binop * exp
    	     | EseqExp of stm * exp
   */
   sealed abstract class Exp
   case class IdExp(id: Id) extends Exp
   case class NumExp(i: Int) extends Exp
   case class OpExp(e1: Exp, op: BinOp, e2: Exp) extends Exp
   case class EseqExp(stm: Stm, exp: Exp) extends Exp
}
