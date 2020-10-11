/**
 *  absyn.scala
 *  vm-comp
 *
 *  Created by Ovidiu Podisor on 02/28/20.
 *  Copyright © 2020 innodocs. All rights reserved.
 */

/*
 structure Absyn = struct
 */
object Absyn { 

  type Pos = Int
	type Id = String
	
	/*
	 datatype binop = PlusOp | MinusOp | TimesOp | DivOp | ModOp | PowOp
	                | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
	                | AndOp | OrOp
	 */
  sealed trait BinOp
  case object PlusOp  extends BinOp
  case object MinusOp extends BinOp
  case object TimesOp extends BinOp
  case object DivOp   extends BinOp
  case object ModOp   extends BinOp
  case object PowOp   extends BinOp
  case object EqOp    extends BinOp
  case object NeqOp   extends BinOp
  case object LtOp    extends BinOp
  case object LeOp    extends BinOp
  case object GtOp    extends BinOp
  case object GeOp    extends BinOp
  case object AndOp   extends BinOp
  case object OrOp    extends BinOp

	/*
	 datatype unop = NegOp | NotOp
	 */
  sealed trait UnOp
  case object NegOp   extends UnOp
  case object NotOp   extends UnOp
  
	/*
	 datatype var = SimpleVar of id * pos
  */
	sealed abstract class Var
  case class SimpleVar(v: Id, pos: Pos=0) extends Var
  
  /*
	 and exp = VarExp of var
           | NumExp of int * pos
  	       | BinOpExp of exp * binop * exp * pos
  	       | UnOpExp of exp * unop * exp * pos
  */
  sealed abstract class Exp
  case class VarExp(v: Var) extends Exp
  case class NumExp(i: Int, pos: Pos=0) extends Exp
  case class BinOpExp(left: Exp, op: BinOp, right: Exp, pos: Pos=0) extends Exp
  case class UnOpExp(op: UnOp, right: Exp, pos: Pos=0) extends Exp
   
  /*
	 and stm = EmptyStm of pos
           | ExpStm of exp
	         | SeqStm of stm list
	         | IfStm of exp * stm * stm option * pos
	         | WhileStm of exp * stm * pos
           | AssignStm of var * exp * pos
           | PrintStm of exp list * pos
  */
	sealed abstract class Stm
  case class EmptyStm(pos: Pos = 0) extends Stm
  case class ExpStm(exp: Exp) extends Stm
  case class SeqStm(stms: List[Stm]) extends Stm
  case class IfStm(test: Exp, thenB: Stm, elseB: Option[Stm], pos: Pos=0) extends Stm
  case class WhileStm(test: Exp, body: Stm, pos: Pos=0) extends Stm
  case class AssignStm(v: Var, exp: Exp, pos: Pos=0) extends Stm  
  case class PrintStm(exps: List[Exp], pos: Pos=0) extends Stm
}
