/**
 * absyn.scala
 * vm-comp
 *
 * Created by Ovidiu Podisor on 02/28/20.
 * Copyright © 2020 innodocs. All rights reserved.
 */

/*
 structure Absyn = struct
 */
object Absyn {
  val T = Type

  type Pos = Int
  type Id = Symbol.Ty

  /*
  datatype binop = PlusOp | MinusOp | TimesOp | DivOp | ModOp | PowOp
                | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
                | AndOp | OrOp
  */
  sealed trait BinOp
  case object PlusOp extends BinOp
  case object MinusOp extends BinOp
  case object TimesOp extends BinOp
  case object DivOp extends BinOp
  case object ModOp extends BinOp
  case object PowOp extends BinOp
  case object EqOp extends BinOp
  case object NeqOp extends BinOp
  case object LtOp extends BinOp
  case object LeOp extends BinOp
  case object GtOp extends BinOp
  case object GeOp extends BinOp
  case object AndOp extends BinOp
  case object OrOp extends BinOp

  /*
  datatype unop = NegOp | NotOp
  */
  sealed trait UnOp
  case object NegOp extends UnOp
  case object NotOp extends UnOp

  /*
  datatype var = SimpleVar of id * pos
  */
  sealed abstract class Var
  case class SimpleVar(v: Id, pos: Pos = 0) extends Var

  /*
  and exp = BoolExp of bool * T.ty ref * pos
         | IntExp of int * T.ty ref * pos
         | StringExp of string * T.ty ref * pos
         | VarExp of var * T.ty ref
         | BinOpExp of exp * binop * exp * T.ty ref * pos
         | UnOpExp of unop * exp * T.ty ref * pos
         | RangeExp of exp * exp * T.ty ref * pos
  */
  sealed abstract class Exp
  case class BoolExp(b: Boolean, var ty: T.Ty = T.BOOL, pos: Pos = 0) extends Exp
  case class IntExp(i: Int, var ty: T.Ty = T.INT, pos: Pos = 0) extends Exp
  case class StringExp(s: String, var ty: T.Ty = T.STRING, pos: Pos = 0) extends Exp
  case class VarExp(v: Var, var ty: T.Ty = T.undef) extends Exp
  case class BinOpExp(left: Exp, op: BinOp, right: Exp, var ty: T.Ty = T.undef, pos: Pos = 0) extends Exp
  case class UnOpExp(op: UnOp, right: Exp, var ty: T.Ty = T.undef, pos: Pos = 0) extends Exp
  case class RangeExp(start: Exp, end: Exp, var ty: T.Ty = T.undef, pos: Pos = 0) extends Exp

  /*
  and stm = EmptyStm of pos
         | ExpStm of exp
         | SeqStm of stm list
         | IfStm of exp * stm * stm option * pos
         | WhileStm of exp * stm * pos
         | ForStm of var * exp * stm * pos
         | RepeatStm of exp * stm * pos
         | AssignStm of var * exp * pos
         | PrintStm of exp list * pos
  */
  sealed abstract class Stm
  case class EmptyStm(pos: Pos = 0) extends Stm
  case class ExpStm(exp: Exp) extends Stm
  case class SeqStm(stms: List[Stm]) extends Stm
  case class IfStm(test: Exp, thenB: Stm, elseB: Option[Stm], pos: Pos = 0) extends Stm
  case class WhileStm(test: Exp, body: Stm, pos: Pos = 0) extends Stm
  case class ForStm(v: Var, range: Exp, body: Stm, pos: Pos = 0) extends Stm
  case class RepeatStm(test: Exp, body: Stm, pos: Pos = 0) extends Stm
  case class AssignStm(v: Var, exp: Exp, pos: Pos = 0) extends Stm
  case class PrintStm(exps: List[Exp], pos: Pos = 0) extends Stm


  /**
   *  Absyn type handling
   */
  def getType(exp: Exp): T.Ty =  exp match {
    case BoolExp(_, ty, _)        => ty
    case IntExp(_, ty, _)         => ty
    case StringExp(_, ty, _)      => ty
    case VarExp(_, ty)            => ty
    case BinOpExp(_, _, _, ty, _) => ty
    case UnOpExp(_, _, ty, _)     => ty
    case RangeExp(_, _, ty, _)    => ty
  }

  def setType(exp: Exp, nTy: T.Ty): Unit =  exp match {
    case e@BoolExp(_, ty, _)        => e.ty = nTy
    case e@IntExp(_, ty, _)         => e.ty = nTy
    case e@StringExp(_, ty, _)      => e.ty = nTy
    case e@VarExp(_, ty)            => e.ty = nTy
    case e@BinOpExp(_, _, _, ty, _) => e.ty = nTy
    case e@UnOpExp(_, _, ty, _)     => e.ty = nTy
    case e@RangeExp(_, _, ty, _)    => e.ty = nTy
  }
}
