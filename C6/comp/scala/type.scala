/**
 * type.scala
 * vm-comp
 *
 * Created by Ovidiu Podisor on 04/05/20.
 * Copyright © 2019-2021 innodocs. All rights reserved.
 */

/*
structure Type = struct
*/
object Type {
  /*
  datatype ty = ANY
              | ANYVAL
              | BOOL
              | INT
              | ANYREF
              | STRING
              | ARRAY of ty
              | RECORD of (Symbol.symbol * ty) list
              | NULL
              | NOTHING
              | META of ty ref
  */
  sealed trait Ty
  case object ANY    extends Ty
  case object ANYVAL extends Ty
  case object BOOL   extends Ty
  case object INT    extends Ty
  case object ANYREF extends Ty
  case object STRING extends Ty
  case class  ARRAY(ty: Ty) extends Ty
  case class  RECORD(stl: List[(String, Ty)]) extends Ty
  case object NULL extends Ty
  case object NOTHING extends Ty
  case class  META(var ty: Ty) extends Ty

  case class Error(s: String, pos: Absyn.Pos = 0) extends Exception(s)

  def isValType(ty: Ty): Boolean = ty match {
    case ANYVAL => true
    case BOOL   => true
    case INT    => true
    case _      => false
  }

  def isRefType(ty: Ty): Boolean = ty match {
    case ANYREF    => true
    case NULL      => true
    case STRING    => true
    case ARRAY(_)  => true
    case RECORD(_) => true
    case _         => false
  }

  def undef: Ty = META(ANY)

  def realTy(ty: Ty): Ty = ty match {
    case META(ty) => realTy(ty)
    case _ => ty
  }

  def eq(ty1: Ty, ty2: Ty): Boolean = (ty1, ty2) match {
    case (RECORD(l1), RECORD(l2)) =>
      l1.forall (re1 => re1 match { case (s1, t1) => {
        l2.find(re2 => re2 match { case (s2, t2) => s1 == s2}) match {
          case Some((s2, t2)) => eq(t1, t2)
          case None => false
        }
      }})
    case _ => ty1 == ty2
  }

  def unify(t1: Ty, t2: Ty): Ty = (t1, t2) match {
    case (m1@META(t1), m2@META(t2)) => {
      val ut = if (t1 == ANY) t2
               else if (t2 == ANY) t1
               else unify(t1, t2);
      m1.ty = ut;
      m2.ty = ut;
      ut;
    }
    case (m1@META(t1), t2) => { m1.ty = t2; t2 }
    case (t1, m2@META(t2)) => { m2.ty = t1; t1 }
    case (ANY, t2) => t2
    case (t1, ANY) => t1
    case (ANYVAL, t2) => if (isValType(t2)) t2 else NOTHING
    case (t1, ANYVAL) => if (isValType(t1)) t1 else NOTHING
    case (ANYREF, t2) => if (isRefType(t2)) t2 else NOTHING
    case (t1, ANYREF) => if (isRefType(t1)) t1 else NOTHING
    case (ARRAY(t1), ARRAY(t2)) => ARRAY(unify(t2, t2))
    case (RECORD(t1), RECORD(t2)) => RECORD(t1) /*!!!*/
    case (t1, t2) => if (t1 == t2) t1 else NOTHING
  }

  /**
   *  debug
   */
  def prUTy(loc: String, e1Ty: Ty, e2Ty: Ty, uTy: Ty): Unit =
    println(s"$loc::unify($e1Ty, $e2Ty) = $uTy")
  /**/

/*
end (* structure Type *)
*/
}
