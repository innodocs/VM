
/*
**  eval.scala
**  vm-comp
**
**  Created by Ovidiu Podisor on 03/15/20.
**  Copyright © 2020 innodocs. All rights reserved.
*/

/*
signature EVAL =
sig
  type env
  val eval : Absyn.stm -> env
end
*/
sealed trait EVAL_SIG {
  type Env
  val eval: Absyn.Stm => Env
}

//structure Eval : EVAL
object Eval extends EVAL_SIG {
  sealed abstract class Obj
  type Env = Symbol.Table[Obj]

  val eval = EvalImp.eval
}
//= struct
private object EvalImp extends EVAL_SIG {

val A = Absyn
val PP = PrettyPrint

type Env = Eval.Env

// exception TypeError = T.Error
val TypeError = Type.Error

/*
datatype obj = B of bool
             | I of int
             | S of string
*/
type Obj = Eval.Obj
case class B(b: Boolean) extends Eval.Obj
case class I(i: Int)     extends Eval.Obj
case class S(s: String)  extends Eval.Obj

def boolVal (v: Obj, p: Int): Boolean = v match {
  case B(b) => b
  case _ => throw TypeError("eval: Boolean expected", p)
}
def intVal (v: Obj, p: Int): Int = v match {
  case I(i) => i
  case _ => throw TypeError("eval: Int expected", p)
}
def stringVal (v: Obj, p: Int): String = v match {
  case S(s) => s
  case _ => throw TypeError("eval: String expected", p)
}
def prVal(v: Obj): String = v match {
  case B(b) => if (b) "true" else "false"
  case I(i) => i.toString
  case S(s) => s
}

/**
 * evaluate a statement
 */
val eval : Absyn.Stm => Eval.Env = prog => {

  def evalStm(stm: A.Stm, env: Eval.Env): Eval.Env = stm match {
    case A.SeqStm(stms) =>
      stms.foldLeft(env)((env, stm) => evalStm(stm, env))

    case A.EmptyStm(_)  => env

    case A.ExpStm(exp)  => evalExp(exp, env)._2

    case A.IfStm(tst, t, eo, p) => {
      val (v, newEnv) = evalExp(tst, env)

      if (intVal(v, p) != 0)
        evalStm(t, newEnv)
      else eo match {
        case Some(e) => evalStm(e, newEnv)
        case _ => newEnv
      }
    }

    case A.WhileStm(tst, b, p) => {
      var (v, newEnv) = evalExp(tst, env)
      while (intVal(v, p) != 0) {
        val (tstV, tstEnv) = evalExp(tst, evalStm(b, newEnv))
        v = tstV; newEnv = tstEnv
      }
      newEnv
    }

    case A.RepeatStm(tst, b, p) => {
      var (v: Obj, newEnv) = (I(0), env)
      do {
        val (tstV, tstEnv) = evalExp(tst, evalStm(b, newEnv))
        v = tstV; newEnv = tstEnv
      } while (intVal(v, p) == 0)
      newEnv
    }

    case A.ForStm(A.SimpleVar(id, _), rangeExp, b, p) => {
      val (startExp, endExp) = rangeExp match {
        case A.RangeExp(startExp, endExp, _, _) => (startExp, endExp)
        case _ => throw TypeError("range exected", p)
      }
      val (startV, rangeEnvS) = evalExp(startExp, env)
      val (endV, rangeEnvE) = evalExp(endExp, rangeEnvS)
      val ev = intVal(endV, p)
      var (v, newEnv) = (startV, rangeEnvE.enter(id, startV))
      while (intVal(v, p) <= ev) {
        v = I(intVal(v, p) + 1)
        newEnv = evalStm(b, newEnv).enter(id, v)
      }
      newEnv
    }

    case A.AssignStm(A.SimpleVar(id, _), exp, _) => {
      val (v, newEnv) = evalExp(exp, env)
      newEnv.enter(id, v)
    }

    case A.PrintStm(exps, _) => {
      if (exps.exists(exp => A.getType(exp) == Type.STRING)) {
        exps.foldLeft(env)((env, exp) => {
          val (v, expEnv) = evalExp(exp, env)
          PP.printString(prVal(v))
          expEnv
        })
      }
      else {
        val newEnv = exps match {
          case hdExp :: tlExps => {
            val (hdV, hdEnv) = evalExp(hdExp, env)
            PP.printString(prVal(hdV))
            tlExps.foldLeft(hdEnv)((env, exp) => {
              val (v, expEnv) = evalExp(exp, env)
              PP.printString(", " + prVal(v));
              expEnv
            })
          }
          case _ => env
        }
        PP.printString("\n")
        newEnv
      }
    }
  }


  def evalExp(exp: A.Exp, env: Eval.Env): (Obj, Env) = exp match {

    case A.VarExp(A.SimpleVar(id, _), _) => {
      env.lookup(id) match {
        case Some(v) => (v, env)
        case None => (I(0), env) /* throw "not found" ?? */
      }
    }
    case A.BoolExp(b, _, _)  => (B(b), env)
    case A.IntExp(i, _, _)   => (I(i), env)
    case A.StringExp(s, _, _)=> (S(s), env)

    case A.BinOpExp(exp1, binop, exp2, _, p) => {
      val (o1, env1) = evalExp(exp1, env)
      val v1 = intVal(o1, p)
      val (o2, env2) = evalExp(exp2, env1)
      val v2 = intVal(o2, p)
      val v = binop match {
        case A.PlusOp  => v1 + v2
        case A.MinusOp => v1 - v2
        case A.TimesOp => v1 * v2
        case A.DivOp   => v1 / v2
        case A.ModOp   => v1 % v2
        case A.PowOp   => Math.pow(v1, v2).toInt
        case A.EqOp    => if (v1 == v2) 1 else 0
        case A.NeqOp   => if (v1 != v2) 1 else 0
        case A.LtOp    => if (v1 <  v2) 1 else 0
        case A.LeOp    => if (v1 <= v2) 1 else 0
        case A.GtOp    => if (v1 >  v2) 1 else 0
        case A.GeOp    => if (v1 >= v2) 1 else 0
        case A.AndOp   => if (v1 != 0 && v2 != 0) 1 else 0
        case A.OrOp    => if (v1 != 0 || v2 != 0) 1 else 0
      }

      (I(v), env2)
    }

    case A.UnOpExp(unop, exp, _, p) => {
      val (o1, env1) = evalExp(exp, env)
      val v1 = intVal(o1, p)
      val v = unop match {
        case A.NotOp  => if (v1 != 0) 0 else 1
        case A.NegOp  => -v1
      }

      (I(v), env1)
    }

    case A.RangeExp(startExp, endExp, _, p) => {
      throw TypeError("bad expression", p)
      (I(0), env)
    }
  }

  // in
  evalStm(prog, Symbol.Table[Obj]())
  // end
}

//end (* structure Eval *)
}
