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

/*
structure Eval : EVAL
*/
object Eval extends EVAL_SIG {
  type Env = List[(Absyn.Id, Int)]
  val eval = EvalImp.eval
}

/*
 * = struct
 */
private object EvalImp extends EVAL_SIG {

/*
structure A = Absyn
structure C = Comp
structure PP = PrettyPrint
*/
val A = Absyn
val C = Comp
val PP = PrettyPrint

type Env = Eval.Env

/**
 * evaluate a statement
 */
val eval : Absyn.Stm => Eval.Env = prog => {

  /* let */
  def evalStm(stm: A.Stm, env: Env): Env = stm match {
    case A.SeqStm(stms) =>
      stms.foldLeft(env)((env, stm) => evalStm(stm, env))

    case A.EmptyStm(_)  => env

    case A.ExpStm(exp)  => evalExp(exp, env)._2

    case A.IfStm(tst, t, eo, _) => {
      val (v, newEnv) = evalExp(tst, env)

      if (v != 0)
        evalStm(t, newEnv)
      else eo match {
        case Some(e) => evalStm(e, newEnv)
        case _ => newEnv
      }
    }

    case A.WhileStm(tst, b, _) => {
      var (v, newEnv) = evalExp(tst, env)
      while (v != 0) {
        val (tstV, tstEnv) = evalExp(tst, evalStm(b, newEnv))
        v = tstV; newEnv = tstEnv
      }
      newEnv
    }

    case A.ForStm(A.SimpleVar(id, _), rangeExp, b, _) => {
      val (startExp, endExp) = rangeExp match {
        case A.RangeExp(startExp, endExp, _) => (startExp, endExp)
        case _ => throw C.TypeError("range exected")
      }
      val (start, rangeEnvS) = evalExp(startExp, env)
      val (end, rangeEnvE) = evalExp(endExp, rangeEnvS)
      var (v, newEnv) = (start, (id, start) :: rangeEnvE)
      while (v <= end) {
        v = v + 1
        newEnv = (id, v) :: evalStm(b, newEnv)
      }
      newEnv
    }

    case A.RepeatStm(tst, b, _) => {
      var (v, newEnv) = (0, env)
      do {
        val (tstV, tstEnv) = evalExp(tst, evalStm(b, newEnv))
        v = tstV; newEnv = tstEnv
      } while (v == 0)
      newEnv
    }

    case A.AssignStm(A.SimpleVar(id, _), exp, _) => {
      val (v, newEnv) = evalExp(exp, env)
      (id, v) :: newEnv
    }

    case A.PrintStm(exps, _) => {
      val newEnv = exps match {
        case hdExp :: tlExps => {
          val (hdV, hdEnv) = evalExp(hdExp, env)
          PP.printString(hdV.toString)
          tlExps.foldLeft(hdEnv)((env, exp) => {
            val (v, expEnv) = evalExp(exp, env)
            PP.printString(", " + v.toString);
            expEnv
          })
        }
        case _ => env
      }
      PP.printString("\n")
      newEnv
    }
  }

  def evalExp(exp: A.Exp, env: Env): (Int, Env) = exp match {

    case A.VarExp(A.SimpleVar(id, _)) => {
      env.find((e) => e._1 == id) match {
        case Some((_, v)) => (v, env)
        case None => (0, env) /* throw "not found" ?? */
      }
    }

    case A.NumExp(i, _) => (i, env)

    case A.BinOpExp(exp1, binop, exp2, _) => {
      val (v1, env1) = evalExp(exp1, env)
      val (v2, env2) = evalExp(exp2, env1)
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

      (v, env2)
    }

    case A.UnOpExp(unop, exp, _) => {
      val (v1, env1) = evalExp(exp, env)
      val v = unop match {
        case A.NotOp  => if (v1 != 0) 0 else 1
        case A.NegOp  => -v1
      }

      (v, env1)
    }

    case A.RangeExp(startExp, endExp, _) => {
      throw C.TypeError("bad expression")
      (0, env)
    }
  }

  /* in */
  evalStm(prog, Nil)
  /* end */
}

/*
end (* structure Eval *)
*/
}
