(*
**  eval.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 03/15/19.
**  Copyright © 2019-2021 innodocs. All rights reserved.
*)

signature EVAL =
sig
  type env
  val eval : Absyn.stm -> env
end

structure Eval : EVAL =
struct

structure A = Absyn
structure T = Type
structure S = Symbol
structure C = Comp
structure PP = PrettyPrint

exception TypeError = T.Error

datatype obj = B of bool
             | I of int
             | S of string

fun boolVal (B v) = v
  | boolVal _ = raise TypeError("eval: Bool expected")
fun intVal (I v) = v
  | intVal _ = raise TypeError("eval: Int expected")
fun stringVal (S v) = v
  | stringVal _ = raise TypeError("eval: String expected")

fun prVal (B v) = if v then "true" else "false"
  | prVal (I v) = Int.toString v
  | prVal (S v) = v

type env = obj S.table

(**
 * evaluate a statement
 *)
fun eval prog = let

  fun evalStm (A.SeqStm stms) env =  List.foldl (fn (stm, env) => evalStm stm env) env stms
    | evalStm (A.EmptyStm _) env = env
    | evalStm (A.ExpStm exp) env = #2 (evalExp exp env)
    | evalStm (A.IfStm (tst, t, eo, _)) env = let
           val (v, newEnv) = evalExp tst env
        in
          if intVal(v) <> 0 then
            evalStm t newEnv
          else
            case eo of
              SOME(e) => evalStm e newEnv
            | _ => newEnv
         end
     | evalStm (A.WhileStm (tst, b, _)) env = let
           val (tstV, tstEnv) = evalExp tst env
           val (v, newEnv) = (ref tstV, ref tstEnv)
         in
           while intVal(!v) <> 0 do let
             val (tstV, tstEnv) = evalExp tst (evalStm b (!newEnv))
           in
             v := tstV;
             newEnv := tstEnv
           end;
           !newEnv
         end
     | evalStm (A.RepeatStm (tst, b, _)) env = let
           val (tstV, tstEnv) = evalExp tst (evalStm b env)
           val (v, newEnv) = (ref tstV, ref tstEnv)
         in
           while intVal(!v) = 0 do let
             val (tstV, tstEnv) = evalExp tst (evalStm b (!newEnv))
           in
             v := tstV;
             newEnv := tstEnv
           end;
           !newEnv
         end
     | evalStm (A.ForStm(A.SimpleVar(id, _), rangeExp, b, _)) env = let
           val (startExp, endExp) = case rangeExp of
               A.RangeExp(startExp, endExp, _, _) => (startExp, endExp)
             | _ => raise TypeError("range exected")
           val (startV, rangeEnvS) = evalExp startExp env
           val (endV, rangeEnvE) = evalExp endExp rangeEnvS
           val (v, newEnv) = (ref startV, ref (S.enter(rangeEnvE, id, startV)))
         in
           while intVal(!v) <= intVal(endV) do (
             v := I(intVal(!v) + 1);
             newEnv := S.enter (evalStm b (!newEnv), id, !v));
           !newEnv
         end
     | evalStm (A.AssignStm (A.SimpleVar(id, _), exp, _)) env = let
           val (v, newEnv) = evalExp exp env
         in
           S.enter (env, id, v)
         end
     | evalStm (A.PrintStm (exps, _)) env = let
           val newEnv = case exps of
             hdExp :: tlExps => let
                 val (hdV, hdEnv) = evalExp hdExp env
               in
                 PP.printString (prVal hdV);
                 List.foldl
                   (fn (exp, env) => let
                      val (v, expEnv) = evalExp exp env
                    in
                      PP.printString (prVal v);
                      expEnv
                    end)
                   hdEnv tlExps
                 end
             | [] => env
         in
           newEnv
         end

  and evalExp (A.VarExp(A.SimpleVar(id, _), _)) env = let
          val v = case S.lookup(env, id) of
              SOME v => v
            | _      => I(0)
        in
          (v, env)
        end
    | evalExp (A.IntExp(i, _, _)) env = (I(i), env)
    | evalExp (A.StringExp(s, _, _)) env = (S(s), env)
    | evalExp (A.BinOpExp(exp1, binop, exp2, _, _)) env = let
          val (o1, env1) = evalExp exp1 env
          val v1 = intVal o1
          val (o2, env2) = evalExp exp2 env1
          val v2 = intVal o2
          val v = case binop of
              A.PlusOp  => v1 + v2
            | A.MinusOp => v1 - v2
            | A.TimesOp => v1 * v2
            | A.DivOp   => v1 div v2
            | A.ModOp   => v1 mod v2
            | A.PowOp   => Int.fromLarge (IntInf.pow(Int.toLarge v1, v2))
            | A.EqOp    => if v1 =  v2 then 1 else 0
            | A.NeqOp   => if v1 <> v2 then 1 else 0
            | A.LtOp    => if v1 <  v2 then 1 else 0
            | A.LeOp    => if v1 <= v2 then 1 else 0
            | A.GtOp    => if v1 >  v2 then 1 else 0
            | A.GeOp    => if v1 >= v2 then 1 else 0
            | A.AndOp   => if v1 <> 0 andalso v2 <> 0 then 1 else 0
            | A.OrOp    => if v1 <> 0 orelse  v2 <> 0 then 1 else 0
        in
          (I(v), env2)
        end
    | evalExp (A.UnOpExp(unop, exp, _, _)) env = let
          val (o1, env1) = evalExp exp env
          val v1 = intVal o1
          val v = case unop of
              A.NotOp  => if v1 <> 0 then 0 else 1
            | A.NegOp  => 0 - v1
        in
          (I(v), env1)
        end
    | evalExp (A.RangeExp(startExp, endExp, _, _)) env = (
        raise TypeError("bad expression");
        (I(0), env))

in
  evalStm prog S.empty
end

end (* structure Eval *)
