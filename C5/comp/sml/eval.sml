(*
**  eval.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 03/15/19.
**  Copyright © 2019 innodocs. All rights reserved.
*)

signature EVAL =
sig
  type env
  val eval : Absyn.stm -> env
end

structure Eval : EVAL =
struct

structure A = Absyn
structure C = Comp
structure PP = PrettyPrint

type env = C.env

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
          if v <> 0 then
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
           while !v <> 0 do let
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
           while !v = 0 do let
             val (tstV, tstEnv) = evalExp tst (evalStm b (!newEnv))
           in
             v := tstV;
             newEnv := tstEnv
           end;
           !newEnv
         end
     | evalStm (A.ForStm(A.SimpleVar(id, _), rangeExp, b, _)) env = let
           val (startExp, endExp) = case rangeExp of
               A.RangeExp(startExp, endExp, _) => (startExp, endExp)
             | _ => raise C.TypeError("range exected")
           val (startV, rangeEnvS) = evalExp startExp env
           val (endV, rangeEnvE) = evalExp endExp rangeEnvS
           val (v, newEnv) = (ref startV, ref ((id, startV) :: rangeEnvE))
         in
           while !v <= endV do (
             v := !v + 1;
             newEnv := (id, !v) :: (evalStm b (!newEnv)));
           !newEnv
         end
     | evalStm (A.AssignStm (A.SimpleVar(id, _), exp, _)) env = let
           val (v, newEnv) = evalExp exp env
         in
           (id, v) :: newEnv
         end
     | evalStm (A.PrintStm (exps, _)) env = let
           val newEnv = case exps of
               hdExp :: tlExps => let
                   val (hdV, hdEnv) = evalExp hdExp env
               in
                 PP.printString (Int.toString hdV);
                 List.foldl
                   (fn (exp, env) => let
                      val (v, expEnv) = evalExp exp env
                    in
                      PP.printString (", " ^ (Int.toString v));
                      expEnv
                    end)
                   hdEnv tlExps
                 end
             | [] => env
         in
           PP.printString "\n";
           newEnv
         end

  and evalExp (A.VarExp(A.SimpleVar(id, _))) env = let
          (*val b = List.find (fn (i, v) => i = id) env*)
          val v = case (List.find (fn (i, v) => i = id) env) of
              SOME (_, v) => v
            | _           => 0
        in
          (v, env)
        end
    | evalExp (A.NumExp(i, _)) env = (i, env)
    | evalExp (A.BinOpExp(exp1, binop, exp2, _)) env = let
          val (v1, env1) = evalExp exp1 env
          val (v2, env2) = evalExp exp2 env1
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
          (v, env2)
        end
    | evalExp (A.UnOpExp(unop, exp, _)) env = let
          val (v1, env1) = evalExp exp env
          val v = case unop of
              A.NotOp  => if v1 <> 0 then 0 else 1
            | A.NegOp  => 0 - v1
        in
          (v, env1)
        end
    | evalExp (A.RangeExp(startExp, endExp, _)) env = (
        raise C.TypeError("bad expression");
        (0, env))

in
  evalStm prog []
end

end (* structure Eval *)
