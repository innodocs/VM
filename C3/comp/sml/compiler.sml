(*
**  compiler.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 01/30/19.
**  Copyright © 2019 innodocs. All rights reserved.
*)

structure Comp : sig
     val print   : Absyn.stm -> unit
     val eval    : Absyn.stm -> (Absyn.id * int) list
     val compile : Absyn.stm -> string -> unit
     val asm     : Absyn.stm -> string -> unit
  end = struct

open Absyn
open Instr

type env = (id * int) list

fun lookup id (env: env) = case env of
  		    (i, v) :: tl => if id = i then v else lookup id tl
  		  | []           => ~1
  
val lastId = ref (~1)
fun newId() = (lastId := !lastId + 1; !lastId)

fun resetEnv() = lastId := ~1

fun printString (s: string) = print s
fun printInt i = printString (Int.toString(i))
fun printNL () = printString "\n"


(*
 * print a statement
 *)
fun print stm = let
     
  fun printStm (CompoundStm(stm1, stm2)) = (printStm stm1; printString ";\n"; printStm stm2)
    | printStm (AssignStm(id, exp))      = (printString id; printString " := "; printExp exp)
    | printStm (PrintStm exps)           = (printString "print("; printExps exps; printString ")")
       
  and printExps (e :: tl) = (printExp e; List.app (fn e => (printString ","; printExp e)) tl)
    | printExps []        = ()   
       
  and printExp exp = let
    fun openParen  prec opPrec = if prec > opPrec then printString "(" else ()
    fun closeParen prec opPrec = if prec > opPrec then printString ")" else ()
    
    fun print prec (IdExp id)  = printString id
      | print prec (NumExp num) = printInt num
      | print prec (OpExp(exp1, Plus, exp2)) = (openParen prec 0; print 0 exp1;
                                        printString "+"; print 0 exp2; closeParen prec 0)
      | print prec (OpExp(exp1, Minus, exp2)) = (openParen prec 1; print 1 exp1;
                                        printString "-"; print 1 exp2; closeParen prec 1)
      | print prec (OpExp(exp1, Times, exp2)) = (openParen prec 2; print 2 exp1;
                                        printString "*"; print 2 exp2; closeParen prec 2)
      | print prec (OpExp(exp1, Div, exp2)) = (openParen prec 3; print 3 exp1;
                                        printString "/"; print 3 exp2; closeParen prec 3)
      | print prec (EseqExp(stm, exp)) = (printString "("; printStm stm; printString ", ";
                                        printExp exp; printString ")")
  in
    print 0 exp
  end

in
  printStm stm;
  printNL()
end

(*
 * evaluate a statement
 *)
fun eval prog = let

  fun evalStm (CompoundStm(stm1, stm2), env) = evalStm(stm2, evalStm(stm1, env))
    | evalStm (AssignStm(id, exp), env) = let
           val (v, newEnv) = evalExp(exp, env)
        in
           (id, v)::newEnv
        end
    | evalStm (PrintStm(exps), env) = List.foldl
                    (fn (exp, env) => let
                       val (v, newEnv) = evalExp(exp, env)
                     in
                       printString (Int.toString(v) ^ "\n");
                       newEnv
                     end) env exps

  and evalExp (IdExp(id), env) = let
         val b = List.find (fn (i, v) => i = id) env
         val v = case b of
            SOME (_, v) => v
          | _           => 0
       in
          (v, env)
       end
    | evalExp (NumExp(i), env) = (i, env)
    | evalExp (OpExp(e1, binop, e2), env) = let
           val (v1, env1) = evalExp(e1, env)
           val (v2, env2) = evalExp(e2, env1)
           val v  = case binop of
               Plus  => v1 + v2
             | Minus => v1 - v2
             | Times => v1 * v2
             | Div   => v1 div v2
       in
          (v, env2)
       end
    | evalExp (EseqExp(stm, exp), env) = evalExp(exp, evalStm(stm, env))

in
  evalStm(prog, [])
end

(*
 * compile a statement
 *)
fun compileStm prog (e: EmitFuns.t) = let
 val {emitMeta=meta, emitInstr=instr, emitInstr_=instr_} = e
 
  fun globalsExp (IdExp(id), env) = env
    | globalsExp (NumExp(num), env) = env
    | globalsExp (OpExp(exp1, binop, exp2), env) = globalsExp(exp2, globalsExp(exp1, env))
    | globalsExp (EseqExp(stm, exp), env) = globalsExp(exp, globalsStm(stm, env))
  
  and globalsStm (CompoundStm(stm1, stm2), env) = globalsStm(stm2, globalsStm(stm1, env))
    | globalsStm (AssignStm(name, exp), env) = let
          val newEnv = globalsExp(exp, env)
        in
          if (lookup name newEnv) <> ~1 then newEnv else (name, newId()) :: newEnv
        end
    | globalsStm (PrintStm(exps), env) = List.foldl globalsExp env exps
    
  fun compileExp (IdExp(id), env) = instr_ (ILOADG, lookup id env)
    | compileExp (NumExp(num), env) = instr_ (IPUSHC, num)
    | compileExp (OpExp(exp1, binop, exp2), env) = (
         compileExp(exp1, env); compileExp(exp2, env);
       case binop of
         Plus  => instr IADD
       | Minus => instr ISUB
       | Times => instr IMULT
       | Div   => instr IDIV)
    | compileExp(EseqExp(stm, exp), env) = (compileStm(stm, env); compileExp(exp, env))

  and compileStm (CompoundStm(stm1, stm2), env) = (compileStm(stm1, env); compileStm(stm2, env))
    | compileStm (AssignStm(id, exp), env) = (compileExp(exp, env); instr_ (ISTOREG, (lookup id env)))
    | compileStm (PrintStm(exps), env) = (List.app (fn exp => compileExp(exp, env)) exps;
                       instr_ (IPRINT, length exps))

in
  let
    val _ = resetEnv()
    val globalsEnv = globalsStm(prog, [])
  in
    meta(MAGIC, MAJOR_VERSION, MINOR_VERSION);
    instr_ (GLBLS, length globalsEnv); (* nr globals *)
    compileStm(prog, globalsEnv);
    instr HALT
  end
end


(*
 * vm file format output of program
 *)
fun compile prog codeFileName = let
  val out = BinIO.openOut codeFileName
in
  compileStm prog (VMFEmitter.withStream out);
  BinIO.closeOut out
end

(*
 * assembly output of program
 *)
fun asm prog codeFileName = let
  val out = TextIO.openOut codeFileName
in
  compileStm prog (AsmEmitter.withStream out);
  TextIO.closeOut out
end

end (* structure Comp *)
