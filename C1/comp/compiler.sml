(*
**  compiler.sml
**  vm
**
**  Created by Ovidiu Podisor on 01/30/19.
**  Copyright © 2019 innodocs. All rights reserved.
*)

 
structure Comp : sig
     val print   : Absyn.stm -> unit
     val eval    : Absyn.stm -> (Absyn.id * int) list
     val compile : Absyn.stm -> string -> unit
  end = struct

open Absyn

type env = (id * int) list

fun lookup id (env: env) = case env of
  		    (i, v) :: tl => if id = i then v else lookup id tl
  		  | []           => 0
  
val lastId = ref 0
fun newId() = (lastId := !lastId + 1; !lastId - 1)

fun resetEnv() = lastId := 0

fun printString (s: string) = print s
fun printInt i = printString (Int.toString(i))
fun printNewline () = printString "\n"

fun print stm =
  let
     
    fun printStm stm  = case stm of
          CompoundStm(stm1, stm2) => (printStm stm1; printString ";\n"; printStm stm2)
        | AssignStm(id, exp)      => (printString id; printString " := "; printExp exp)
        | PrintStm exps           => (printString "print("; printExps exps; printString ")")
         
    and printExps exps = case exps of
	      e1 :: e2 :: tl => (printExp e1; printString ","; printExp e2; printExps tl)
        | e1 :: []       => printExp e1
        | []             => ()
         
    and printExp exp = 
      let
        fun openParen  prec opPrec = if prec > opPrec then printString "(" else ()
        fun closeParen prec opPrec = if prec > opPrec then printString ")" else ()
        
        fun print prec exp =  case exp of
            IdExp id => printString id
          | NumExp num => printInt num
          | OpExp(exp1, Plus, exp2)  => (openParen prec 0; print 0 exp1;
                                            printString "+"; print 0 exp2; closeParen prec 0)
          | OpExp(exp1, Minus, exp2) => (openParen prec 1; print 1 exp1;
                                            printString "-"; print 1 exp2; closeParen prec 1)
          | OpExp(exp1, Times, exp2) => (openParen prec 2; print 2 exp1;
                                            printString "*"; print 2 exp2; closeParen prec 2)
          | OpExp(exp1, Div, exp2)   => (openParen prec 3; print 3 exp1;
                                            printString "/"; print 3 exp2; closeParen prec 3)
          | EseqExp(stm, exp)        => (printString "("; printStm stm; printString ", ";
                                            printExp exp; printString ")")
      in
        print 0 exp
    end

  in
    printStm stm;
    printNewline()
end


fun eval prog =
  let
    fun evalStm (CompoundStm(stm1, stm2), env) = evalStm(stm2, evalStm(stm1, env))
      | evalStm (AssignStm(id, exp), env) =
               let
                  val (v, newEnv) = evalExp(exp, env)
               in
                  (id, v)::newEnv
               end
      | evalStm (PrintStm(exps), env) = List.foldl
                      (fn (exp, env) =>
                          let
                             val (v, newEnv) = evalExp(exp, env)
                          in
                             printString (Int.toString(v) ^ "\n");
                             newEnv
                          end)
                       env
                       exps

    and evalExp (IdExp(id), env) =
             let
               val b = List.find (fn (i, v) => i = id) env
               val v = case b of
                  SOME (_, v) => v
                | _           => 0
             in
                (v, env)
             end
      | evalExp (NumExp(i), env) = (i, env)
      | evalExp (OpExp(e1, binop, e2), env) =
             let
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

fun compile prog codeFileName =
  let
  
    (* code file markers *)
  	val MAGIC         = 0x12345678
    val MAJOR_VERSION = 0x00000001
    val MINOR_VERSION = 0x00000001

    (* instruction set *)
    val HALT     = 0xFFFF

    val NOP      = 0x00

    val IGETGLBL = 0x10
    val ISETGLBL = 0x11

    val IPUSHC   = 0x20
    val IPOP     = 0x21
    val ISWAP    = 0x22
    val IDUP     = 0x23

    val IADD     = 0x40
    val ISUB     = 0x41
    val IMULT    = 0x42
    val IDIV     = 0x43
    val IMOD     = 0x44

    val INEG     = 0x60

    val IPRINT   = 0x100


    val out = BinIO.openOut codeFileName
    fun instr i = 
      let
        open Word
        infix andb >>
     
        val w = Word.fromInt i
        val wl = [ w >> 0w24, 
       		   (w andb 0wx00FF0000) >> 0w16, 
       		   (w andb 0wx0000FF00) >> 0w8,
       		   (w andb 0wx000000FF)]
        val w8l = map (fn w => Word8.fromInt (Word.toInt w)) wl
      in
        BinIO.output (out, Word8Vector.fromList w8l)
      end

   fun  globalsExp (IdExp(id), env) = env
      | globalsExp (NumExp(num), env) = env
      | globalsExp (OpExp(exp1, binop, exp2), env) = globalsExp(exp2, globalsExp(exp1, env))
	  | globalsExp (EseqExp(stm, exp), env) = globalsExp(exp, globalsStm(stm, env))
	  
    and globalsStm (CompoundStm(stm1, stm2), env) = globalsStm(stm2, globalsStm(stm1, env))
      | globalsStm (AssignStm(id, exp), env) = (id, newId()) :: globalsExp(exp, env)
      | globalsStm (PrintStm(exps), env) = List.foldl globalsExp env exps
      

    fun compileExp (IdExp(id), env) = (instr IGETGLBL; instr (lookup id env))
      | compileExp (NumExp(num), env) = (instr IPUSHC; instr num)
      | compileExp (OpExp(exp1, binop, exp2), env) = (
           compileExp(exp1, env); compileExp(exp2, env);
	       case binop of
	         Plus  => instr IADD
	       | Minus => instr ISUB
	       | Times => instr IMULT
	       | Div   => instr IDIV)
	  | compileExp(EseqExp(stm, exp), env) = (compileStm(stm, env); compileExp(exp, env))

    and compileStm (CompoundStm(stm1, stm2), env) = (compileStm(stm1, env); compileStm(stm2, env))
      | compileStm (AssignStm(id, exp), env) = (compileExp(exp, env); instr ISETGLBL; instr (lookup id env))
      | compileStm (PrintStm(exps), env) = (map (fn exp => compileExp(exp, env)) exps;
                         instr IPUSHC; instr (length exps); instr IPRINT)

  in
    let
      val _ = resetEnv()
      val globalsEnv = globalsStm(prog, [])
      val env = globalsEnv
    in

	  instr MAGIC; instr MAJOR_VERSION; instr MINOR_VERSION;
	  instr (length globalsEnv); (* nr globals *)
	  compileStm(prog, env);
	  instr HALT;
	  
	  BinIO.closeOut out
    end
end


end (* structure Comp *)
