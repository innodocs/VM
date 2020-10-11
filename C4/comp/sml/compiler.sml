(*
**  compiler.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 01/31/19.
**  Copyright © 2019 innodocs. All rights reserved.
*)

structure Comp : sig
  
    type env
    exception TypeError of string

    val print   : Absyn.stm -> unit
    val eval    : Absyn.stm -> env
    val compile : Absyn.stm -> string -> unit
    val asm     : Absyn.stm -> string -> unit
  end = struct

structure A = Absyn
structure I = Instr

exception TypeError of string

(**
 * ids
 *)
val lastId = ref ~1
val ifLabelId = ref 0
val whileLabelId = ref 0

fun resetEnv() = (
  lastId := ~1;
  ifLabelId := 0;
  whileLabelId := 0)


(**
 * environments
 *)
type env = (A.id * int) list

fun lookup (id, env: env) = case env of
  		    (i, v) :: tl => if id = i then v else lookup (id, tl)
  		  | []           => ~1

fun newId() = (
  lastId := !lastId + 1;
  !lastId)


(**
 * printing
 *)
fun printString (s: string) = print s
fun printInt i = printString (Int.toString(i))
fun printNL () = printString "\n"


(**
 * print a statement
 *)
fun print stm = let

  fun indent 0 = ()
    | indent level = (printString(" "); indent (level-1))
     
  fun printStm (A.SeqStm stms) l = List.app (fn s => printStm s l) stms
    | printStm (A.EmptyStm _) l = printString ";"
    | printStm (A.ExpStm exp) l = printExp(exp)   
    | printStm (A.IfStm (tst, t, eo, _)) l =
            (printNL(); indent l ;
             printString "if " ; printExp tst; printString " then" ;
             printStm t(l+1);
             case eo of
               SOME(e) => (indent l; printNL(); printString "else"; printStm e (l+1))
             | _ => ();
             printNL(); indent l ; printString "fi;")
    | printStm (A.WhileStm (tst, b, _)) l =
            (printNL(); indent l;
             printString "while "; printExp tst ; printString " do";
             printStm b (l+1);
             printNL(); indent l; printString "od;")
    | printStm (A.AssignStm (v, exp, _)) l =
            (printNL(); indent l;
             printVar v; printString " := "; printExp exp; printString ";")
    | printStm (A.PrintStm (exps, _)) l =
            (printNL(); indent l;
             printString "Print(";
             case exps of
                 e :: tl => (printExp e;
                   List.app (fn e => (printString ", "; printExp e)) tl)
               | [] => ();
             printString ");")
         
  and printExp exp = let
    fun openParen  prec opPrec = if prec > opPrec then printString "(" else ()
    fun closeParen prec opPrec = if prec > opPrec then printString ")" else () 
  
    fun print prec (A.VarExp v) = printVar v
      | print prec (A.NumExp (num, _)) = printInt num
      | print prec (A.BinOpExp (exp1, binop, exp2, _)) = let
          val (opStr, opPrec) = case binop of
            A.PlusOp  => (" + ", 4)
          | A.MinusOp => (" - ", 5)
          | A.TimesOp => (" * ", 6)
          | A.DivOp   => (" / ", 7)
          | A.ModOp   => (" mod ", 8)
          | A.PowOp   => ("^", 9)
          | A.EqOp    => (" = ", 3)
          | A.NeqOp   => (" <> ", 3)
          | A.LtOp    => (" < ", 2)
          | A.LeOp    => (" <= ", 2)
          | A.GtOp    => (" > ", 2)
          | A.GeOp    => (" >= ", 2)
          | A.AndOp   => (" and ", 0)
          | A.OrOp    => (" or ", 0)          
        in
          openParen prec opPrec;
          print opPrec exp1; printString opStr; print opPrec exp2;
          closeParen prec opPrec
        end
      | print prec (A.UnOpExp (unop, exp, _)) = let
          val (opStr, opPrec) = case unop of
            A.NotOp => ("not", 10)
          | A.NegOp => ("-", 1)
        in
          openParen prec opPrec;
          printString opStr; print opPrec exp;
          closeParen prec opPrec
        end
    in
      print 0 exp
    end
      
  and printVar (A.SimpleVar (id, pos)) = printString(id)

in
  printStm stm 0;
  printNL()
end


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
           val v = ref tstV
           val newEnv = ref tstEnv
         in
           while !v <> 0 do let
             val bEnv = evalStm b (!newEnv)
             val (tstV, tstEnv) = evalExp tst bEnv
           in
             v := tstV;
             newEnv := tstEnv
           end;
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
                 printString (Int.toString hdV);
                 List.foldl
                   (fn (exp, env) => let
                      val (v, expEnv) = evalExp exp env
                    in
                      printString (", " ^ (Int.toString v)); expEnv
                    end)
                   hdEnv tlExps                 
                 end
             | [] => env
         in
           printString "\n";
           newEnv
         end

  and evalExp (A.VarExp(A.SimpleVar(id, _))) env = let
          val b = List.find (fn (i, v) => i = id) env
          val v = case b of
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
in
  evalStm prog []
end


(**
 * compile a program
 *)
functor CompileProgFun(structure E: EMITTER):
  sig
    val compileProg: A.stm -> unit
  end =
struct

  fun compileTestExp (exp, env) = case exp of
          A.BinOpExp(exp1, _, exp2, _) => (
            compileBlock (A.ExpStm(exp1), env);
            compileBlock (A.ExpStm(exp2), env))
        | A.UnOpExp(_, exp, _) =>
            compileBlock (A.ExpStm(exp), env)
        | _ => 
            compileBlock (A.ExpStm(exp), env)


  and compileBlock (prog, env) = let
  
    val (instr, instr_) = (E.emitInstr, E.emitInstr_)
   
    fun nextIfLabels(startP, elseP, endP) = let
        val _ = (ifLabelId := !ifLabelId + 1)
        val prefix = "If" ^ Int.toString(!ifLabelId)
      in
        (E.Label(prefix ^ "Start", startP),
         E.Label(prefix ^ "Then", elseP),
         E.Label(prefix ^ "End" , endP))
      end
    
    fun nextWhileLabels(startP, endP) = let 
        val _ = (whileLabelId := !whileLabelId + 1)
        val prefix = "While" ^ Int.toString(!whileLabelId)
      in
        (E.Label(prefix ^ "Start", startP),
         E.Label(prefix ^ "End"  , endP))
      end
  
    fun jmpInstr (A.BinOpExp(_, binop, _, _)) = (case binop of
            A.EqOp  => I.IF_ICMPEQ
          | A.NeqOp => I.IF_ICMPNE
          | A.LtOp  => I.IF_ICMPLT
          | A.LeOp  => I.IF_ICMPLE
          | A.GtOp  => I.IF_ICMPGT
          | A.GeOp  => I.IF_ICMPGE
          | A.AndOp => I.IF_INE
          | A.OrOp  => I.IF_INE
          | _       => (raise TypeError("expected <bool>"); I.HALT))
      | jmpInstr (A.UnOpExp(unop, _, _)) = (case unop of
            A.NotOp => I.IF_INE
          | _       => (raise TypeError("expected <bool>"); I.HALT))
      | jmpInstr _  =  (raise TypeError("expected <bool>"); I.HALT)
   
    fun compileExp (exp, env) = case exp of
            A.VarExp(A.SimpleVar(id, _)) => instr_(I.ILOADG, lookup(id, env))
          | A.NumExp(num, _) => instr_(I.IPUSHC, num)
          | A.BinOpExp(exp1, binop, exp2, _) => (
              compileExp(exp1, env);
              compileExp(exp2, env);
              case binop of
                A.PlusOp  => instr(I.IADD)
              | A.MinusOp => instr(I.ISUB)
              | A.TimesOp => instr(I.IMULT)
              | A.DivOp   => instr(I.IDIV)
              | A.ModOp   => instr(I.IMOD)
              | A.PowOp   => instr(I.IPOW)
              | A.EqOp    => instr(I.ICMPEQ)
              | A.NeqOp   => instr(I.ICMPNE)
              | A.LtOp    => instr(I.ICMPLT)
              | A.LeOp    => instr(I.ICMPLE)
              | A.GtOp    => instr(I.ICMPGT)
              | A.GeOp    => instr(I.ICMPGE)
              | A.AndOp   => instr(I.IAND)
              | A.OrOp    => instr(I.IOR))
          | A.UnOpExp(unop, exp, _) => (
              compileExp(exp, env);
              case unop of
                A.NegOp   => instr(I.INEG)
              | A.NotOp   => instr(I.INOT))
     
    and compileStm (stm, env) = case stm of
            A.EmptyStm(_) => ()
            
          | A.ExpStm(exp) => compileExp(exp, env)

          | A.SeqStm(stms) => List.app (fn stm => compileStm(stm, env)) stms
          
          | A.IfStm(tstExp, thenStm, elseStmO, _) =>
              let
                val tstStr = E.newMemStream()
                val tstLen = E.withStream tstStr (fn () => compileTestExp(tstExp, env))
                
                val thenStr = E.newMemStream()
                val thenLen = E.withStream thenStr (fn () => compileBlock(thenStm, env))
                
                val elseStr = E.newMemStream()
                val elseLen = case elseStmO of
                      SOME(elseStm) => E.withStream elseStr (fn () => compileBlock(elseStm, env))
                    | _ => 0
                    
                val (startLabel, elseLabel, endLabel) = nextIfLabels(
                     (* start label not needed, just a marker in the 'asm' file *)
                     ~1,
                     (* jump to 'else' branch: 
                          length of 'then' branch
                          + 2: size of conditional jump instr
                          + 2: size of unconditional GOTO instr at end of 'then' *)
                     thenLen + 2 + 2,
                     (* jump to end: 
                          length of 'then' or 'else' branch
                          + 2: size of unconditional GOTO instr at end of 'then'
                          or size of conditional jump instr when no 'else' branch *)
                     2 + (if elseLen > 0 then elseLen else thenLen))             
              in
                E.emitLabel startLabel;
                E.mergeStream tstStr;
                E.emitJump(
                    Instr.not (jmpInstr tstExp), 
                    if elseLen > 0 then elseLabel else endLabel);
                
                E.mergeStream thenStr;
                if elseLen > 0 then (
                  E.emitJump (I.GOTO, endLabel);
                  E.emitLabel elseLabel;
                  E.mergeStream elseStr )
                else ();

                E.emitLabel endLabel                
              end
 
          | A.WhileStm(tstExp, bodyStm, _) =>
              let
                val tstStr = E.newMemStream()
                val tstLen = E.withStream tstStr (fn () => compileTestExp(tstExp, env))

                val bodyStr = E.newMemStream()
                val bodyLen = E.withStream bodyStr (fn () => compileBlock(bodyStm, env))

                val (startLabel, endLabel) = nextWhileLabels(
                     (* jump to start of loop:
                          length of test
                        + length of block
                        + 2: size of conditional jump instr *)
                     ~(tstLen + bodyLen + 2),
                     (* jump to end (exit loop):
                          length of block
                        + 2: size of conditional jump instr
                        + 2: size of unconditional GOTO instr at end of loop *)
                     bodyLen + 2 + 2)
              in
                E.emitLabel startLabel;
                E.mergeStream tstStr;
                E.emitJump (I.not (jmpInstr tstExp), endLabel);
                E.mergeStream bodyStr;
                E.emitJump (I.GOTO, startLabel);
                E.emitLabel endLabel
            end
 
          | A.AssignStm(A.SimpleVar(id, _), exp, _) => (
              compileExp(exp, env);
              instr_(I.ISTOREG, lookup(id, env)))

          | A.PrintStm(exps, _) => (
              List.app (fn exp => compileExp(exp, env)) exps;
              instr_(I.IPRINT, length exps))
  
  in
    compileStm(prog, env)
  end
  
  
  (**
   * compile a program
   *)
  fun compileProg prog = let
  
    val (instr, instr_) = (E.emitInstr, E.emitInstr_)
 
    fun globalsExp (exp, env) = case exp of
            A.VarExp(v) => env
          | A.NumExp(num, _) => env
          | A.BinOpExp(exp1, binop, exp2, _) => globalsExp(exp2, globalsExp(exp1, env))
          | A.UnOpExp(unop, exp, _) => globalsExp(exp, env)
   
    and globalsStm (stm, env) = case stm of
            A.EmptyStm(_) => env
          | A.ExpStm(exp) => globalsExp (exp, env)
          | A.SeqStm(stms) => List.foldl globalsStm env stms 
          | A.IfStm(tstExp, thenStm, elseStmO, _) => let
                val thenEnv = globalsStm (thenStm, globalsExp (tstExp, env))
              in
                case elseStmO of 
                  SOME(elseStm) => globalsStm (elseStm, thenEnv)
                | NONE => thenEnv
              end  
          | A.WhileStm(tstExp, bodyStm, _) =>
              globalsStm (bodyStm, globalsExp (tstExp, env))
          | A.AssignStm(A.SimpleVar(name, _), exp, _) => let
                val newEnv = globalsExp (exp, env)
              in
                if (lookup (name, newEnv) <> ~1)
                  then newEnv
                else (name, newId()) :: newEnv
              end
          | A.PrintStm(exps, _) => List.foldl globalsExp env exps
  
  in
    let
      val _ = resetEnv()
      val globalsEnv = globalsStm(prog, [])
    in
      E.emitMeta (I.MAGIC, I.MAJOR_VERSION, I.MINOR_VERSION);
      instr_ (I.GLBLS, length globalsEnv); (* nr globals *)
      compileBlock (prog, globalsEnv);
      instr I.HALT
    end
  end
  
end (* functor CompileProgFun *)


(**
 * vm file format output of program
 *)
structure VMFC = CompileProgFun(structure E = VMFEmitter)

fun compile prog codeFileName = let
  val out = BinIO.openOut codeFileName
in
  VMFEmitter.withIOStream out (fn () => VMFC.compileProg prog);
  BinIO.closeOut out
end

(**
 * assembly output of program
 *)
structure ASMC = CompileProgFun(structure E = AsmEmitter)

fun asm prog codeFileName = let
  val out = TextIO.openOut codeFileName
in
  AsmEmitter.withIOStream out (fn () => ASMC.compileProg prog);
  TextIO.closeOut out
end

end (* structure Comp *)
