(*
**  compiler.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 03/15/19.
**  Copyright © 2019 innodocs. All rights reserved.
*)

structure Comp : sig
  
    type env
    exception TypeError of string

    val compile : Absyn.stm -> string -> unit
    val asm     : Absyn.stm -> string -> unit
  end = struct

structure A = Absyn
structure I = Instr
structure PP = PrettyPrint

exception TypeError of string

(**
 * ids
 *)
val lastId = ref ~1
val ifLabelId = ref 0
val whileLabelId = ref 0
val forLabelId = ref 0
val repeatLabelId = ref 0

fun resetEnv() = (
  lastId := ~1;
  ifLabelId := 0;
  whileLabelId := 0;
  forLabelId := 0;
  repeatLabelId := 0)


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
        val prefix = "if" ^ Int.toString(!ifLabelId)
      in
        (E.Label(prefix ^ "_start", startP),
         E.Label(prefix ^ "_else", elseP),
         E.Label(prefix ^ "_end" , endP))
      end

    fun nextLoopLabels(labelId, labelPrefix, startP, endP) = let
        val _ = (labelId := !labelId + 1)
        val prefix = labelPrefix ^ Int.toString(!labelId)
      in
        (E.Label(prefix ^ "_start", startP),
         E.Label(prefix ^ "_end"  , endP))
      end
    
    fun nextWhileLabels(startP, endP)  = nextLoopLabels (whileLabelId, "while", startP, endP)
    fun nextForLabels(startP, endP)    = nextLoopLabels (forLabelId, "for", startP, endP)
    fun nextRepeatLabels(startP, endP) = nextLoopLabels (repeatLabelId, "repeat", startP, endP)

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
          | A.RangeExp(startExp, endExp, _) =>
                  raise TypeError("bad expression")
     
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

          | A.ForStm(A.SimpleVar(id, _), rangeExp, bodyStm, _) =>
              let
                val tstStr = E.newMemStream()
                val (startExp, endExp) = case rangeExp of
                    A.RangeExp(startExp, endExp, _) => (startExp, endExp)
                  | _ => raise TypeError("range exected")
                val startStr = E.newMemStream()
                val startLen = E.withStream startStr (fn () => compileBlock(A.ExpStm(startExp), env))
                val endStr = E.newMemStream()
                val endLen = E.withStream endStr (fn () => compileBlock(A.ExpStm(endExp), env))
                val bodyStr = E.newMemStream()
                val bodyLen = E.withStream bodyStr (fn () => compileBlock(bodyStm, env))
                val idSlot = lookup(id, env)

                val (startLabel, endLabel) = nextForLabels(
                    (* jump to start of loop:
                         2: load loop variable
                       + length of end expression
                       + 2: size of conditional jump instr
                       + length of body
                       + 7: loop variable increment *)
                    ~(2 + endLen + 2 + bodyLen + 7),
                    (* jump to end (exit loop):
                      + 2: size of conditional jump instr
                      + length of body
                      + 7: loop variable increment
                      + 2: size of unconditional GOTO instr at end of loop *)
                    2 + bodyLen + 7 + 2)
              in
               E.mergeStream startStr;
               instr_ (I.ISTOREG, idSlot);
               E.emitLabel startLabel;
               instr_ (I.ILOADG, idSlot);
               E.mergeStream endStr;
               E.emitJump (I.IF_ICMPGT, endLabel);
               E.mergeStream bodyStr;

               (* increment loop variable *)
               instr_ (I.IPUSHC, 1);
               instr_ (I.ILOADG, idSlot);
               instr (I.IADD);
               instr_ (I.ISTOREG, idSlot);
               (* next loop iteration *)
               E.emitJump (I.GOTO, startLabel);
               E.emitLabel endLabel
           end

          | A.RepeatStm(tstExp, bodyStm, _) =>
              let
                val tstStr = E.newMemStream()
                val tstLen = E.withStream tstStr (fn () => compileTestExp(tstExp, env))

                val bodyStr = E.newMemStream()
                val bodyLen = E.withStream bodyStr (fn () => compileBlock(bodyStm, env))

                val (startLabel, endLabel) = nextRepeatLabels(
                     (* jump to start of loop:
                          length of block
                        + length of test *)
                     ~(tstLen + bodyLen),
                     (* jump to end (exit loop - not needed, just a marker in 'asm' file):
                          0 *)
                     0)
              in
                E.emitLabel startLabel;
                E.mergeStream bodyStr;
                E.mergeStream tstStr;
                E.emitJump (I.not (jmpInstr tstExp), startLabel);
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
          | A.RangeExp(startExp, endExp, _) => globalsExp(endExp, globalsExp(startExp, env))
   
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
          | A.ForStm(A.SimpleVar(id, _), rangeExp, bodyStm, _) =>
              globalsStm (bodyStm, globalsExp (rangeExp, env))
          | A.RepeatStm(tstExp, bodyStm, _) =>
              globalsStm (bodyStm, globalsExp(tstExp, env))
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
