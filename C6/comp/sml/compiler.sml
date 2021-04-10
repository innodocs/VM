(*
**  compiler.sml
**  vm-comp
**
*A  Created by Ovidiu Podisor on 03/15/19.
*C  Copyright © 2019-2021 innodocs. All rights reserved.
*)

structure Comp :
  sig
    type env
    val compile : Absyn.stm -> string -> unit
    val asm     : Absyn.stm -> string -> unit
  end =
struct

structure A = Absyn
structure T = Type
structure S = Symbol
structure I = Instr
structure SP = StringPool
structure PP = PrettyPrint

exception TypeError = T.Error

(**
 * environments
 *)
type env = (int * T.ty ref) S.table

val lastVarId = ref ~1

fun addVar (sym, env: env) =
  case S.lookup (env, sym) of
    SOME _ => env
  | NONE => (lastVarId := !lastVarId + 1; S.enter (env, sym, (!lastVarId, T.undef())))

fun varId (sym, env: env) =
  case S.lookup (env, sym) of
    SOME (i, _) => i
  | NONE => raise TypeError("undefined variable '" ^ S.name sym ^ "'")

fun varType (sym, env: env) =
  case S.lookup (env, sym) of
    SOME (_, ty) => !ty
  | NONE => raise TypeError("undefined variable '" ^ S.name sym ^ "'")

fun unifyVarType(sym, env, ty) =
let
  val vTyR = case S.lookup (env, sym) of
      SOME (_, ty) => ty
    | NONE => raise TypeError("undefined variable '" ^ S.name sym ^ "'")
  val uTy = T.unify(!vTyR, ty)
in
  if uTy = T.NOTHING
    then raise TypeError("no compatible type for '" ^ S.name sym ^ "' found")
  else
    vTyR := uTy;
  uTy
end

(**
 *  reset the global environment
 *   - call before compiling another program
 *)
val ifLabelId = ref 0
val whileLabelId = ref 0
val forLabelId = ref 0
val repeatLabelId = ref 0

fun resetEnv() = (
  (* variables *)
  lastVarId := ~1;

  (* string pool *)
  SP.clear();

  (* loop labels *)
  ifLabelId := 0;
  whileLabelId := 0;
  forLabelId := 0;
  repeatLabelId := 0;

  S.empty
)


(**
 * compile a program
 *)
functor CompileProgFun(structure E: EMITTER):
  sig
    val compileProg: A.stm -> unit
  end =
struct

  fun compileTestExp (exp, env) = case exp of
          A.BinOpExp(exp1, _, exp2, _, _) => (
            compileBlock (A.ExpStm(exp1), env);
            compileBlock (A.ExpStm(exp2), env))
        | A.UnOpExp(_, exp, _, _) =>
            compileBlock (A.ExpStm(exp), env)
        | _ => 
            compileBlock (A.ExpStm(exp), env)


  and compileBlock (prog, env) = let

    val (instr, instr_, instr__) = (E.emitInstr, E.emitInstrI, E.emitInstrIS)

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

    fun jmpInstr (A.BinOpExp(_, binop, _, _, _)) = (case binop of
            A.EqOp  => I.IF_ICMPEQ
          | A.NeqOp => I.IF_ICMPNE
          | A.LtOp  => I.IF_ICMPLT
          | A.LeOp  => I.IF_ICMPLE
          | A.GtOp  => I.IF_ICMPGT
          | A.GeOp  => I.IF_ICMPGE
          | A.AndOp => I.IF_INE
          | A.OrOp  => I.IF_INE
          | _       => (raise TypeError("expected <bool>"); I.HALT))
      | jmpInstr (A.UnOpExp(unop, _, _, _)) = (case unop of
            A.NotOp => I.IF_INE
          | _       => (raise TypeError("expected <bool>"); I.HALT))
      | jmpInstr _  =  (raise TypeError("expected <bool>"); I.HALT)
   
    fun compileExp (exp, env) = case exp of
            A.VarExp(A.SimpleVar(name, _), _) => instr_(I.ILOADG, varId(name, env))
          | A.BoolExp(b, _, _) => instr_(I.IPUSHC, if b then 1 else 0)
          | A.IntExp(num, _, _) => instr_(I.IPUSHC, num)
          | A.StringExp(s, _, _) => instr__(I.SPUSHC, SP.id(SP.add(s)), s)
          | A.BinOpExp(exp1, binop, exp2, _, _) => (
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
          | A.UnOpExp(unop, exp, _, _) => (
              compileExp(exp, env);
              case unop of
                A.NegOp   => instr(I.INEG)
              | A.NotOp   => instr(I.INOT))
          | A.RangeExp(startExp, endExp, _, _) =>
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

          | A.ForStm(A.SimpleVar(name, _), rangeExp, bodyStm, _) =>
              let
                val tstStr = E.newMemStream()
                val (startExp, endExp) = case rangeExp of
                    A.RangeExp(startExp, endExp, _, _) => (startExp, endExp)
                  | _ => raise TypeError("range exected")
                val startStr = E.newMemStream()
                val startLen = E.withStream startStr (fn () => compileBlock(A.ExpStm(startExp), env))
                val endStr = E.newMemStream()
                val endLen = E.withStream endStr (fn () => compileBlock(A.ExpStm(endExp), env))
                val bodyStr = E.newMemStream()
                val bodyLen = E.withStream bodyStr (fn () => compileBlock(bodyStm, env))
                val idSlot = varId(name, env)

                val (startLabel, endLabel) = nextForLabels(
                    (* jump to start of loop:
                         3: load loop variable (2) + DUP (1)
                       + length of end expression
                       + 2: size of conditional jump instr
                       + length of body
                       + 5: loop variable increment *)
                    ~(3 + endLen + 2 + bodyLen + 5),
                    (* jump to end (exit loop):
                      + 2: size of conditional jump instr
                      + length of body
                      + 5: loop variable increment
                      + 2: size of unconditional GOTO instr at end of loop *)
                    2 + bodyLen + 5 + 2)
              in
               E.mergeStream startStr;
               instr_ (I.ISTOREG, idSlot);
               E.emitLabel startLabel;
               instr_ (I.ILOADG, idSlot);
               instr (I.IDUP);
               E.mergeStream endStr;
               E.emitJump (I.IF_ICMPGT, endLabel);
               E.mergeStream bodyStr;

               (* increment loop variable *)
               instr_ (I.IPUSHC, 1);
               (* instr_ (I.ILOADG, idSlot); *)
               instr (I.IADD);
               instr_ (I.ISTOREG, idSlot);
               (* next loop iteration *)
               E.emitJump (I.GOTO, startLabel);
               E.emitLabel endLabel;
               instr (I.IPOP)
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

          | A.AssignStm(A.SimpleVar(name, _), exp, _) => (
              compileExp(exp, env);
              (* this is the variable definition *)
              instr_(I.ISTOREG, varId(name, env)))

          | A.PrintStm(exps, _) => (
              if List.exists (fn exp => A.getType(exp) = T.STRING) exps
                then
                  List.app
                    (fn exp => let
                      val ty = T.realTy(A.getType(exp))
                    in
                      compileExp(exp, env);
                      case ty of
                        T.INT => instr_(I.IPRINT, 1)
                      | T.STRING => instr_(I.SPRINT, 1)
                      | _ => raise TypeError("Print[" ^ T.toString(ty) ^ "T] not defined")
                    end)
                    exps
              else (
                List.app (fn exp => compileExp(exp, env)) exps;
                instr_(I.IPRINT, length exps)))
  
  in
    compileStm(prog, env)
  end
  
  (**
   * compile a program
   *)
  fun compileProg prog = let

    (**
     *  find global definitions
     *)
    fun globalDefs (A.AssignStm(A.SimpleVar(name, _), _, _), env) =
          addVar(name, env)
      | globalDefs (A.PrintStm(exps, _), env) = (
          List.app
            (fn exp => case exp of
                 A.StringExp(s, _, _) => (SP.add(s); ())
               | _ => ())
            exps;
          env)
      | globalDefs (A.SeqStm(stms), env) = List.foldl globalDefs env stms
      | globalDefs (A.IfStm(tstExp, thenStm, elseStmO, _), env) = let
            val thenEnv = globalDefs (thenStm, env)
          in
            case elseStmO of
              SOME(elseStm) => globalDefs (elseStm, thenEnv)
            | NONE => thenEnv
          end
      | globalDefs (A.WhileStm(tstExp, bodyStm, _), env) =
          globalDefs (bodyStm, env)
      | globalDefs (A.ForStm(A.SimpleVar(name, _), rangeExp, bodyStm, _), env) =
          globalDefs (bodyStm, addVar(name, env))
      | globalDefs (A.RepeatStm(tstExp, bodyStm, _), env) =
          globalDefs (bodyStm, env)
      | globalDefs (_, env) = env

    (**
     *  type inference
     *)
    fun expectType (exp, ty) =
    let
      val uTy = T.unify(ty, A.getType(exp))
    in
      if uTy = T.NOTHING
        then raise TypeError("type inference failed, expected type " ^ T.toString ty)
      else A.setType(exp, uTy);
      uTy
    end

    fun inferTypeExp (exp as A.BoolExp(_, ty, p), env) = expectType(exp, T.BOOL)
      | inferTypeExp (exp as A.IntExp(_, ty, p), env) = expectType(exp, T.INT)
      | inferTypeExp (exp as A.StringExp(_, ty, p), env) = expectType(exp, T.STRING)
      | inferTypeExp (A.VarExp(A.SimpleVar(name, _), ty), env) = let
            val uTy = T.unify(varType(name, env), !ty)
          in
            if uTy = T.NOTHING
              then raise TypeError("type inference failed (VarExp[" ^ S.name name ^ "])")
            else (
              unifyVarType(name, env, uTy);
              ty := uTy);
            !ty
          end
      | inferTypeExp (A.RangeExp(exp1, exp2, ty, p), env) = let
            val e1Ty = inferTypeExp(exp1, env)
            val e2Ty = inferTypeExp(exp2, env)
            val uTy = T.unify(A.getType(exp1), A.getType(exp2))
          in
            if uTy = T.NOTHING
              then raise TypeError("<bad unify: RangeExp>")
            else ty := T.ARRAY(uTy);
            !ty
          end
      | inferTypeExp (A.UnOpExp(unop, exp, ty, p), env) = let
            val eTy = case unop of
                A.NegOp => T.INT
              | A.NotOp => T.BOOL
          in
            expectType(exp, eTy);
            ty := eTy;
            !ty
          end
      | inferTypeExp (A.BinOpExp(exp1, binop, exp2, ty, p), env) = let
            val eTy = case binop of
                A.PlusOp  => T.INT
              | A.MinusOp => T.INT
              | A.TimesOp => T.INT
              | A.DivOp   => T.INT
              | A.ModOp   => T.INT
              | A.PowOp   => T.INT
              | A.EqOp    => T.BOOL
              | A.NeqOp   => T.BOOL
              | A.LtOp    => T.BOOL
              | A.LeOp    => T.BOOL
              | A.GtOp    => T.BOOL
              | A.GeOp    => T.BOOL
              | A.AndOp   => T.BOOL
              | A.OrOp    => T.BOOL
            val tyExp1 = T.unify(eTy, A.getType(exp1))
            val tyExp2 = T.unify(eTy, A.getType(exp2))
            val uTy = T.unify(tyExp1, tyExp2)
          in
            if uTy = T.NOTHING
              then raise TypeError("<bad unify: BinOpExp>")
            else (
              expectType(exp1, uTy);
              expectType(exp2, uTy);
              ty := uTy);
            !ty
          end

    fun inferTypes (A.EmptyStm(_), env: env) = env
      | inferTypes (A.ExpStm(exp), env)= env (*!! remove: not in GAP *)
      | inferTypes (A.PrintStm(exps, _), env) = (
          List.app (fn e => (inferTypeExp(e, env); ())) exps;
          env)
      | inferTypes (A.SeqStm(stms), env) = List.foldl inferTypes env stms
      | inferTypes (A.AssignStm(A.SimpleVar(name, _), exp, p), env) = let
          val uTy = T.unify(varType(name, env), inferTypeExp(exp, env))
        in
          if uTy = T.NOTHING
            then raise TypeError("<bad unify: Assign>")
          else (
            unifyVarType(name, env, uTy);
            expectType(exp, uTy));
          env
        end
      | inferTypes (A.IfStm(tstExp, thenStm, elseStm, p), env) = let
            val _ = expectType(tstExp, T.BOOL)
          in
            inferTypes(thenStm, env);
            case elseStm of
              SOME(elseStm) => inferTypes(elseStm, env)
            | NONE => env
          end
      | inferTypes (A.WhileStm(tstExp, bodyStm, p), env) = (
          expectType(tstExp, T.BOOL);
          inferTypes(bodyStm, env))
      | inferTypes (A.RepeatStm(tstExp, bodyStm, p), env) = (
          expectType(tstExp, T.BOOL);
          inferTypes(bodyStm, env))
      | inferTypes (A.ForStm(A.SimpleVar(name, _), rangeExp, bodyStm, p), env) = let
            val rTy = T.unify(T.ARRAY(!(T.undef())), inferTypeExp(rangeExp, env))
            val uTy = case rTy of
                T.ARRAY(eTy) => T.unify(eTy, varType(name, env))
              | _ => raise TypeError("for statement: range type should be Array[*]")
          in
            if uTy = T.NOTHING
              then raise TypeError("for statement: bad unify")
            else (
              unifyVarType(name, env, uTy);
              expectType(rangeExp, T.ARRAY(uTy)));
            inferTypes(bodyStm, env)
          end

  in
    let
      val (instr, instr_) = (E.emitInstr, E.emitInstrI)

      val globalsEnv = inferTypes(prog, globalDefs(prog, resetEnv()))
      val nrStrings = SP.size()
    in
      (* file header *)
      E.emitMeta (I.MAGIC, I.MAJOR_VERSION, I.MINOR_VERSION);
      instr_ (I.GLOBALS, S.size globalsEnv);   (* nr globals *)
      instr_ (I.STRINGS, nrStrings);           (* nr strings *)
      if nrStrings > 0 then E.emitStrings() else ();

      compileBlock (prog, globalsEnv);

      (* file ending *)
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
