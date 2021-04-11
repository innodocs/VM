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

fun varId (sym, env: env, p) =
  case S.lookup (env, sym) of
    SOME (i, _) => i
  | NONE => raise TypeError("undefined variable '" ^ S.name sym ^ "'", p)

fun varType (sym, env: env, p) =
  case S.lookup (env, sym) of
    SOME (_, ty) => !ty
  | NONE => raise TypeError("undefined variable '" ^ S.name sym ^ "'", p)


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

    fun jmpInstr (A.BinOpExp(_, binop, _, _, _), p) = (case binop of
            A.EqOp  => I.IF_ICMPEQ
          | A.NeqOp => I.IF_ICMPNE
          | A.LtOp  => I.IF_ICMPLT
          | A.LeOp  => I.IF_ICMPLE
          | A.GtOp  => I.IF_ICMPGT
          | A.GeOp  => I.IF_ICMPGE
          | A.AndOp => I.IF_INE
          | A.OrOp  => I.IF_INE
          | _       => (raise TypeError("incompatible type, expected Bool", p); I.HALT))
      | jmpInstr (A.UnOpExp(unop, _, _, _), p) = (case unop of
            A.NotOp => I.IF_INE
          | _       => (raise TypeError("incompatible type, expected Bool", p); I.HALT))
      | jmpInstr (_, p) =  (raise TypeError("incompatible type, expected Bool", p); I.HALT)
   
    fun compileExp (exp, env) = case exp of
            A.VarExp(A.SimpleVar(name, p), _) => instr_(I.ILOADG, varId(name, env, p))
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
          | A.RangeExp(startExp, endExp, _, p) =>
                  raise TypeError("bad expression", p)
     
    and compileStm (stm, env) = case stm of
            A.EmptyStm(_) => ()
            
          | A.ExpStm(exp) => compileExp(exp, env)

          | A.SeqStm(stms) => List.app (fn stm => compileStm(stm, env)) stms
          
          | A.IfStm(tstExp, thenStm, elseStmO, p) =>
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
                    Instr.not (jmpInstr (tstExp, p)),
                    if elseLen > 0 then elseLabel else endLabel);
                
                E.mergeStream thenStr;
                if elseLen > 0 then (
                  E.emitJump (I.GOTO, endLabel);
                  E.emitLabel elseLabel;
                  E.mergeStream elseStr )
                else ();

                E.emitLabel endLabel                
              end
 
          | A.WhileStm(tstExp, bodyStm, p) =>
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
                E.emitJump (I.not (jmpInstr(tstExp, p)), endLabel);
                E.mergeStream bodyStr;
                E.emitJump (I.GOTO, startLabel);
                E.emitLabel endLabel
            end

          | A.ForStm(A.SimpleVar(name, _), rangeExp, bodyStm, p) =>
              let
                val tstStr = E.newMemStream()
                val (startExp, endExp) = case rangeExp of
                    A.RangeExp(startExp, endExp, _, _) => (startExp, endExp)
                  | _ => raise TypeError("range exected", p)
                val startStr = E.newMemStream()
                val startLen = E.withStream startStr (fn () => compileBlock(A.ExpStm(startExp), env))
                val endStr = E.newMemStream()
                val endLen = E.withStream endStr (fn () => compileBlock(A.ExpStm(endExp), env))
                val bodyStr = E.newMemStream()
                val bodyLen = E.withStream bodyStr (fn () => compileBlock(bodyStm, env))
                val idSlot = varId(name, env, p)

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

          | A.RepeatStm(tstExp, bodyStm, p) =>
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
                E.emitJump (I.not (jmpInstr (tstExp, p)), startLabel);
                E.emitLabel endLabel
            end

          | A.AssignStm(A.SimpleVar(name, _), exp, p) => (
              compileExp(exp, env);
              (* this is the variable definition *)
              instr_(I.ISTOREG, varId(name, env, p)))

          | A.PrintStm(exps, p) => (
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
                      | _ => raise TypeError("Print[" ^ T.toString(ty) ^ "T] not defined", p)
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
    fun expectType (ty, exp, env, p) =
    let
      val eTy = A.getType(exp)
      val uTy = T.unify(ty, eTy)
    in
      if eTy = uTy
        then uTy
      else if uTy = T.NOTHING
        then raise TypeError("type inference failed, expected type " ^ T.toString ty, p)
      else let
          val uTy = case exp of
              (* in case of a VarExp, update environment w/ new type *)
              A.VarExp(A.SimpleVar(name, p), _) => let
                  val vTy = case S.lookup (env, name) of
                      SOME (_, ty) => ty
                    | NONE => raise TypeError("undefined variable '" ^ S.name name ^ "'", p)
                  val uTy = T.unify(uTy, !vTy)
                in
                  if uTy = T.NOTHING
                    then raise TypeError("type inference failed, no compatible type for '" ^ S.name name ^ "' found", p)
                  else (
                    vTy := uTy;
                    uTy)
                end
            | _ => uTy
        in
          (* run inference again down the expression tree when type changes *)
          A.setType(exp, uTy);
          inferTypeExp(exp, env);
          uTy
        end
    end

    and inferTypeExp (exp as A.BoolExp(_, ty, p), env) = expectType(T.BOOL, exp, env, p)
      | inferTypeExp (exp as A.IntExp(_, ty, p), env) = expectType(T.INT, exp, env, p)
      | inferTypeExp (exp as A.StringExp(_, ty, p), env) = expectType(T.STRING, exp, env, p)
      | inferTypeExp (exp as A.VarExp(A.SimpleVar(name, p), ty), env) = let
            val vTy = case S.lookup (env, name) of
                        SOME (_, vTy) => vTy
                      | NONE => raise TypeError("undefined variable '" ^ S.name name ^ "'", p)
            val uTy = T.unify(!vTy, !ty)
          in
            if uTy = T.NOTHING
             then raise TypeError("type inference failed, no compatible type for '" ^ S.name name ^ "' found", p)
            else (
               ty := uTy;
               vTy := uTy);
            uTy
          end
      | inferTypeExp (A.RangeExp(exp1, exp2, ty, p), env) =
          let
            val e1Ty = inferTypeExp(exp1, env)
            val e2Ty = inferTypeExp(exp2, env)
            val uTy = T.unify(A.getType(exp1), A.getType(exp2))
          in
            if uTy = T.NOTHING
              then raise TypeError("type inference failed in range expression", p)
            else (
              ty := T.ARRAY(uTy);
              expectType(uTy, exp1, env, p);
              expectType(uTy, exp2, env, p));
            !ty
          end
      | inferTypeExp (A.UnOpExp(unop, exp, ty, p), env) =
          let
            val eTy = case unop of
                A.NegOp => T.INT
              | A.NotOp => T.BOOL
          in
            ty := eTy;
            expectType(eTy, exp, env, p);
            !ty
          end
      | inferTypeExp (A.BinOpExp(exp1, binop, exp2, ty, p), env) =
          let
            val (opTy, eTy) = case binop of
                A.PlusOp  => (T.INT, T.INT)
              | A.MinusOp => (T.INT, T.INT)
              | A.TimesOp => (T.INT, T.INT)
              | A.DivOp   => (T.INT, T.INT)
              | A.ModOp   => (T.INT, T.INT)
              | A.PowOp   => (T.INT, T.INT)
              | A.EqOp    => (T.BOOL, T.ANY)
              | A.NeqOp   => (T.BOOL, T.ANY)
              | A.LtOp    => (T.BOOL, T.INT)
              | A.LeOp    => (T.BOOL, T.INT)
              | A.GtOp    => (T.BOOL, T.INT)
              | A.GeOp    => (T.BOOL, T.INT)
              | A.AndOp   => (T.BOOL, T.BOOL)
              | A.OrOp    => (T.BOOL, T.BOOL)
            val e1Ty = T.unify(eTy, A.getType(exp1))
            val e2Ty = T.unify(eTy, A.getType(exp2))
            val uTy = T.unify(e1Ty, e2Ty)
          in
            if uTy = T.NOTHING
              then raise TypeError("unification of binary op failed", p)
            else (
              ty := opTy;
              expectType(uTy, exp1, env, p);
              expectType(uTy, exp2, env, p));
            !ty
          end

    fun inferTypes (A.EmptyStm(_), env: env) = env
      | inferTypes (A.ExpStm(exp), env)= env
      | inferTypes (A.PrintStm(exps, _), env) = (
          List.app (fn e => (inferTypeExp(e, env); ())) exps;
          env)
      | inferTypes (A.SeqStm(stms), env) = List.foldl inferTypes env stms
      | inferTypes (A.AssignStm(v as A.SimpleVar(name, _), exp, p), env) = let
          val vTy = varType(name, env, p)
          val eTy = inferTypeExp(exp, env)
          val uTy = T.unify(vTy, eTy)
        in
          if uTy = T.NOTHING
            then raise TypeError("unification of assignment failed", p)
          else (
            expectType(uTy, A.VarExp(v, ref vTy), env, p);
            expectType(uTy, exp, env, p));
          env
        end
      | inferTypes (A.IfStm(tstExp, thenStm, elseStm, p), env) = let
            val _ = expectType(T.BOOL, tstExp, env, p)
          in
            inferTypes(thenStm, env);
            case elseStm of
              SOME(elseStm) => inferTypes(elseStm, env)
            | NONE => env
          end
      | inferTypes (A.WhileStm(tstExp, bodyStm, p), env) = (
          expectType(T.BOOL, tstExp, env, p);
          inferTypes(bodyStm, env))
      | inferTypes (A.RepeatStm(tstExp, bodyStm, p), env) = (
          expectType(T.BOOL, tstExp, env, p);
          inferTypes(bodyStm, env))
      | inferTypes (A.ForStm(v as A.SimpleVar(name, _), rangeExp, bodyStm, p), env) = let
            val vTy = varType(name, env, p)
            val rTy = inferTypeExp(rangeExp, env)
            (*val T.ARRAY(uTy) = expectType(T.ARRAY(T.ANY), rangeExp, env, p)*)
            val uTy = case rTy of
                T.ARRAY(eTy) => T.unify(eTy, vTy)
              | _ => raise TypeError("unification in for statement failed: range type should be Array[*]", p)
          in
            if uTy = T.NOTHING
              then raise TypeError("unification in for range expression failed", p)
            else (
              expectType(uTy, A.VarExp(v, ref vTy), env, p);
              inferTypes(bodyStm, env))
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
