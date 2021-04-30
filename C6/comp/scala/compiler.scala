/*
**  compiler.scala
**  vm-comp
**
**  Created by Ovidiu Podisor on 04/05/20.
**  Copyright © 2019-2021 innodocs. All rights reserved.
*/

/*
structure Comp : sig
    type env
    exception TypeError of string

    val compile : Absyn.stm -> string -> unit
    val asm     : Absyn.Stm -> string -> unit
  end
*/
sealed trait COMP_SIG {
  type Env

  val compile : Absyn.Stm => String => Unit
  val asm     : Absyn.Stm => String => Unit
}

object Comp extends COMP_SIG {
  case class EnvEntry(id: Int, var ty: Type.Ty)
  type Env = Symbol.Table[EnvEntry]

  val compile = CompImp.compile
  val asm     = CompImp.asm
}
/*
= struct
*/
private object CompImp extends COMP_SIG {

val A = Absyn
val T = Type
val S = Symbol
val I = Instr
val SP = StringPool
val PP = PrettyPrint

//exception TypeError = T.Error
val TypeError = T.Error

/**
 * environments
 */
type Env = Comp.Env
val  EnvEntry = Comp.EnvEntry

var lastVarId = -1

def addVar(sym: S.Ty, env: Env): Env =
  env.lookup(sym) match {
    case Some(_) => env
    case None => lastVarId += 1; env.enter(sym, EnvEntry(lastVarId, T.undef))
  }

def varId(sym: S.Ty, env: Env, p: A.Pos): Int =
  env.lookup(sym) match {
    case Some(EnvEntry(i, _)) => i
    case None => throw TypeError(s"undefined variable '${S.name(sym)}'", p)
  }

def varType(sym: S.Ty, env: Env, p: A.Pos): T.Ty =
  env.lookup(sym) match {
    case Some(EnvEntry(_, ty)) => ty
    case None => throw TypeError(s"undefined variable '${S.name(sym)}'", p)
  }

/**
 *  reset the global environment
 *   -- call before compiling another program
 */
object labelId {
  var if_ = 0
  var while_ = 0
  var for_ = 0
  var repeat_ = 0
}

def resetEnv(): Env = {
  // variables
  lastVarId = -1

  // string pool
  SP.clear()

  // loop labels
  labelId.if_ = 0
  labelId.while_ = 0
  labelId.for_ = 0
  labelId.repeat_ = 0

  new Env()
}


/**
 * compile a block
 */
def compileTestExp(exp: A.Exp, env: Env, E: EMITTER) : Unit = exp match {
  case A.BinOpExp(exp1, _, exp2, _, _) =>
    compileBlock(A.ExpStm(exp1), env, E)
    compileBlock(A.ExpStm(exp2), env, E)
  case A.UnOpExp(_, exp, _, _) =>
    compileBlock(A.ExpStm(exp), env, E)
  case _ =>
    compileBlock(A.ExpStm(exp), env, E)
}

def compileBlock(prog: A.Stm, env: Env, E: EMITTER): Unit = {
/* let */
  val (instr, instr_, instr__) =
    (E.emitInstr(_:Int), E.emitInstr(_:Int, _:Int), E.emitInstr(_:Int, _:Int, _:String))

  def nextIfLabels(startP: Int, elseP: Int, endP: Int)
         : (E.Label, E.Label, E.Label) = { 
    labelId.if_ += 1
    val prefix = "if" + labelId.if_.toInt
    (E.Label(prefix + "_start", startP),
     E.Label(prefix + "_else", elseP),
     E.Label(prefix + "_end" , endP))
  }

  def nextLoopLabels(labelId: Int, labelPrefix: String, startP: Int, endP: Int) : (E.Label, E.Label) = {
    val prefix = labelPrefix + labelId.toInt
    (E.Label(prefix + "_start", startP), E.Label(prefix + "_end"  , endP))
  }
  def nextWhileLabels(startP: Int, endP: Int) = {
    labelId.while_ += 1; nextLoopLabels(labelId.while_, "while", startP, endP) }
  def nextForLabels(startP: Int, endP: Int) = {
    labelId.for_ += 1; nextLoopLabels(labelId.for_, "for", startP, endP) }
  def nextRepeatLabels(startP: Int, endP: Int) = {
    labelId.repeat_ += 1; nextLoopLabels(labelId.repeat_, "repeat", startP, endP) }

  
  def jmpInstr(exp: A.Exp, p: A.Pos): Int = exp match {
    case A.BinOpExp(_, binop, _, _, _) => binop match {
      case A.EqOp  => I.IF_ICMPEQ
      case A.NeqOp => I.IF_ICMPNE
      case A.LtOp  => I.IF_ICMPLT
      case A.LeOp  => I.IF_ICMPLE
      case A.GtOp  => I.IF_ICMPGT
      case A.GeOp  => I.IF_ICMPGE
      case A.AndOp => I.IF_INE
      case A.OrOp  => I.IF_INE
      case _       => throw TypeError("incompatible type, expected Bool", p); I.HALT
    }
    case A.UnOpExp(unop, _, _, _) => unop match {
      case A.NotOp => I.IF_INE
      case _       => throw TypeError("incompatible type, expected Bool", p); I.HALT
    }
    case _         => throw TypeError("incompatible type, expected Bool", p); I.HALT
  }

  def compileExp(exp: A.Exp, env: Env) : Unit = exp match {
    case A.VarExp(A.SimpleVar(name, p), _) => instr_(I.ILOADG, varId(name, env, p))
    case A.BoolExp(b, _, _) => instr_(I.IPUSHC, if (b) 1 else 0)
    case A.IntExp(num, _, _) => instr_(I.IPUSHC, num)
    case A.StringExp(s, _, _) => instr__(I.SPUSHC, SP.id(SP.add(s)), s)
    case A.BinOpExp(exp1, binop, exp2, _, _) => {
      compileExp(exp1, env)
      compileExp(exp2, env)
      binop match {
        case A.PlusOp  => instr(I.IADD)
        case A.MinusOp => instr(I.ISUB)
        case A.TimesOp => instr(I.IMULT)
        case A.DivOp   => instr(I.IDIV)
        case A.ModOp   => instr(I.IMOD)
        case A.PowOp   => instr(I.IPOW)
        case A.EqOp    => instr(I.ICMPEQ)
        case A.NeqOp   => instr(I.ICMPNE)
        case A.LtOp    => instr(I.ICMPLT)
        case A.LeOp    => instr(I.ICMPLE)
        case A.GtOp    => instr(I.ICMPGT)
        case A.GeOp    => instr(I.ICMPGE)
        case A.AndOp   => instr(I.IAND)
        case A.OrOp    => instr(I.IOR)      
      }
    }
    case A.UnOpExp(unop, exp, _, _) => {
      compileExp(exp, env)
      unop match {
        case A.NegOp   => instr(I.INEG)
        case A.NotOp   => instr(I.INOT)
      }
    }
    case A.RangeExp(startExp, endExp, ty, p) =>
      throw TypeError("bad expression", p)
  }
  
  def compileStm(stm: A.Stm, env: Env) : Unit = stm match {
    // compile an empty statement
    case A.EmptyStm(_) => ()

    // compile an expression statement
    case A.ExpStm(exp) => compileExp(exp, env)

    // compile a sequence of statements
    case A.SeqStm(stms) =>
      stms.foreach(stm => compileStm(stm, env))

    // compile an 'if' statement
    case A.IfStm(tstExp, thenStm, elseStmO, p) => {
      
      val tstStr = E.newMemStream()
      val tstLen = E.withStream(tstStr)(
                      (e) => compileTestExp(tstExp, env, e))
      
      val thenStr = E.newMemStream()
      val thenLen = E.withStream(thenStr)(
                      (e) => compileBlock(thenStm, env, e))
      
      val elseStr = E.newMemStream()
      val elseLen = elseStmO match {
        case Some(elseStm) =>
          E.withStream(elseStr)((e) => compileBlock(elseStm, env, e))
        case _ => 0
      }

      val (startLabel, elseLabel, endLabel) = nextIfLabels(
           // start label not needed, just a marker in the 'asm' file
           -1,
           // jump to 'else' branch: 
           //   length of 'then' branch
           // + 2: size of conditional jump instr
           // + 2: size of unconditional GOTO instr at end of 'then'
           thenLen + 2 + 2,
           // jump to end: 
           //   length of 'then' or 'else' branch
           // + 2: size of unconditional GOTO instr at end of 'then'
           //      or size of conditional jump instr when no 'else' branch
           2 + (if (elseLen > 0) elseLen else thenLen))
           
      E.emitLabel(startLabel)
      E.mergeStream(tstStr)
      E.emitJump(
          Instr.not(jmpInstr(tstExp, p)),
          if (elseLen > 0) elseLabel else endLabel)
      
      E.mergeStream(thenStr)
      if (elseLen > 0) {
        E.emitJump(I.GOTO, endLabel)
        E.emitLabel(elseLabel)
        E.mergeStream(elseStr)
      }

      E.emitLabel(endLabel)
    }

    // compile a 'while' statement
    case A.WhileStm(tstExp, bodyStm, p) =>
      
      val tstStr = E.newMemStream()
      val tstLen = E.withStream(tstStr)(
                      (e) => compileTestExp(tstExp, env, e))      
      
      val bodyStr = E.newMemStream()
      val bodyLen = E.withStream(bodyStr)(
                      (e) => compileBlock(bodyStm, env, e))
                      
      val (startLabel, endLabel) = nextWhileLabels(
           // jump to start of loop:
           //   length of test
           // + length of block
           // + 2: size of conditional jump instr          
           -(tstLen + bodyLen + 2),
           // jump to end (exit loop): 
           //   length of block
           // + 2: size of conditional jump instr
           // + 2: size of unconditional GOTO instr at end of loop          
           bodyLen + 2 + 2)

      E.emitLabel(startLabel)
      E.mergeStream(tstStr)
      E.emitJump(I.not(jmpInstr(tstExp, p)), endLabel)
      E.mergeStream(bodyStr)
      E.emitJump(I.GOTO, startLabel)
      E.emitLabel(endLabel)

    // compile a 'for' statement
    case A.ForStm(A.SimpleVar(name, _), rangeExp, bodyStm, p) =>
      val (startExp, endExp) = rangeExp match {
        case A.RangeExp(startExp, endExp, _, _) => (startExp, endExp)
        case _ => throw TypeError("range expected", p)
      }
      val startStr = E.newMemStream()
      val startLen = E.withStream(startStr)(
        (e) => compileBlock(A.ExpStm(startExp), env, e))
      val endStr = E.newMemStream()
      val endLen = E.withStream(endStr)(
        (e) => compileBlock(A.ExpStm(endExp), env, e))

      val bodyStr = E.newMemStream()
      val bodyLen = E.withStream(bodyStr)(
        (e) => compileBlock(bodyStm, env, e))

      val idSlot = varId(name, env, p)

      val (startLabel, endLabel) = nextForLabels(
        // jump to start of loop:
        //   3: load loop variable (2) + DUP (1)
        // + length of end expression
        // + 2: size of conditional jump instr
        // + length of body
        // + 5: loop variable increment
        -(3 + endLen + 2 + bodyLen + 5),
        // jump to end (exit loop):
        // + 2: size of conditional jump instr
        // + length of body
        // + 5: loop variable increment
        // + 2: size of unconditional GOTO instr at end of loop
        2 + bodyLen + 5 + 2)

      E.mergeStream(startStr)
      instr_(I.ISTOREG, idSlot)
      E.emitLabel(startLabel)
      instr_(I.ILOADG, idSlot)
      instr (I.IDUP);
      E.mergeStream(endStr)
      E.emitJump(I.IF_ICMPGT, endLabel)
      E.mergeStream(bodyStr)

      // increment loop variable
      instr_(I.IPUSHC, 1)
      //!! instr_(I.ILOADG, idSlot)
      instr(I.IADD)
      instr_(I.ISTOREG, idSlot)
      // next loop iteration
      E.emitJump(I.GOTO, startLabel)
      E.emitLabel(endLabel)
      instr(I.IPOP)

    // compile a 'repeat' statement
    case A.RepeatStm(tstExp, bodyStm, p) =>

      val tstStr = E.newMemStream()
      val tstLen = E.withStream(tstStr)(
        (e) => compileTestExp(tstExp, env, e))

      val bodyStr = E.newMemStream()
      val bodyLen = E.withStream(bodyStr)(
        (e) => compileBlock(bodyStm, env, e))

      val (startLabel, endLabel) = nextRepeatLabels(
        // jump to start of loop:
        //   length of block
        // + length of test
        -(tstLen + bodyLen),
        // jump to end (exit loop - not needed, just a marker in 'asm' file):
        //   0
        0)

      E.emitLabel(startLabel)
      E.mergeStream(bodyStr)
      E.mergeStream(tstStr)
      E.emitJump(I.not(jmpInstr(tstExp, p)), startLabel)
      E.emitLabel(endLabel)

    // compile an 'assign' statement
    case A.AssignStm(A.SimpleVar(name, p), exp, _) =>
      compileExp(exp, env)
      instr_(I.ISTOREG, varId(name, env, p)) // this is the variable definition

    // compile a 'print' statement
    case A.PrintStm(exps, p) => {
      if (exps.exists(exp => A.getType(exp) == T.STRING))
        exps.foreach(exp => {
          val ty = T.realTy(A.getType(exp))
          compileExp(exp, env)
          ty match {
            case T.INT => instr_(I.IPRINT, 1)
            case T.STRING => instr_(I.SPRINT, 1)
            case _ => throw TypeError(s"Print[$ty] not defined", p)
          }
        })
      else {
        exps.foreach(exp => compileExp(exp, env))
        instr_(I.IPRINT, exps.length)
      }
    }
  }

/* in */
  compileStm(prog, env)
/* end */
}

/**
 * compile a program
 */  
def compileProg(prog: A.Stm, E: EMITTER): Unit = {
/* let */

  /**
   *  find global definitions
   */
  def globalDefs(stm: A.Stm, env: Env): Env = stm match {
    case A.AssignStm(A.SimpleVar(name, _), _, _) => addVar(name, env)
    case A.PrintStm(exps, _) => {
      exps.foreach(exp => exp match {
        case A.StringExp (s, _, _) => SP.add(s); ()
        case _ => ()
      })
      env
    }
    case A.SeqStm(stms) => stms.foldLeft(env)((env, stm) => globalDefs(stm, env))
    case A.IfStm(tstExp, thenStm, elseStmO, _) => {
      val thenEnv = globalDefs(thenStm, env)
      elseStmO match {
        case Some(elseStm) => globalDefs(elseStm, thenEnv)
        case None => thenEnv
      }
    }
    case A.WhileStm(tstExp, bodyStm, _) => globalDefs(bodyStm, env)
    case A.ForStm(A.SimpleVar(name, _), rangeExp, bodyStm, _) => globalDefs(bodyStm, addVar(name, env))
    case A.RepeatStm(tstExp, bodyStm, _) => globalDefs(bodyStm, env)
    case _ => env
  }

  /**
   *  type inference
   */
  def expectType(ty: T.Ty, exp: A.Exp, env: Env, p: A.Pos): T.Ty = {
    val eTy = A.getType(exp)
    val uTy = T.unify(ty, eTy)

    if (eTy == uTy)
      uTy
    else if (uTy == T.NOTHING)
      throw TypeError(s"type inference failed, expected type $ty", p)
    else {
      val uTy2 = exp match {
        // in case of a VarExp, update environment w/ new type
        case A.VarExp(A.SimpleVar(name, p), _) => {
          val v = env.lookup(name) match {
            case Some(v) => v
            case None => throw TypeError(s"undefined variable '${S.name(name)}'", p)
          }

          val uTy3 = T.unify(uTy, v.ty)
          if (uTy3 == T.NOTHING)
            throw TypeError(s"type inference failed, no compatible type for '${S.name(name)}' found", p)
          else {
            v.ty = uTy3
            uTy3
          }
        }
        case _ => uTy
      }

      // run inference again down the expression tree when type changes
      A.setType(exp, uTy2)
      inferTypeExp(exp, env)
      uTy2
    }
  }

  def inferTypeExp(exp: A.Exp, env: Env): T.Ty = exp match {
    case A.BoolExp(_, ty, p) => expectType(T.BOOL, exp, env, p)
    case A.IntExp(_, ty, p) => expectType(T.INT, exp, env, p)
    case A.StringExp(_, ty, p) => expectType(T.STRING, exp, env, p)
    case e@A.VarExp(A.SimpleVar(name, p), ty) => {
      val v = env.lookup(name) match {
        case Some(v) => v
        case None => throw TypeError(s"undefined variable '${S.name(name)}'", p)
      }
      val uTy = T.unify(v.ty, ty)

      if (uTy == T.NOTHING)
        throw TypeError(s"type inference failed, no compatible type for '${S.name(name)}' found", p)
      else {
        e.ty = uTy
        v.ty = uTy
        e.ty
      }
    }
    case e@A.RangeExp(exp1, exp2, ty, p) => {
      val e1Ty = inferTypeExp(exp1, env)
      val e2Ty = inferTypeExp(exp2, env)
      val uTy = T.unify(e1Ty, e2Ty)

      if (uTy == T.NOTHING)
        throw TypeError("type inference failed in range expression", p)
      else {
        e.ty = T.ARRAY(uTy)
        expectType(uTy, exp1, env, p)
        expectType(uTy, exp2, env, p)
        e.ty
      }
    }
    case e@A.UnOpExp(unop, exp, ty, p) => {
      val eTy = unop match {
        case A.NegOp => T.INT
        case A.NotOp => T.BOOL
      }

      e.ty = eTy
      expectType(eTy, exp, env, p)
      e.ty
    }
    case e@A.BinOpExp(exp1, binop, exp2, ty, p) => {
      val (opTy, eTy) = binop match {
        case A.PlusOp => (T.INT, T.INT)
        case A.MinusOp => (T.INT, T.INT)
        case A.TimesOp => (T.INT, T.INT)
        case A.DivOp => (T.INT, T.INT)
        case A.ModOp => (T.INT, T.INT)
        case A.PowOp => (T.INT, T.INT)
        case A.EqOp => (T.BOOL, T.ANY)
        case A.NeqOp => (T.BOOL, T.ANY)
        case A.LtOp => (T.BOOL, T.INT)
        case A.LeOp => (T.BOOL, T.INT)
        case A.GtOp => (T.BOOL, T.INT)
        case A.GeOp => (T.BOOL, T.INT)
        case A.AndOp => (T.BOOL, T.BOOL)
        case A.OrOp => (T.BOOL, T.BOOL)
      }

      val e1Ty = T.unify(eTy, inferTypeExp(exp1, env))
      val e2Ty = T.unify(eTy, inferTypeExp(exp2, env))
      val uTy = T.unify(e1Ty, e2Ty)

      if (uTy == T.NOTHING)
        throw TypeError("unification of binary op failed", p)
      else {
        e.ty = opTy
        expectType(uTy, exp1, env, p)
        expectType(uTy, exp2, env, p)
        e.ty
      }
    }
  }

  def inferTypes(stm: A.Stm, env: Env): Env = stm match {
    case A.EmptyStm(_) => env
    case A.ExpStm(_) => env
    case A.PrintStm(exps, _) => exps.foreach(exp => inferTypeExp(exp, env)); env //??
    case A.SeqStm(stms) => stms.foldLeft(env)((env, stm) => inferTypes(stm, env))
    case A.AssignStm(v@A.SimpleVar(name, _), exp, p) => {
      val vTy = varType(name, env, p)
      val eTy = inferTypeExp(exp, env)
      val uTy = T.unify(vTy, eTy)

      if (uTy == T.NOTHING)
        throw TypeError("unification of assignment failed", p)
      else {
        expectType(uTy, A.VarExp(v, vTy), env, p)
        expectType(uTy, exp, env, p)
      }
      env
    }
    case A.IfStm(tstExp, thenStm, elseStm, p) => {
      expectType(T.BOOL, tstExp, env, p)
      inferTypes(thenStm, env)
      elseStm match {
        case Some(elseStm) => inferTypes(elseStm, env)
        case None => env
      }
    }
    case A.WhileStm(tstExp, bodyStm, p) => {
      expectType(T.BOOL, tstExp, env, p)
      inferTypes(bodyStm, env)
    }
    case A.RepeatStm(tstExp, bodyStm, p) => {
      expectType(T.BOOL, tstExp, env, p)
      inferTypes(bodyStm, env)
    }
    case A.ForStm(v@A.SimpleVar(name, _), rangeExp, bodyStm, p) => {
      val vTy = varType(name, env, p)
      val rTy = inferTypeExp(rangeExp, env)
      /* val T.ARRAY (uTy) = expectType (T.ARRAY (T.ANY), rangeExp, env, p) */
      val uTy = rTy match {
        case T.ARRAY(eTy) => T.unify(eTy, vTy)
        case _ => throw TypeError("unification in for statement failed: range type should be Array[*]", p)
      }
      if (uTy == T.NOTHING)
        throw TypeError("unification in for range expression failed", p)
      else {
        expectType(uTy, A.VarExp(v, vTy), env, p)
        inferTypes(bodyStm, env)
      }
    }
  }

/* in */
  val globalsEnv = inferTypes(prog, globalDefs(prog, resetEnv()))
  val nrStrings = SP.size()

  // file header
  E.emitMeta(I.MAGIC, I.MAJOR_VERSION, I.MINOR_VERSION)
  E.emitInstr(I.GLOBALS, globalsEnv.size)  // nr globals
  E.emitInstr(I.STRINGS, nrStrings)        // nr strings
  if (nrStrings > 0) E.emitStrings()

  compileBlock(prog, globalsEnv, E)

  // file ending
  E.emitInstr(I.HALT)
/* end */
}

/**
 * vm file format output of program
 */
val compile : Absyn.Stm => String => Unit = prog => { codeFileName => {
  val out = BinIO.openOut(codeFileName)
  VMFEmitter.withIOStream(out)((E) => compileProg(prog, E))
  BinIO.closeOut(out);
}}

/**
 * assembly output of program
 */
val asm : Absyn.Stm => String => Unit = prog => { codeFileName => {
  val out = TextIO.openOut(codeFileName)
  AsmEmitter.withIOStream(out)((E) => compileProg(prog, E))
  TextIO.closeOut(out)
}}

/* end (* structure Comp *) */
}
