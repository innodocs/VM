/*
**  compiler.scala
**  vm-comp
**
**  Created by Ovidiu Podisor on 02/28/20.
**  Copyright © 2020 innodocs. All rights reserved.
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
  type TypeError

  val compile : Absyn.Stm => String => Unit
  val asm     : Absyn.Stm => String => Unit
}

object Comp extends COMP_SIG {
  type Env = List[(Absyn.Id, Int)]
  case class TypeError(s: String) extends Exception(s)

  val compile = CompImp.compile
  val asm     = CompImp.asm
}

/*
 * = struct
 */
private object CompImp extends COMP_SIG {

/*
structure A = Absyn
structure I = Instr
*/
val A = Absyn
val I = Instr

//type TypeError = Comp.TypeError
case class TypeError(s: String) extends Exception(s)

/**
 * ids
 */
var lastId = -1

object labelId {
  var if_ = 0
  var while_ = 0
  var for_ = 0
  var repeat_ = 0
}

def resetEnv() = {
  lastId = -1

  labelId.if_ = 0;
  labelId.while_ = 0
  labelId.for_ = 0
  labelId.repeat_ = 0
}

/**
 * environments
 */
type Env = Comp.Env

def newId() : Int = { lastId = lastId + 1; lastId }

def lookup(id: A.Id, env: Env): Int = env match {
      case (i, v) :: tl => if (id == i) v else lookup(id, tl)
      case Nil          => -1
}

/**
 * compile a block
 */
def compileTestExp(exp: A.Exp, env: Env, E: EMITTER) : Unit = exp match {
  case A.BinOpExp(exp1, _, exp2, _) =>
    compileBlock(A.ExpStm(exp1), env, E)
    compileBlock(A.ExpStm(exp2), env, E)
  case A.UnOpExp(_, exp, _) =>
    compileBlock(A.ExpStm(exp), env, E)
  case _ =>
    compileBlock(A.ExpStm(exp), env, E)
}

def compileBlock(prog: A.Stm, env: Env, E: EMITTER): Unit = {

  /* let */
  val (instr, instr_) = (E.emitInstr(_:Int), E.emitInstr(_:Int, _:Int)) 
  
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

  
  def jmpInstr(exp: A.Exp): Int = exp match {
    case A.BinOpExp(_, binop, _, _) => binop match {
      case A.EqOp  => I.IF_ICMPEQ
      case A.NeqOp => I.IF_ICMPNE
      case A.LtOp  => I.IF_ICMPLT
      case A.LeOp  => I.IF_ICMPLE
      case A.GtOp  => I.IF_ICMPGT
      case A.GeOp  => I.IF_ICMPGE
      case A.AndOp => I.IF_INE
      case A.OrOp  => I.IF_INE
      case _       => throw TypeError("expected <bool>"); I.HALT
    }
    case A.UnOpExp(unop, _, _) => unop match {
      case A.NotOp => I.IF_INE
      case _       => throw TypeError("expected <bool>"); I.HALT
    }
    case _         => throw TypeError("expected <bool>"); I.HALT
  }
  
  
  def compileExp(exp: A.Exp, env: Env) : Unit = exp match {
    case A.VarExp(A.SimpleVar(id, _)) => instr_(I.ILOADG, lookup(id, env))
    case A.NumExp(num, _) => instr_(I.IPUSHC, num)
    case A.BinOpExp(exp1, binop, exp2, _) => {
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
    case A.UnOpExp(unop, exp, _) => {
      compileExp(exp, env)
      unop match {
        case A.NegOp   => instr(I.INEG)
        case A.NotOp   => instr(I.INOT)
      }
    }
    case A.RangeExp(startExp, endExp, _) =>
      throw TypeError("bad expression")
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
    case A.IfStm(tstExp, thenStm, elseStmO, _) => {
      
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
          Instr.not(jmpInstr(tstExp)), 
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
    case A.WhileStm(tstExp, bodyStm, _) =>
      
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
      E.emitJump(I.not(jmpInstr(tstExp)), endLabel)
      E.mergeStream(bodyStr)
      E.emitJump(I.GOTO, startLabel)
      E.emitLabel(endLabel)

    // compile a 'for' statement
    case A.ForStm(A.SimpleVar(id, _), rangeExp, bodyStm, _) =>

      val (startExp, endExp) = rangeExp match {
        case A.RangeExp(startExp, endExp, _) => (startExp, endExp)
        case _ => throw TypeError("range exected")
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

      val idSlot = lookup(id, env)

      val (startLabel, endLabel) = nextForLabels(
        // jump to start of loop:
        //   2: load loop variable
        // + length of end expression
        // + 2: size of conditional jump instr
        // + length of body
        // + 7: loop variable increment
        -(2 + endLen + 2 + bodyLen + 7),
        // jump to end (exit loop):
        // + 2: size of conditional jump instr
        // + length of body
        // + 7: loop variable increment
        // + 2: size of unconditional GOTO instr at end of loop
        2 + bodyLen + 7 + 2)

      E.mergeStream(startStr)
      instr_(I.ISTOREG, idSlot)
      E.emitLabel(startLabel)
      instr_(I.ILOADG, idSlot)
      E.mergeStream(endStr)
      E.emitJump(I.IF_ICMPGT, endLabel)
      E.mergeStream(bodyStr)

      // increment loop variable
      instr_(I.IPUSHC, 1)
      instr_(I.ILOADG, idSlot)
      instr(I.IADD)
      instr_(I.ISTOREG, idSlot)
      // next loop iteration
      E.emitJump(I.GOTO, startLabel)
      E.emitLabel(endLabel)

    // compile a 'repeat' statement
    case A.RepeatStm(tstExp, bodyStm, _) =>

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
      E.emitJump(I.not(jmpInstr(tstExp)), startLabel)
      E.emitLabel(endLabel)

    // compile an 'assign' statement
    case A.AssignStm(A.SimpleVar(id, _), exp, _) =>
      compileExp(exp, env)
      instr_(I.ISTOREG, lookup(id, env))

    // compile a 'print' statement
    case A.PrintStm(exps, _) =>
      exps.foreach(exp => compileExp(exp, env))
      instr_(I.IPRINT, exps.length)
  }
  
  /* in */
  compileStm(prog, env)
}

/**
 * compile a program
 */  
def compileProg(prog: A.Stm, E: EMITTER): Unit = {

  /* let */
  def globalsExp(exp: A.Exp, env: Env) : Env = exp match {
    case A.VarExp(v) => env
    case A.NumExp(num, _) => env
    case A.BinOpExp(exp1, binop, exp2, _) => globalsExp(exp2, globalsExp(exp1, env))
    case A.UnOpExp(unop, exp, _) => globalsExp(exp, env)
    case A.RangeExp(startExp, endExp, _) => globalsExp(endExp, globalsExp(startExp, env))
  }
  
  def globalsStm(stm: A.Stm, env: Env) : Env = stm match {
    case A.EmptyStm(_) => env
    case A.ExpStm(exp) => globalsExp(exp, env)
    case A.SeqStm(stms) =>
      stms.foldLeft(env)((env, stm) => globalsStm(stm, env))
    case A.IfStm(tstExp, thenStm, elseStmO, _) => {
      val thenEnv = globalsStm(thenStm, globalsExp(tstExp, env))
      elseStmO match {
        case Some(elseStm) => globalsStm(elseStm, thenEnv)
        case _ => thenEnv
      }
    }
    case A.WhileStm(tstExp, bodyStm, _) =>
      globalsStm(bodyStm, globalsExp(tstExp, env))
    case A.ForStm(A.SimpleVar(id, _), rangeExp, bodyStm, _) =>
      globalsStm(bodyStm, globalsExp(rangeExp, env))
    case A.RepeatStm(tstExp, bodyStm, _) =>
      globalsStm(bodyStm, globalsExp(tstExp, env))
    case A.AssignStm(A.SimpleVar(name, _), exp, _) =>
      val newEnv = globalsExp(exp, env)
      if (lookup(name, newEnv) != -1) newEnv else (name, newId()) :: newEnv
    case A.PrintStm(exps, _) =>
      exps.foldLeft(env)((env, exp) => globalsExp(exp, env))
  }
  
  val _ = resetEnv()
  val globalsEnv = globalsStm(prog, Nil)
  
  /* in */
  E.emitMeta(I.MAGIC, I.MAJOR_VERSION, I.MINOR_VERSION);
  E.emitInstr(I.GLBLS, globalsEnv.length); // nr globals
  compileBlock(prog, globalsEnv, E);
  E.emitInstr(I.HALT);
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

/*
end (* structure Comp *)
*/
}
