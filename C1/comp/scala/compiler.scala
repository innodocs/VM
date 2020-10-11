/*
**  compiler.scala
**  vm-comp
**
**  Created by Ovidiu Podisor on 02/15/20.
**  Copyright © 2020 innodocs. All rights reserved.
*/

/*
structure Comp : sig
     val print   : Absyn.stm -> unit
     val eval    : Absyn.stm -> (Absyn.id * int) list
     val compile : Absyn.stm -> string -> unit
  end
*/

sealed trait COMP_SIG {
  
  val print   : Absyn.Stm => Unit
  val eval    : Absyn.Stm => Unit
  val compile : Absyn.Stm => String => Unit
}

object Comp extends COMP_SIG {

  override val print   = CompImp.print
  override val eval    = CompImp.eval
  override val compile = CompImp.compile
}

/*
 * = struct
 */
private object CompImp extends COMP_SIG {

/* open Absyn */
import Absyn._

/*
 * ids
 */
var lastId = -1
def newId() : Int = { lastId = lastId + 1; return lastId }

/*
 * environments
 */
type Env = List[(Id, Int)]
def resetEnv() = lastId = -1

def lookup(id: Id, env: Env) : Int = env match {
      case (i, v) :: tl => if (id == i) v else lookup(id, tl)
      case Nil          => -1
}

/*
 * printing
 */
def printString(s: String) = Console.print(s)
def printInt(i: Int) = Console.print(i)
def printNL() = Console.println()

/*
 * print a statement
 */
val print : Stm => Unit = prog => {
  /* let */
 
  def printStm(stm: Stm) : Unit = stm match {
    case CompoundStm(stm1, stm2) => printStm(stm1); printString(";"); printNL(); printStm(stm2)
    case AssignStm(id, exp)      => printString(id); printString(" := "); printExp(exp)
    case PrintStm(exps)          => printString("print("); printExps(exps); printString(")")
  }
  
  def printExps(exps: List[Exp]) : Unit = exps match {
    case e :: tl => printExp(e); tl.foreach(e => { printString(", "); printExp(e) })
    case Nil     => ()
  }    

  def printExp(exp: Exp) : Unit = {
    /* let */
    def openParen(prec: Int, opPrec: Int) = if (prec > opPrec) printString("(")
    def closeParen(prec: Int, opPrec: Int) = if (prec > opPrec) printString(")")

    def print(prec: Int, exp: Exp) : Unit = exp match {
      case IdExp(id) => printString(id)
      case NumExp(num) => printInt(num)
      case OpExp(exp1, Plus, exp2)  => openParen(prec, 0); print(0, exp1);
                                            printString("+"); print(0, exp2); closeParen(prec, 0)
      case OpExp(exp1, Minus, exp2) => openParen(prec, 1); print(1, exp1);
                                            printString("-"); print(1, exp2); closeParen(prec, 1)
      case OpExp(exp1, Times, exp2) => openParen(prec, 2); print(2, exp1);
                                            printString("*"); print(2, exp2); closeParen(prec, 2)
      case OpExp(exp1, Div, exp2)   => openParen(prec, 3); print(3, exp1);
                                            printString("/"); print(3, exp2); closeParen(prec, 3)
      case EseqExp(stm, exp)        => printString("("); printStm(stm); printString(", ");
                                            printExp(exp); printString(")")
    }
    
    /* in */
    print(0, exp)
  }

  /* in */
  printStm(prog);
  printNL()
}


/*
 * evaluate a statement
 */
val eval : Stm => Unit = prog => {
  
  /* let */
  def evalStm(stm: Stm, env: Env) : Env = stm match {
    case CompoundStm(stm1, stm2)  => evalStm(stm2, evalStm(stm1, env))
    case AssignStm(id, exp) => {
      val (v, newEnv) = evalExp(exp, env);
      (id, v) :: newEnv }
    case PrintStm(exps) =>
      exps.foldLeft(env) { (env, exp) => {
        val (v, newEnv) = evalExp(exp, env)
        printInt(v); println()
        newEnv }}
  }

  def evalExp(exp: Exp, env: Env) : (Int, Env) = exp match {
    case IdExp(id) => {
      val b = env.find(t => { val (i, v) = t; i == id})
      val v = b match {
        case Some((_, v)) => v
        case _            => 0 }
      (v, env) }
    case NumExp(i) => (i, env)
    case OpExp(e1, binop, e2) => {
      val (v1, env1) = evalExp(e1, env)
      val (v2, env2) = evalExp(e2, env1)
      val v  = binop match {
        case Plus  => v1 + v2
        case Minus => v1 - v2
        case Times => v1 * v2
        case Div   => v1 / v2 }
      (v, env2) }
    case EseqExp(stm, exp) => evalExp(exp, evalStm(stm, env))
  }
  
  evalStm(prog, Nil)
}


/*
 * compile a statement
 */
object BinIO {
  import java.io._
  
  def openOut(fileName: String) : OutputStream = new BufferedOutputStream(
                                                    new FileOutputStream(fileName))
  def closeOut(os: OutputStream) : Unit = os.close()
  
  type Word = Int
  def output(os: OutputStream, w: Word) = {
    os.write(w >> 24 & 0xFF)
    os.write(w >> 16 & 0xFF)
    os.write(w >>  8 & 0xFF)
    os.write(w       & 0xFF)
  }
}

val compile : Stm => String => Unit = prog => { codeFileName => {
  
  /* let */
  
	// code file markers
	val MAGIC         = 0x12345678
	val MAJOR_VERSION = 0x00000001
	val MINOR_VERSION = 0x00000001
	
	// instruction set	
  val NOP     = 0x00
  val HALT    = 0x01

  // load/save globals
  val GLBLS   = 0x10
  val ILOADG  = 0x11
  val ISTOREG = 0x12

  // stack ops
  val IPUSHC  = 0x20
  val IPOP    = 0x21
  val ISWAP   = 0x22
  val IDUP    = 0x23

  // arithmetic ops
  val IADD    = 0x40
  val ISUB    = 0x41
  val IMULT   = 0x42
  val IDIV    = 0x43
  val IMOD    = 0x44

  // unary ops
  val INEG    = 0x60

  // built-in functions
  val IPRINT  = 0x100
  
  
  val out = BinIO.openOut(codeFileName)
  def instr(i: BinIO.Word) = BinIO.output(out, i)

  def globalsExp(exp: Exp, env: Env) : Env = exp match {
    case IdExp(id) => env
    case NumExp(num) => env
    case OpExp(exp1, binop, exp2) => globalsExp(exp2, globalsExp(exp1, env))
    case EseqExp(stm, exp) => globalsExp(exp, globalsStm(stm, env))
  }
  
  def globalsStm(stm: Stm, env: Env) : Env = stm match {
    case CompoundStm(stm1, stm2) =>
      globalsStm(stm2, globalsStm(stm1, env))
    case AssignStm(name, exp) =>
      val newEnv = globalsExp(exp, env)
      if (lookup(name, newEnv) != -1) newEnv else (name, newId()) :: newEnv      
    case PrintStm(exps) =>
      exps.foldLeft(env)((env, exp) => globalsExp(exp, env))
  }
 
  def compileExp(exp: Exp, env: Env) : Unit = exp match {
    case IdExp(id) => instr(ILOADG); instr(lookup(id, env))
    case NumExp(num) => instr(IPUSHC); instr(num)
    case OpExp(exp1, binop, exp2) => {
      compileExp(exp1, env); compileExp(exp2, env);
      binop match {
        case Plus  => instr(IADD)
        case Minus => instr(ISUB)
        case Times => instr(IMULT)
        case Div   => instr(IDIV) } }
    case EseqExp(stm, exp) => compileStm(stm, env); compileExp(exp, env)
  }

  def compileStm(stm: Stm, env: Env) : Unit = stm match {
      case CompoundStm(stm1, stm2) => compileStm(stm1, env); compileStm(stm2, env)
      case AssignStm(id, exp) => compileExp(exp, env); instr(ISTOREG); instr(lookup(id, env))
      case PrintStm(exps) => { exps.foreach(exp => compileExp(exp, env));
                       instr(IPRINT); instr(exps.length) }
  }

  val _ = resetEnv()
  val globalsEnv = globalsStm(prog, Nil)
  val env = globalsEnv
  
  /* in */
  instr(MAGIC); instr(MAJOR_VERSION); instr(MINOR_VERSION);
  instr(GLBLS); instr(globalsEnv.length); // nr globals
  compileStm(prog, env);
  instr(HALT);
  BinIO.closeOut(out)
  
  /* end */
}}


/*
end (* structure Comp *)
*/
}
