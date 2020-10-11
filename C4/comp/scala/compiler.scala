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

    val print   : Absyn.stm -> unit
    val eval    : Absyn.stm -> env
    val compile : Absyn.stm -> string -> unit
    val asm     : Absyn.Stm -> string -> unit
  end
*/

sealed trait COMP_SIG {
  type Env
  type TypeError
  
  val print   : Absyn.Stm => Unit
  val eval    : Absyn.Stm => Env
  val compile : Absyn.Stm => String => Unit
  val asm     : Absyn.Stm => String => Unit
}

object Comp extends COMP_SIG {
  
  type Env = List[(Absyn.Id, Int)]
  case class TypeError(s: String) extends Exception(s)

  val print   = CompImp.print
  val eval    = CompImp.eval
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
var ifLabelId = 0
var whileLabelId = 0

def resetEnv() = {
  lastId = -1
  ifLabelId = 0
  whileLabelId = 0
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


/*
 * printing
 */
def printString(s: String) = Console.print(s)
def printInt(i: Int) = Console.print(i)
def printNL() = Console.println()


/*
 * print a statement
 */
val print : Absyn.Stm => Unit = prog => {
  /* let */

  def printStm(stm: A.Stm, l: Int) : Unit = {
    /* let */
    def indent(level: Int): Unit = for (l <- 1 to level*2) { printString(" ") }
    
    /* in */
    stm match {
    case A.SeqStm(stms) => stms.foreach(s => printStm(s, l))
    
    case A.EmptyStm(stms) => printString(";")
    
    case A.ExpStm(exp) => printExp(exp)
    
    case A.IfStm(tst, t, eo, _) =>
            printNL(); indent(l)
            printString("if "); printExp(tst); printString(" then")
            printStm(t, l+1)
            eo match {
              case Some(e) => indent(l); printNL(); printString("else"); printStm(e, l+1)
              case _ => ()
            }
            printNL(); indent(l); printString("fi;")
            
    case A.WhileStm(tst, b, pos) =>
            printNL(); indent(l)
            printString("while "); printExp(tst); printString(" do")
            printStm(b, l+1);
            printNL(); indent(l); printString("od;")
    
    case A.AssignStm(v, exp, pos) =>
            printNL(); indent(l)
            printVar(v); printString(" := "); printExp(exp); printString(";")
    
    case A.PrintStm(exps, pos) =>
            printNL(); indent(l)
            printString("Print(")
            exps match {
            case e :: tl => printExp(e); tl.foreach(e => { printString(", "); printExp(e) })
            case Nil     => ()
            }
            printString(");")
    }
  }
  
  def printExp(exp: A.Exp) : Unit = {
    /* let */
    def openParen(prec: Int, opPrec: Int) = if (prec > opPrec) printString("(")
    def closeParen(prec: Int, opPrec: Int) = if (prec > opPrec) printString(")")
    
    def printBinOp(prec: Int, exp1: A.Exp, binOp: A.BinOp, exp2: A.Exp): Unit = {
      val (opStr, opPrec) = binOp match {
        case A.PlusOp  => (" + ", 4)
        case A.MinusOp => (" - ", 5)
        case A.TimesOp => (" * ", 6)
        case A.DivOp   => (" / ", 7)
        case A.ModOp   => (" mod ", 8)
        case A.PowOp   => ("^", 9)
        case A.EqOp    => (" = ", 3)
        case A.NeqOp   => (" <> ", 3)
        case A.LtOp    => (" < ", 2)
        case A.LeOp    => (" <= ", 2)
        case A.GtOp    => (" > ", 2)
        case A.GeOp    => (" >= ", 2)
        case A.AndOp   => (" and ", 0)
        case A.OrOp    => (" or ", 0)
      }
      openParen(prec, opPrec);
      print(opPrec, exp1); printString(opStr); print(opPrec, exp2);
      closeParen(prec, opPrec) 
    }
    def printUnOp(prec: Int, unOp: A.UnOp, exp: A.Exp) = {
      val (opStr, opPrec) = unOp match {
        case A.NotOp => ("not", 10)
        case A.NegOp => ("-", 1)
      }
      openParen(prec, opPrec)
      printString(opStr); print(opPrec, exp)
      closeParen(prec, opPrec) 
    }

    def print(prec: Int, exp: A.Exp) : Unit = exp match {
      case A.VarExp(v) => printVar(v)
      case A.NumExp(num, pos) => printInt(num)
      case A.BinOpExp(exp1, binOp, exp2, pos)  => printBinOp(prec, exp1, binOp, exp2)
      case A.UnOpExp(unOp, exp, pos)  => printUnOp(prec, unOp, exp)
    }
    
    /* in */
    print(0, exp)
  }
  
  def printVar(v: A.Var) : Unit = v match {
    case A.SimpleVar(id, pos) => printString(id)
  }  

/* in */
  printStm(prog, 0)
  printNL()
/* end */
}


/**
 * evaluate a statement
 */
val eval : Absyn.Stm => Comp.Env = prog => {

  /* let */
  def evalStm(stm: A.Stm, env: Env): Env = stm match {
    case A.SeqStm(stms) =>
      stms.foldLeft(env)((env, stm) => evalStm(stm, env))
    
    case A.EmptyStm(_)  => env
    
    case A.ExpStm(exp)  => evalExp(exp, env)._2
    
    case A.IfStm(tst, t, eo, _) => {
      val (v, newEnv) = evalExp(tst, env)
      
      if (v != 0)
        evalStm(t, newEnv)
      else eo match {
        case Some(e) => evalStm(e, newEnv)
        case _ => newEnv
      }
    }

    case A.WhileStm(tst, b, _) => {
      var (v, newEnv) = evalExp(tst, env)
      while (v != 0) {
        val (tstV, tstEnv) = evalExp(tst, evalStm(b, newEnv))
        v = tstV
        newEnv = tstEnv
      }
      newEnv
    }

    case A.AssignStm(A.SimpleVar(id, _), exp, _) => {
      val (v, newEnv) = evalExp(exp, env)
      (id, v) :: newEnv
    }
    
    case A.PrintStm(exps, _) => {
      val newEnv = exps match {
        case hdExp :: tlExps => {
          val (hdV, hdEnv) = evalExp(hdExp, env)
          printString(hdV.toString)
          tlExps.foldLeft(hdEnv)((env, exp) => {
            val (v, expEnv) = evalExp(exp, env)
            printString(", " + v.toString);
            expEnv
          })
        }
        case _ => env
      }
      printString("\n")
      newEnv
    }
  }

  def evalExp(exp: A.Exp, env: Env): (Int, Env) = exp match {
      
    case A.VarExp(A.SimpleVar(id, _)) => {
      env.find((e) => e._1 == id) match {
        case Some((_, v)) => (v, env)
        case None       => (0, env) /* throw "not found" ?? */
      }
    }

    case A.NumExp(i, _) => (i, env)
    
    case A.BinOpExp(exp1, binop, exp2, _) => {
      val (v1, env1) = evalExp(exp1, env)
      val (v2, env2) = evalExp(exp2, env1)
      val v = binop match {
        case A.PlusOp  => v1 + v2
        case A.MinusOp => v1 - v2
        case A.TimesOp => v1 * v2
        case A.DivOp   => v1 / v2
        case A.ModOp   => v1 % v2
        case A.PowOp   => Math.pow(v1, v2).toInt
        case A.EqOp    => if (v1 == v2) 1 else 0
        case A.NeqOp   => if (v1 != v2) 1 else 0
        case A.LtOp    => if (v1 <  v2) 1 else 0
        case A.LeOp    => if (v1 <= v2) 1 else 0
        case A.GtOp    => if (v1 >  v2) 1 else 0
        case A.GeOp    => if (v1 >= v2) 1 else 0
        case A.AndOp   => if (v1 != 0 && v2 != 0) 1 else 0
        case A.OrOp    => if (v1 != 0 || v2 != 0) 1 else 0
      }
      
      (v, env2)
    }
    
    case A.UnOpExp(unop, exp, _) => {
      val (v1, env1) = evalExp(exp, env)
      val v = unop match {
        case A.NotOp  => if (v1 != 0) 0 else 1
        case A.NegOp  => -v1
      }

      (v, env1)
    }
  }
  
/* in */
  evalStm(prog, Nil)
/* end */
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
    ifLabelId = ifLabelId + 1
    val prefix = "If" + ifLabelId.toInt
    (E.Label(prefix + "Start", startP),
     E.Label(prefix + "Else", elseP),
     E.Label(prefix + "End" , endP))
  }

  def nextWhileLabels(startP: Int, endP: Int) : (E.Label, E.Label) = { 
    whileLabelId = whileLabelId + 1
    val prefix = "While" + whileLabelId.toInt
    (E.Label(prefix + "Start", startP),
     E.Label(prefix + "End"  , endP))
  }
  
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
  }
  
  def compileStm(stm: A.Stm, env: Env) : Unit = stm match {
    case A.EmptyStm(_) => ()
    
    case A.ExpStm(exp) => compileExp(exp, env)
    
    case A.SeqStm(stms) =>
      stms.foreach(stm => compileStm(stm, env))
      
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
      
    case A.AssignStm(A.SimpleVar(id, _), exp, _) =>
      compileExp(exp, env)
      instr_(I.ISTOREG, lookup(id, env))
      
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
