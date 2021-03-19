/*
**  pp.scala
**  vm-comp
**
**  Created by Ovidiu Podisor on 03/15/20.
**  Copyright © 2020 innodocs. All rights reserved.
*/

/*
signature PRETTYPRINT =
sig
  val print : Absyn.stm -> unit

  val printString: string -> unit
  val printInt: int -> unit
  val printNL: unit -> unit
end
*/
sealed trait PRETTYPRINT_SIG {
  val print: Absyn.Stm => Unit

  val printString: String => Unit
  val printInt: Int => Unit
  val printNL: () => Unit
}

/*
structure PrettyPrint : PRETTYPRINT
*/
object PrettyPrint extends PRETTYPRINT_SIG {
  val print = PrettyPrintImp.print

  val printString = PrettyPrintImp.printString
  val printInt = PrettyPrintImp.printInt
  val printNL = PrettyPrintImp.printNL
}

/*
 * = struct
 */
private object PrettyPrintImp extends PRETTYPRINT_SIG {

/*
structure A = Absyn
*/
val A = Absyn

/*
 * printing
 */
val printString: String => Unit = s => Console.print(s)
val printInt: Int => Unit = i => Console.print(i)
val printNL: () => Unit = Console.println

/*
 * print a statement
 */
val print: Absyn.Stm => Unit = prog => {
  /* let */
  val ps = printString
  val pi = printInt
  val nl = printNL
  /* in */

  def printStm(stm: A.Stm, l: Int) : Unit = {
    /* let */
    def indent(level: Int): Unit = for (l <- 1 to level*2) { ps(" ") }

    /* in */
    stm match {
      case A.SeqStm(stms) => stms.foreach(s => printStm(s, l))

      case A.EmptyStm(stms) => ps(";")

      case A.ExpStm(exp) => printExp(exp)

      case A.IfStm(tst, t, eo, _) =>
        nl(); indent(l)
        ps("if "); printExp(tst); ps(" then")
        printStm(t, l+1)
        eo match {
          case Some(e) => indent(l); nl(); ps("else"); printStm(e, l+1)
          case _ => ()
        }
        nl(); indent(l); ps("fi;")

      case A.WhileStm(tst, b, pos) =>
        nl(); indent(l)
        ps("while "); printExp(tst); ps(" do")
        printStm(b, l+1)
        nl(); indent(l); ps("od;")

      case A.ForStm(v, r, b, pos) =>
        nl(); indent(l)
        ps("for "); printVar(v); ps(" in "); printExp(r); ps(" do")
        printStm(b, l+1)
        nl(); indent(l); ps("od;")

      case A.RepeatStm(tst, b, pos) =>
        nl(); indent(l); ps("repeat")
        printStm(b, l+1)
        nl(); ps("until "); printExp(tst); ps(";");

      case A.AssignStm(v, exp, pos) =>
        nl(); indent(l)
        printVar(v); ps(" := "); printExp(exp); ps(";")

      case A.PrintStm(exps, pos) =>
        nl(); indent(l)
        ps("Print(")
        exps match {
          case e :: tl => printExp(e); tl.foreach(e => { ps(", "); printExp(e) })
          case Nil     => ()
        }
        ps(");")
    }
  }

  def printExp(exp: A.Exp) : Unit = {
    /* let */
    def openParen(prec: Int, opPrec: Int) = if (prec > opPrec) ps("(")
    def closeParen(prec: Int, opPrec: Int) = if (prec > opPrec) ps(")")

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
      print(opPrec, exp1); ps(opStr); print(opPrec, exp2);
      closeParen(prec, opPrec)
    }
    def printUnOp(prec: Int, unOp: A.UnOp, exp: A.Exp) = {
      val (opStr, opPrec) = unOp match {
        case A.NotOp => ("not", 10)
        case A.NegOp => ("-", 1)
      }
      openParen(prec, opPrec)
      ps(opStr); print(opPrec, exp)
      closeParen(prec, opPrec)
    }

    def print(prec: Int, exp: A.Exp) : Unit = exp match {
      case A.VarExp(v) => printVar(v)
      case A.NumExp(num, pos) => pi(num)
      case A.BinOpExp(exp1, binOp, exp2, pos)  => printBinOp(prec, exp1, binOp, exp2)
      case A.UnOpExp(unOp, exp, pos)  => printUnOp(prec, unOp, exp)
      case A.RangeExp(start, end, pos) => ps("[");
        print(prec+1, start); ps(" .. "); print(prec+1, end); ps("]");
    }

    /* in */
    print(0, exp)
  }

  def printVar(v: A.Var) : Unit = v match {
    case A.SimpleVar(id, pos) => ps(id)
  }

  /* in */
  printStm(prog, 0)
  nl()
  /* end */
}

/*
end (* structure PrettyPrint *)
*/
}
