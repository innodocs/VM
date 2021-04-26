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

  val parseString: string * (string -> unit) -> string
  val formatString: string -> string
end
*/
sealed trait PRETTYPRINT_SIG {
  val print: Absyn.Stm => Unit

  val printString: String => Unit
  val printInt: Int => Unit
  val printNL: () => Unit

  val parseString: (String, String => Unit) => String
  val formatString: String => String
}

//structure PrettyPrint : PRETTYPRINT
object PrettyPrint extends PRETTYPRINT_SIG {
  val print = PrettyPrintImp.print

  val printString = PrettyPrintImp.printString
  val printInt = PrettyPrintImp.printInt
  val printNL = PrettyPrintImp.printNL

  val parseString = PrettyPrintImp.parseString
  val formatString = PrettyPrintImp.formatString
}
//= struct
private object PrettyPrintImp extends PRETTYPRINT_SIG {

val A = Absyn
val S = Symbol

/**
 * basic printing
 */
val printString: String => Unit = s => Console.print(s)
val printInt: Int => Unit = i => Console.print(i)
val printNL: () => Unit = Console.println

/**
 * parsing/formatting strings
 */
val parseString: (String, String => Unit) => String = (str, errFn) => {

  val strLen = str.length
  var sb = new StringBuilder(strLen)
  var i = 0
  while (i < strLen - 1) {
    if (str(i) == '\\') {
      sb += (str(i + 1) match {
        case '\\' => '\\'
        case '"' => '"'
        case 'b' => '\b'
        case 'f' => '\f'
        case 'n' => '\n'
        case 'r' => '\r'
        case 't' => '\t'
        case _ => str(i + 1)
      })
      i += 2
    }
    else {
      sb += str(i)
      i += 1
    }
  }
  if (i == strLen - 1) {
    if (str(strLen - 1) == '\\')
      errFn("'\\' character at end of string [" + str + "]")
    else
      sb += str(strLen - 1)
  }
  sb.toString()
}

val formatString: String => String = str => {
  val strLen = str.length
  var sb = new StringBuilder(2 * strLen)
  for (i <- 0 until strLen) {
    sb ++= (str(i) match {
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case '\b' => "\\b"
      case '\f' => "\\f"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case _ => str(i).toString
    })
  }
  sb.toString()
}

/**
 * print a statement
 */
val print: Absyn.Stm => Unit = prog => {
  // let
  val ps = printString
  val pi = printInt
  val nl = printNL
  // in

  def printStm(stm: A.Stm, l: Int) : Unit = {
    // let
    def indent(level: Int): Unit = for (l <- 1 to level*2) { ps(" ") }

    // in
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
    // let
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
      case A.VarExp(v, _) => printVar(v)
      case A.BoolExp (b, _, _) => ps(if (b) "true" else "false")
      case A.IntExp(i, _, _) => pi(i)
      case A.StringExp (s, _, _) => ps("\"" + formatString(s)  + "\"")
      case A.BinOpExp(exp1, binOp, exp2, _, _)  => printBinOp(prec, exp1, binOp, exp2)
      case A.UnOpExp(unOp, exp, _, _)  => printUnOp(prec, unOp, exp)
      case A.RangeExp(start, end, _, _) => ps("[");
        print(prec+1, start); ps(" .. "); print(prec+1, end); ps("]");
    }

    // in
    print(0, exp)
  }

  def printVar(v: A.Var) : Unit = v match {
    case A.SimpleVar(id, pos) => ps(S.name(id))
  }

  // in
  printStm(prog, 0)
  nl()
  // end
}

//end (* structure PrettyPrint *)
}
