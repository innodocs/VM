(*
**  pp.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 03/15/19.
**  Copyright © 2019-2021 innodocs. All rights reserved.
*)

signature PRETTYPRINT =
sig
  val print : Absyn.stm -> unit

  val printString: string -> unit
  val printInt: int -> unit
  val printNL: unit -> unit

  val parseString: string * (string -> unit) -> string
  val formatString: string -> string
end

structure PrettyPrint : PRETTYPRINT =
struct

structure A = Absyn
structure S = Symbol

(**
 * printing
 *)
fun printString (s: string) = print s
fun printInt i = printString (Int.toString(i))
fun printNL () = printString "\n"

(**
 * parsing/formatting strings
 *)
fun parseString (s, errFn) =
let
  fun escChs (#"\\" :: c :: cs) = let
           val ec = case c of
             #"\"" => #"\""
           | #"b"  => #"\b"
           | #"f"  => #"\f"
           | #"n"  => #"\n"
           | #"r"  => #"\r"
           | #"t"  => #"\t"
           | _     => c
         in
           ec :: escChs cs
         end
     | escChs (#"\\" :: nil) = (*!! this should never happen *)
          (errFn ("'\\' character at end of string " ^ s); nil)
     | escChs (c :: cs) = c :: escChs cs
     | escChs nil = nil
in
  String.implode (escChs (String.explode s))
end

fun formatString s =
let
  fun escChs nil = nil
    | escChs (c :: cs) = case c of
        #"\\" => #"\\" :: #"\\" :: escChs cs
      | #"\"" => #"\\" :: #"\"" :: escChs cs
      | #"\b" => #"\\" :: #"b"  :: escChs cs
      | #"\f" => #"\\" :: #"f"  :: escChs cs
      | #"\n" => #"\\" :: #"n"  :: escChs cs
      | #"\r" => #"\\" :: #"r"  :: escChs cs
      | #"\t" => #"\\" :: #"t"  :: escChs cs
      | _     => c :: escChs cs
in
  String.implode (escChs (String.explode s))
end


(**
 * print a statement
 *)
fun print prog =
let
  val ps = printString
  val pi = printInt
  val nl = printNL

  fun indent 0 = ()
    | indent level = (ps " "; indent (level-1))

  fun printStm (A.SeqStm stms) l = List.app (fn s => printStm s l) stms
    | printStm (A.EmptyStm _) l = ps ";"
    | printStm (A.ExpStm exp) l = printExp(exp)
    | printStm (A.IfStm (tst, t, eo, _)) l =
            (nl(); indent l;
             ps "if "; printExp tst; ps " then";
             printStm t (l+1);
             case eo of
               SOME(e) => (indent l; nl(); ps "else"; printStm e (l+1))
             | _ => ();
             nl(); indent l; ps "fi;")
    | printStm (A.WhileStm (tst, b, _)) l =
            (nl(); indent l;
             ps "while "; printExp tst; ps " do";
             printStm b (l+1);
             nl(); indent l; ps "od;")
    | printStm (A.ForStm (v, r, b, _)) l =
            (nl(); indent l;
             ps "for "; printVar v; ps " in "; printExp(r); ps " do";
             printStm b (l+1);
             nl(); indent l; ps "od;")
    | printStm (A.RepeatStm(tst, b, _)) l =
            (nl(); indent l; ps "repeat";
             printStm b (l+1);
             nl(); ps "until "; printExp(tst); ps ";")
    | printStm (A.AssignStm (v, exp, _)) l =
            (nl(); indent l;
             printVar v; ps " := "; printExp exp; ps ";")
    | printStm (A.PrintStm (exps, _)) l =
            (nl(); indent l;
             ps "Print(";
             case exps of
                 e :: tl => (printExp e;
                   List.app (fn e => (ps ", "; printExp e)) tl)
               | [] => ();
             ps ");")

  and printExp exp = let
    fun openParen  prec opPrec = if prec > opPrec then ps "(" else ()
    fun closeParen prec opPrec = if prec > opPrec then ps ")" else ()

    fun print prec (A.VarExp (v, _)) = printVar v
      | print prec (A.BoolExp (b, _, _)) = ps (if b then "true" else "false")
      | print prec (A.IntExp (num, _, _)) = pi num
      | print prec (A.StringExp (s, _, _)) = ps ("\"" ^ (formatString s) ^ "\"")
      | print prec (A.BinOpExp (exp1, binop, exp2, _, _)) = let
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
          print opPrec exp1; ps opStr; print opPrec exp2;
          closeParen prec opPrec
        end
      | print prec (A.UnOpExp (unop, exp, _, _)) = let
          val (opStr, opPrec) = case unop of
            A.NotOp => ("not", 10)
          | A.NegOp => ("-", 1)
        in
          openParen prec opPrec;
          ps opStr; print opPrec exp;
          closeParen prec opPrec
        end
      | print prec (A.RangeExp (s, e, _, _)) =
          (ps "[";
          print (prec+1)  s; ps " .. "; print (prec+1) e;
          ps "]")
    in
      print 0 exp
    end

  and printVar (A.SimpleVar (id, pos)) = ps (S.name id)
in
  printStm prog 0;
  nl()
end

end (* structure PrettyPrint *)
