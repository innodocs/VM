(*
**  test.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 01/30/19.
**  Copyright © 2019 innodocs. All rights reserved.
*)

structure Test = struct

structure PP = PrettyPrint
structure E  = Eval

local
  open Absyn
in

val pos = ~1

val prog1 =
  SeqStm([
    AssignStm(
      SimpleVar("a", pos),
      BinOpExp(
        NumExp(5, pos),
        PlusOp,
        NumExp(3, pos), pos), pos),
    AssignStm(
      SimpleVar("b", pos),
      BinOpExp(
        NumExp(10, pos),
        TimesOp,
        VarExp(SimpleVar("a", pos)), pos), pos),
    PrintStm([
      VarExp(SimpleVar("a", pos)),
      BinOpExp(
        VarExp(SimpleVar("a", pos)),
        MinusOp,
        NumExp(1, pos), pos),
      BinOpExp(
        NumExp(10, pos),
        TimesOp,
        VarExp(SimpleVar("a", pos)), pos)], pos),
    PrintStm([
        VarExp(SimpleVar("b", pos))], pos)])

val prog2 =
  SeqStm([
    AssignStm(
      SimpleVar("a", pos),
      BinOpExp(
        BinOpExp(
          BinOpExp(
            NumExp(1, pos),
            PlusOp,
            NumExp(2, pos), pos),
          PlusOp,
          BinOpExp(
            NumExp(3, pos),
            PlusOp,
            NumExp(4, pos), pos), pos),
        PlusOp,
        BinOpExp(
          BinOpExp(
            NumExp(5, pos),
            PlusOp,
            NumExp(6, pos), pos),
          PlusOp,
          BinOpExp(
            NumExp(7, pos),
            PlusOp,
            NumExp(8, pos), pos), pos), pos), pos),
    PrintStm([
      VarExp(SimpleVar("a", pos))], pos)])

val prog3 =
  SeqStm([
    AssignStm(
      SimpleVar("a", pos),
      BinOpExp(
        BinOpExp(
          BinOpExp(
            NumExp(1, pos),
            PlusOp,
            NumExp(2, pos), pos),
          PlusOp,
          BinOpExp(
            NumExp(3, pos),
            PlusOp,
            NumExp(4, pos), pos), pos),
        TimesOp,
        BinOpExp(
          BinOpExp(
            NumExp(5, pos),
            PlusOp,
            NumExp(6, pos), pos),
          PlusOp,
          BinOpExp(
            NumExp(7, pos),
            PlusOp,
            NumExp(8, pos), pos), pos), pos), pos),
    PrintStm([
      VarExp(SimpleVar("a", pos))], pos)])

val prog4 = 
  SeqStm([
    AssignStm(
      SimpleVar("a", pos),
      BinOpExp(
        NumExp(5, pos),
        PlusOp,
        NumExp(3, pos), pos), pos),
    AssignStm(
      SimpleVar("b", pos),
      BinOpExp(
        NumExp(4, pos),
        MinusOp,
        NumExp(2, pos), pos), pos),
    IfStm(
      BinOpExp(
        VarExp(SimpleVar("a", pos)),
        LtOp,
        VarExp(SimpleVar("b", pos)), pos),
      SeqStm([
        PrintStm([
          VarExp(SimpleVar("a", pos))], pos)]),
      SOME(SeqStm([
        PrintStm([
          VarExp(SimpleVar("b", pos))], pos)])), pos)])
   
fun run() = let
  val progs = [(prog1, "prog1"), (prog2, "prog2"), (prog3, "prog3"), (prog4, "prog4")]
  val files = ["test1", "test2", "test3", "test4"];
in
  map (fn p => let val fp = "../../test/" ^ (#2 p) in
         (print ("\n" ^ (#2 p) ^ ":\n");
          PP.print (#1 p);
          E.eval (#1 p);
          Comp.compile (#1 p) (fp ^ ".vm");
          Comp.asm (#1 p) (fp ^ ".asm");
          print("--------------\n"))
        end)
      progs;
      
  map (fn f => let val fp = "../../test/" ^ f in
         (print ("\n" ^ f ^ ":\n");
          GAP.print (fp ^ ".gap");
          GAP.eval (fp ^ ".gap");
          GAP.compile (fp ^ ".gap") (fp ^ ".vm");
          GAP.asm (fp ^ ".gap") (fp ^ ".asm");
          print("--------------\n"))
        end)
      files;
  ()
end

end (* local *)
end (* Test *)
