(*
**  test.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 01/30/19.
**  Copyright © 2019-2021 innodocs. All rights reserved.
*)

structure Test = struct

local
  structure PP = PrettyPrint
  structure E = Eval
  open AbsynUtil

  val sym = Symbol.symbol
in

val pos = ~1

val prog1 =
  SeqStm([
    AssignStm(
      SimpleVar(sym "a", pos),
      BinOpExp(
        IntExp(5, pos),
        PlusOp,
        IntExp(3, pos), pos), pos),
    AssignStm(
      SimpleVar(sym "b", pos),
      BinOpExp(
        IntExp(10, pos),
        TimesOp,
        VarExp(SimpleVar(sym "a", pos)), pos), pos),
    PrintStm([
      VarExp(SimpleVar(sym "a", pos)),
      BinOpExp(
        VarExp(SimpleVar(sym "a", pos)),
        MinusOp,
        IntExp(1, pos), pos),
      BinOpExp(
        IntExp(10, pos),
        TimesOp,
        VarExp(SimpleVar(sym "a", pos)), pos)], pos),
    PrintStm([
        VarExp(SimpleVar(sym "b", pos))], pos)])

val prog2 =
  SeqStm([
    AssignStm(
      SimpleVar(sym "a", pos),
      BinOpExp(
        BinOpExp(
          BinOpExp(
            IntExp(1, pos),
            PlusOp,
            IntExp(2, pos), pos),
          PlusOp,
          BinOpExp(
            IntExp(3, pos),
            PlusOp,
            IntExp(4, pos), pos), pos),
        PlusOp,
        BinOpExp(
          BinOpExp(
            IntExp(5, pos),
            PlusOp,
            IntExp(6, pos), pos),
          PlusOp,
          BinOpExp(
            IntExp(7, pos),
            PlusOp,
            IntExp(8, pos), pos), pos), pos), pos),
    PrintStm([
      VarExp(SimpleVar(sym "a", pos))], pos)])

val prog3 =
  SeqStm([
    AssignStm(
      SimpleVar(sym "a", pos),
      BinOpExp(
        BinOpExp(
          BinOpExp(
            IntExp(1, pos),
            PlusOp,
            IntExp(2, pos), pos),
          PlusOp,
          BinOpExp(
            IntExp(3, pos),
            PlusOp,
            IntExp(4, pos), pos), pos),
        TimesOp,
        BinOpExp(
          BinOpExp(
            IntExp(5, pos),
            PlusOp,
            IntExp(6, pos), pos),
          PlusOp,
          BinOpExp(
            IntExp(7, pos),
            PlusOp,
            IntExp(8, pos), pos), pos), pos), pos),
    PrintStm([
      VarExp(SimpleVar(sym "a", pos))], pos)])

val prog4 = 
  SeqStm([
    AssignStm(
      SimpleVar(sym "a", pos),
      BinOpExp(
        IntExp(5, pos),
        PlusOp,
        IntExp(3, pos), pos), pos),
    AssignStm(
      SimpleVar(sym "b", pos),
      BinOpExp(
        IntExp(4, pos),
        MinusOp,
        IntExp(2, pos), pos), pos),
    IfStm(
      BinOpExp(
        VarExp(SimpleVar(sym "a", pos)),
        LtOp,
        VarExp(SimpleVar(sym "b", pos)), pos),
      SeqStm([
        PrintStm([
          VarExp(SimpleVar(sym "a", pos))], pos)]),
      SOME(SeqStm([
        PrintStm([
          VarExp(SimpleVar(sym "b", pos))], pos)])), pos)])
   
fun run() = let
  val progs = [(prog1, "prog-01"), (prog2, "prog-02"), (prog3, "prog-03"), (prog4, "prog-04")]
  val files = ["test-01", "test-02", "test-03", "test-04", "test-05"]
in
  map (fn p => let val fp = "../../test/gen/" ^ (#2 p) in
         (print ("\n" ^ (#2 p) ^ ":\n");
          PP.print (#1 p);
          E.eval (#1 p);
          Comp.compile (#1 p) (fp ^ ".vm");
          Comp.asm (#1 p) (fp ^ ".asm");
          print("--------------\n"))
        end)
      progs;
      
  map (fn f => let val fp = "../../test/gap/" ^ f in
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
