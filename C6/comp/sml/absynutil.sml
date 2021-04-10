(*
**  absynutil.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 03/05/19.
**  Copyright © 2019-2021 innodocs. All rights reserved.
**
**  The purpose of this file is to provide default arguments to Absyn
**  constructors so we do not have keep changing dependant files/structures
**  as we move from one version to the next - would be unnecessary if SML
**  supported default values for parameters.
*)

structure AbsynUtil =
struct
  structure A = Absyn
  structure T = Type

  open Absyn;

  fun BoolExp (n, p) = A.BoolExp(n, T.undef(), p)
  fun IntExp (n, p) = A.IntExp(n, T.undef(), p)
  fun StringExp(s, p) = A.StringExp(s, T.undef(), p)
  fun BinOpExp(e1, bo, e2, p) = A.BinOpExp(e1, bo, e2, T.undef(), p)
  fun UnOpExp(e, uo, p) = A.UnOpExp(e, uo, T.undef(), p)
  fun RangeExp(e1, e2, p) = A.RangeExp(e1, e2, T.undef(), p)
  fun VarExp var = A.VarExp(var, T.undef())
end