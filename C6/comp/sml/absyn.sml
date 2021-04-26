(*
**  absyn.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 03/05/19.
**  Copyright © 2019-2021 innodocs. All rights reserved.
*)

structure Absyn =
struct
  structure T = Type

  type id = Symbol.ty
  type pos = int

  datatype binop = PlusOp | MinusOp | TimesOp | DivOp | ModOp | PowOp
                 | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
                 | AndOp | OrOp	

  datatype unop  = NegOp | NotOp

  datatype var = SimpleVar of id * pos

       and exp = BoolExp of bool * T.ty ref * pos
               | IntExp of int * T.ty ref * pos
               | StringExp of string * T.ty ref * pos
               | VarExp of var * T.ty ref
               | BinOpExp of exp * binop * exp * T.ty ref * pos
               | UnOpExp of unop * exp * T.ty ref * pos
               | RangeExp of exp * exp * T.ty ref * pos

       and stm = EmptyStm of pos
               | ExpStm of exp
               | SeqStm of stm list
               | IfStm of exp * stm * stm option * pos
               | WhileStm of exp * stm * pos
               | ForStm of var * exp * stm * pos
               | RepeatStm of exp * stm * pos
               | AssignStm of var * exp * pos
               | PrintStm of exp list * pos

  (**
   *  Absyn type handling
   *)
  fun getType (BoolExp(_, ty, _)) = !ty
    | getType (IntExp(_, ty, _)) = !ty
    | getType (StringExp(_, ty, _)) = !ty
    | getType (BinOpExp(_, _, _, ty, _)) = !ty
    | getType (UnOpExp(_, _, ty, _)) = !ty
    | getType (RangeExp(_, _, ty, _)) = !ty
    | getType (VarExp(_, ty)) = !ty

  fun setType (BoolExp(_, ty, _), nTy) = (ty := nTy)
    | setType (IntExp(_, ty, _), nTy) = (ty := nTy)
    | setType (StringExp(_, ty, _), nTy) = (ty := nTy)
    | setType (BinOpExp(exp1, binop, exp2, ty, _), nTy) = (ty := nTy)
    | setType (UnOpExp(unop, exp, ty, _), nTy) = (ty := nTy)
    | setType (RangeExp(startExp, endExp, ty, _), nTy) = (ty := nTy)
    | setType (VarExp(v, ty), nTy) = (ty := nTy)

end
