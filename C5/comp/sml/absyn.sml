(*
**  absyn.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 03/05/19.
**  Copyright © 2019 innodocs. All rights reserved.
*)

structure Absyn = struct 

  type id = string
  type pos = int

  datatype binop = PlusOp | MinusOp | TimesOp | DivOp | ModOp | PowOp
                 | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
                 | AndOp | OrOp	

  datatype unop  = NegOp | NotOp

  datatype var = SimpleVar of id * pos

       and exp = VarExp of var
               | NumExp of int * pos
               | BinOpExp of exp * binop * exp * pos
               | UnOpExp of unop * exp * pos
               | RangeExp of exp * exp * pos

       and stm = EmptyStm of pos
               | ExpStm of exp
               | SeqStm of stm list
               | IfStm of exp * stm * stm option * pos
               | WhileStm of exp * stm * pos
               | ForStm of var * exp * stm * pos
               | RepeatStm of exp * stm * pos
               | AssignStm of var * exp * pos
               | PrintStm of exp list * pos
end
