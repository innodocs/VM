(*
**  type.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 04/03/19.
**  Copyright © 2019-2021 innodocs. All rights reserved.
*)

structure Type =
struct

  datatype ty = ANY
              | ANYVAL
              | BOOL
              | INT
              | ANYREF
              | STRING
              | ARRAY of ty
              | RECORD of (Symbol.symbol * ty) list
              | NULL
              | NOTHING
              | META of ty ref

  exception Error of string

  fun undef() = ref (META(ref ANY))
  fun realTy (META ty) = realTy(!ty)
    | realTy ty = ty

  fun eq (RECORD(l1), RECORD(l2)) =
        List.foldl
          (fn ((s1, ty1), equal) =>
            if equal then
              case List.find (fn (s2, ty2) => s1 = s2) l2
               of SOME (s2, ty2) => eq(ty1, ty2)
                | _ => false
            else false)
          true l1
    | eq (t1, t2) = op=(t1, t2)

  fun unify(META(t1), META(t2)) = let
          val ut = if !t1 = ANY then !t2
                   else if !t2 = ANY then !t1
                   else unify(!t1, !t2)
        in
          t1 := ut; t2 := ut;
          ut
        end
    | unify(META(t1), t2) = (t1 := t2; t2)
    | unify(t1, META(t2)) = (t2 := t1; t1)
    | unify(ARRAY(t1), ARRAY(t2)) = ARRAY(unify(t2, t2))
    | unify(RECORD(t1), RECORD(t2)) = RECORD(t1) (*!!!*)
    | unify(t1, t2) = if eq(t1, t2) then t1 else NOTHING

  fun isValType BOOL = true
    | isValType INT = true
    | isValType _ = false

  fun isRefType STRING = true
    | isRefType (ARRAY _) = true
    | isRefType (RECORD _) = true
    | isRefType _ = false

  fun toString ANY = "Any"
    | toString ANYVAL = "AnyVal"
    | toString BOOL = "Bool"
    | toString INT = "Int"
    | toString ANYREF = "AnyRef"
    | toString STRING = "String"
    | toString (ARRAY ty) = "Array[" ^ toString ty ^ "]"
    | toString (RECORD _) = "Record"
    | toString NULL = "Null"
    | toString NOTHING = "Nothing"
    | toString (META rTy) = "Meta[" ^ toString (!rTy) ^ "]"
end
