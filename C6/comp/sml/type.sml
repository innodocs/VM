(*
**  type.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 04/03/19.
**  Copyright © 2019-2021 innodocs. All rights reserved.
*)

structure Type = struct

  datatype ty = ANY
              | ANYVAL
              | BOOL
              | INT
              | ANYREF
              | STRING
              | ARRAY of ty
              | RECORD of (Symbol.ty * ty) list
              | NULL
              | NOTHING
              | META of ty ref

  exception Error of string * int

  fun isValType ANYVAL = true
    | isValType BOOL = true
    | isValType INT = true
    | isValType _ = false

  fun isRefType ANYREF = true
    | isRefType NULL = true
    | isRefType STRING = true
    | isRefType (ARRAY _) = true
    | isRefType (RECORD _) = true
    | isRefType _ = false

  fun undef() = ref (META(ref ANY))
  fun realTy (META ty) = realTy(!ty)
    | realTy ty = ty

  fun eq (RECORD(l1), RECORD(l2)) =
        List.all
          (fn (s1, ty1) =>
             case (List.find (fn (s2, ty2) => s1 = s2) l2) of
               SOME (s2, ty2) => eq(ty1, ty2)
             | NONE => false)
          l1
    | eq (t1, t2) = op=(t1, t2)

  fun unify(META(t1), META(t2)) = let
          val ut = if !t1 = ANY then !t2
                   else if !t2 = ANY then !t1
                   else unify(!t1, !t2)
        in
          t1 := ut; t2 := ut;
          ut
        end
    | unify(META(t1), t2)  = (t1 := t2; t2)
    | unify(t1, META(t2))  = (t2 := t1; t1)
    | unify(ANY, t2) = t2
    | unify(t1, ANY) = t1
    | unify(ANYVAL, t2) = if isValType(t2) then t2 else NOTHING
    | unify(t1, ANYVAL) = if isValType(t1) then t1 else NOTHING
    | unify(ANYREF, t2) = if isRefType(t2) then t2 else NOTHING
    | unify(t1, ANYREF) = if isRefType(t1) then t1 else NOTHING
    | unify(ARRAY(t1), ARRAY(t2)) = ARRAY(unify(t2, t2))
    | unify(RECORD(t1), RECORD(t2)) = RECORD(t1) (*!!!*)
    | unify(t1, t2) = if eq(t1, t2) then t1 else NOTHING


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


  (**
   *  debug
   * )
  fun prUTy(loc, e1Ty, e2Ty, uTy) =
    print (loc ^ "::unify("
     ^ toString(e1Ty) ^ ", " ^ toString(e2Ty)
     ^ ") = " ^ toString(uTy) ^ "\n")

   ( **)
end (* structure Type *)
