(*
**  symbol.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 04/03/19.
*)

signature SYMBOL =
sig
  eqtype symbol
  val symbol: string -> symbol
  val name  : symbol -> string

  type 'a table
  val empty : 'a table
  val enter : 'a table * symbol * 'a -> 'a table
  val lookup: 'a table * symbol -> 'a option
  val size  : 'a table -> int
  val list  : 'a table -> 'a list
end

structure Symbol :> SYMBOL =
struct
  type symbol = string * int
  exception Symbol

  val nextsym = ref 0
  val symbolTable : (string, symbol) HashTable.hash_table =
        HashTable.mkTable(HashString.hashString, op =) (128, Symbol)

  (*
  fun eq((s1,n1): symbol, (s2,n2): symbol): bool = (
     print "custom =\n"; n1 = n2)
  val op= = eq *)

  fun name (s, n) = s
  fun symbol name = case HashTable.find symbolTable name of
        SOME sym => sym
      | NONE => let val i = !nextsym in
          nextsym := !nextsym + 1;
          HashTable.insert symbolTable (name, (name, i));
          (name, i)
        end

  type 'a table = 'a IntRedBlackMap.map
  val empty = IntRedBlackMap.empty
  fun enter (t: 'a table, (s, n): symbol, a: 'a) = IntRedBlackMap.insert(t, n, a)
  fun lookup(t: 'a table, (s, n): symbol) = IntRedBlackMap.find(t, n)
  fun size  (t: 'a table) = IntRedBlackMap.numItems t
  fun list  (t: 'a table) = IntRedBlackMap.listItems t
end
