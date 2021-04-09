(*
**  stringpool.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 03/15/19.
**  Copyright © 2019-2021 innodocs. All rights reserved.
*)

signature STRING_POOL =
sig
  type entry
  val id : entry -> int
  val str: entry -> string

  val clear : unit -> unit
  val add   : string -> entry
  val lookup: string -> entry option
  val size  : unit -> int
  val emit  : (int -> unit) -> unit
end

structure StringPool : STRING_POOL =
struct
  type entry = string * int

  exception StringPool

  val nextstring = ref ~1
  val stringTable : (string, entry) HashTable.hash_table =
        HashTable.mkTable(HashString.hashString, op =) (128, StringPool)

  fun id  (s, n) = n
  fun str (s, n) = s

  fun clear()= (HashTable.clear stringTable; nextstring := ~1)
  val size   = (fn () => HashTable.numItems stringTable)
  val lookup = HashTable.find stringTable

  fun add str = case lookup str of
      SOME e => e
    | NONE => let
        val _ = nextstring := !nextstring + 1
        val e = (str, !nextstring)
      in
        HashTable.insert stringTable (str, e);
        e
      end

  fun emit emitInt =
  let
    fun mkInt (c1, c2, c3, c4) =
    let
      open Word
      infix << orb
      fun c2w c = Word.fromInt(Char.ord(c))
      val (b1, b2, b3, b4) = (c2w c1, c2w c2, c2w c3, c2w c4)
    in
      Word.toInt((b1 << 0w24) orb ((b2 << 0w16) orb ((b3 << 0w08) orb b4)))
    end

    val acc = ref nil
    fun emitInts ints = let
      fun eis (b1 :: b2 :: b3 :: b4 :: tl) = (emitInt(mkInt(b1, b2, b3, b4)); eis tl)
        | eis incompleteInt = acc := incompleteInt
    in
      eis (!acc @ ints)
    end

    fun emitFinalInt() =
    let
      val c0 = Char.chr 0
      val i = case !acc of
          b1 :: b2 :: b3 :: nil => mkInt(b1, b2, b3, c0)
        | b1 :: b2 :: nil       => mkInt(b1, b2, c0, c0)
        | b1 :: nil             => mkInt(b1, c0, c0, c0)
        | _ => raise StringPool
    in
      emitInt(i)
    end

    val nrStrings = HashTable.numItems stringTable
  in
    if nrStrings > 0 then let
        val sorted = Array.array (nrStrings, "")
        val _ = HashTable.app (fn (s, n) => Array.update(sorted, n, s)) stringTable
        val stringPoolSize = Array.foldl (fn (str, sum) => sum + 1 + String.size str) 0 sorted
      in
        emitInt(stringPoolSize);
        Array.app (fn s => emitInts(Char.chr(String.size s) :: String.explode s)) sorted;
        emitFinalInt()
      end
    else ()
  end
end

