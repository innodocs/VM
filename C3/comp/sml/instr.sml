(*
**  instr.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 01/30/19.
**  Copyright © 2019 innodocs. All rights reserved.
*)

structure Instr = struct

  (* code file markers *)
  val MAGIC         = 0x12345678
  val MAJOR_VERSION = 0x00000001
  val MINOR_VERSION = 0x00000001
  
  (* instruction set *)
  val NOP     = 0x00
  val HALT    = 0x01

  (* load/save globals *)
  val GLBLS   = 0x10
  val ILOADG  = 0x11
  val ISTOREG = 0x12

  (* stack ops *)
  val IPUSHC  = 0x20
  val IPOP    = 0x21
  val ISWAP   = 0x22
  val IDUP    = 0x23

  (* arithmetic ops *)
  val IADD    = 0x40
  val ISUB    = 0x41
  val IMULT   = 0x42
  val IDIV    = 0x43
  val IMOD    = 0x44

  (* unary ops *)
  val INEG    = 0x60

  (* built-in functions *)
  val IPRINT  = 0x100

      
  (**
   * instrName - get name of instruction from instruction opcode
   *)
  val instrName = let
    open HashTable
    
    val tSize = 20
    val t : (int, string) HashTable.hash_table =
      mkTable (fn i => Word.fromInt (i div tSize), op=) (tSize, Fail "not found")
      
    val _ = insert t (HALT, "HALT")
    val _ = insert t (NOP, "NOP")
    val _ = insert t (GLBLS, "GLOBALS")
    val _ = insert t (ILOADG, "ILOADG")
    val _ = insert t (ISTOREG, "ISTOREG")
    val _ = insert t (IPUSHC, "IPUSHC")
    val _ = insert t (IPOP, "IPOP")
    val _ = insert t (ISWAP, "ISWAP")
    val _ = insert t (IDUP, "IDUP")
    val _ = insert t (IADD, "IADD")
    val _ = insert t (ISUB, "ISUB")
    val _ = insert t (IMULT, "IMULT")
    val _ = insert t (IDIV, "IDIV")
    val _ = insert t (IMOD, "IMOD")
    val _ = insert t (INEG, "INEG")
    val _ = insert t (IPRINT, "IPRINT")
  in
    HashTable.lookup t
  end
  
end (* structure Instr *)
