(*
**  instr.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 01/31/19.
**  Copyright © 2019-2021 innodocs. All rights reserved.
*)

structure Instr = struct

  (* code file markers *)
  val MAGIC = 0x12345678
  val MAJOR_VERSION = 0x01
  val MINOR_VERSION = 0x03

  (* pool sizes *)
  val GLOBALS = 0xFF01
  val STRINGS = 0xFF02

  (* instruction set *)
  val NOP     = 0x00
  val HALT    = 0x01

  (* load/save globals *)
  val ILOADG  = 0x10
  val ISTOREG = 0x11

  (* push constants *)
  val IPUSHC  = 0x16
  val SPUSHC  = 0x17

  (* stack ops *)
  val IPOP    = 0x20
  val ISWAP   = 0x21
  val IDUP    = 0x22

  (* branch ops *)
  val GOTO      = 0x30
  val IF_ICMPEQ = 0x31
  val IF_ICMPNE = 0x32
  val IF_ICMPLT = 0x33
  val IF_ICMPLE = 0x34
  val IF_ICMPGT = 0x35
  val IF_ICMPGE = 0x36
  val IF_IEQ    = 0x37
  val IF_INE    = 0x38

  (* arithmetic ops *)
  val IADD    = 0x40
  val ISUB    = 0x41
  val IMULT   = 0x42
  val IDIV    = 0x43
  val IMOD    = 0x44
  val IPOW    = 0x45

  (* logical ops *)
  val IAND    = 0x4A
  val IOR     = 0x4B

  (* comparison ops *)
  val ICMP    = 0x50
  val ICMPEQ  = 0x51
  val ICMPNE  = 0x52
  val ICMPLT  = 0x53
  val ICMPLE  = 0x54
  val ICMPGT  = 0x55
  val ICMPGE  = 0x56

  (* unary ops *)
  val INEG    = 0x60
  val INOT    = 0x61

  (* built-in functions *)
  val IPRINT  = 0x100
  val SPRINT  = 0x101
      
  (**
   * instrName - get name of instruction from instruction opcode
   *)
  val instrName = let
    open HashTable
    
    val tSize = 40
    val t : (int, string) HashTable.hash_table =
      mkTable (fn i => Word.fromInt (i div tSize), op=) (tSize, Fail "not found")
      
    val _ = insert t (HALT      , "HALT")
    val _ = insert t (NOP       , "NOP")
    val _ = insert t (GLOBALS   , "GLOBALS")
    val _ = insert t (STRINGS   , "STRINGS")
    val _ = insert t (ILOADG    , "ILOADG")
    val _ = insert t (ISTOREG   , "ISTOREG")
    val _ = insert t (IPUSHC    , "IPUSHC")
    val _ = insert t (SPUSHC    , "SPUSHC")
    val _ = insert t (IPOP      , "IPOP")
    val _ = insert t (ISWAP     , "ISWAP")
    val _ = insert t (IDUP      , "IDUP")
    val _ = insert t (GOTO      , "GOTO")
    val _ = insert t (IF_ICMPEQ , "IF_ICMPEQ")
    val _ = insert t (IF_ICMPNE , "IF_ICMPNE")
    val _ = insert t (IF_ICMPLT , "IF_ICMPLT")
    val _ = insert t (IF_ICMPLE , "IF_ICMPLE")
    val _ = insert t (IF_ICMPGT , "IF_ICMPGT")
    val _ = insert t (IF_ICMPGE , "IF_ICMPGE")
    val _ = insert t (IADD      , "IADD")
    val _ = insert t (ISUB      , "ISUB")
    val _ = insert t (IMULT     , "IMULT")
    val _ = insert t (IDIV      , "IDIV")
    val _ = insert t (IMOD      , "IMOD")
    val _ = insert t (IPOW      , "IPOW")
    val _ = insert t (IAND      , "IAND")
    val _ = insert t (IOR       , "IOR")
    val _ = insert t (ICMP      , "ICMP")
    val _ = insert t (ICMPEQ    , "ICMPEQ")
    val _ = insert t (ICMPNE    , "ICMPNE")
    val _ = insert t (ICMPLT    , "ICMPLT")
    val _ = insert t (ICMPLE    , "ICMPLE")
    val _ = insert t (ICMPGT    , "ICMPGT")
    val _ = insert t (ICMPGE    , "ICMPGE")
    val _ = insert t (INEG      , "INEG")
    val _ = insert t (INOT      , "INOT")
    val _ = insert t (IPRINT    , "IPRINT")
    val _ = insert t (SPRINT    , "SPRINT")
  in
    HashTable.lookup t
  end
  
  
  fun not opcode = 
    if opcode = IF_ICMPEQ then IF_ICMPNE else
    if opcode = IF_ICMPNE then IF_ICMPEQ else
    if opcode = IF_ICMPLT then IF_ICMPGE else
    if opcode = IF_ICMPLE then IF_ICMPGT else
    if opcode = IF_ICMPGT then IF_ICMPLE else
    if opcode = IF_ICMPGE then IF_ICMPLT else
    if opcode = ICMPEQ    then ICMPNE    else
    if opcode = ICMPNE    then ICMPEQ    else
    if opcode = ICMPLT    then ICMPGE    else
    if opcode = ICMPLE    then ICMPGT    else
    if opcode = ICMPGT    then ICMPLE    else
    if opcode = ICMPGE    then ICMPLT    else NOP
    
  
end (* structure Instr *)