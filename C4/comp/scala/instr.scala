/*
**  instr.scala
**  vm
**
**  Created by Ovidiu Podisor on 02/28/20.
**  Copyright © 2020 innodocs. All rights reserved.
*/

/* structure Instr = struct */
object Instr {

  // code file markers
  val MAGIC         = 0x12345678
  val MAJOR_VERSION = 0x00000001
  val MINOR_VERSION = 0x00000002

  // instruction set
  val NOP  = 0x00
  val HALT = 0x01

  // load/save globals
  val GLBLS   = 0x10
  val ILOADG  = 0x11
  val ISTOREG = 0x12

  // stack ops
  val IPUSHC = 0x20
  val IPOP   = 0x21
  val ISWAP  = 0x22
  val IDUP   = 0x23

  // branch ops
  val GOTO      = 0x30
  val IF_ICMPEQ = 0x31
  val IF_ICMPNE = 0x32
  val IF_ICMPLT = 0x33
  val IF_ICMPLE = 0x34
  val IF_ICMPGT = 0x35
  val IF_ICMPGE = 0x36
  val IF_IEQ    = 0x37
  val IF_INE    = 0x38

  // arithmetic ops
  val IADD   = 0x40
  val ISUB   = 0x41
  val IMULT  = 0x42
  val IDIV   = 0x43
  val IMOD   = 0x44
  val IPOW   = 0x45

  // logical ops
  val IAND   = 0x4A
  val IOR    = 0x4B

  // comparison ops
  val ICMP   = 0x50
  val ICMPEQ = 0x51
  val ICMPNE = 0x52
  val ICMPLT = 0x53
  val ICMPLE = 0x54
  val ICMPGT = 0x55
  val ICMPGE = 0x56

  // unary ops
  val INEG   = 0x60
  val INOT   = 0x61

  // built-in functions
  val IPRINT = 0x100

  
  /**
   * instrName - get name of instruction from instruction opcode
   */
  def instrName(opcode: Int): String = opcode match {
    case HALT      => "HALT"
    case NOP       => "NOP"
    case GLBLS     => "GLOBALS"
    case ILOADG    => "ILOADG"
    case ISTOREG   => "ISTOREG"
    case IPUSHC    => "IPUSHC"
    case IPOP      => "IPOP"
    case ISWAP     => "ISWAP"
    case IDUP      => "IDUP"
    case GOTO      => "GOTO"
    case IF_ICMPEQ => "IF_ICMPEQ"
    case IF_ICMPNE => "IF_ICMPNE"
    case IF_ICMPLT => "IF_ICMPLT"
    case IF_ICMPLE => "IF_ICMPLE"
    case IF_ICMPGT => "IF_ICMPGT"
    case IF_ICMPGE => "IF_ICMPGE"
    case IADD      => "IADD"
    case ISUB      => "ISUB"
    case IMULT     => "IMULT"
    case IDIV      => "IDIV"
    case IMOD      => "IMOD"
    case IPOW      => "IPOW"
    case IAND      => "IAND"
    case IOR       => "IOR"
    case ICMP      => "ICMP"
    case ICMPEQ    => "ICMPEQ"
    case ICMPNE    => "ICMPNE"
    case ICMPLT    => "ICMPLT"
    case ICMPLE    => "ICMPLE"
    case ICMPGT    => "ICMPGT"
    case ICMPGE    => "ICMPGE"
    case INEG      => "INEG"
    case INOT      => "INOT"
    case IPRINT    => "IPRINT"
    case _ => "??"
  }
  
  def not(opcode: Int): Int = opcode match {
    case IF_ICMPEQ => IF_ICMPNE
    case IF_ICMPNE => IF_ICMPEQ
    case IF_ICMPLT => IF_ICMPGE
    case IF_ICMPLE => IF_ICMPGT
    case IF_ICMPGT => IF_ICMPLE
    case IF_ICMPGE => IF_ICMPLT

    case ICMPEQ    => ICMPNE
    case ICMPNE    => ICMPEQ
    case ICMPLT    => ICMPGE
    case ICMPLE    => ICMPGT
    case ICMPGT    => ICMPLE
    case ICMPGE    => ICMPLT   
  }
      
/* end (* structure Instr *) */
}
  
