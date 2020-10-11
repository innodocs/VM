/*
**  instr.scala
**  vm
**
**  Created by Ovidiu Podisor on 02/15/20.
**  Copyright © 2020 innodocs. All rights reserved.
*/

/* structure Instr = struct */
object Instr {

	// code file markers
	val MAGIC         = 0x12345678
	val MAJOR_VERSION = 0x00000001
	val MINOR_VERSION = 0x00000001
	
	// instruction set	
  val NOP     = 0x00
  val HALT    = 0x01

  // load/save globals
  val GLBLS   = 0x10
  val ILOADG  = 0x11
  val ISTOREG = 0x12

  // stack ops
  val IPUSHC  = 0x20
  val IPOP    = 0x21
  val ISWAP   = 0x22
  val IDUP    = 0x23

  // arithmetic ops
  val IADD    = 0x40
  val ISUB    = 0x41
  val IMULT   = 0x42
  val IDIV    = 0x43
  val IMOD    = 0x44

  // unary ops
  val INEG    = 0x60

  // built-in functions
  val IPRINT  = 0x100
  
  /**
   * instrName - get name of instruction from instruction opcode
   */
  def instrName(opcode: Int): String = {
    opcode match {
      case HALT    => "HALT"
      case NOP     => "NOP"
      case GLBLS   => "GLOBALS"
      case ILOADG  => "ILOADG"
      case ISTOREG => "ISTOREG"
      case IPUSHC  => "IPUSHC"
      case IPOP    => "IPOP"
      case ISWAP   => "ISWAP"
      case IDUP    => "IDUP"
      case IADD    => "IADD"
      case ISUB    => "ISUB"
      case IMULT   => "IMULT"
      case IDIV    => "IDIV"
      case IMOD    => "IMOD"
      case INEG    => "INEG"
      case IPRINT  => "IPRINT"
      case _ => "??"
    }
  }

/* end (* structure Instr *) */
}
  
