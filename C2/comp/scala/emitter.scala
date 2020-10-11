/*
**  emitter.scala
**  vm
**
**  Created by Ovidiu Podisor on 01/30/19.
**  Copyright © 2019 innodocs. All rights reserved.
*/

  
/* signature EMITTER = sig */
sealed trait EMITTER {
  def emitMeta(magic: Int, major: Int, minor: Int) : Unit
  def emitInstr(opcode: Int) : Unit
  def emitInstr(opcode: Int, arg: Int) : Unit
}
/* end */

/**
 * VM File format Emitter
 */
/* structure VMFEmitter : EMITTER = struct */
class VMFEmitter(val out: BinIO.OutStream) extends EMITTER {
  
  private def emitInt(i: Int) = BinIO.output(out, i)
  
  override def emitMeta(magic: Int, major: Int, minor: Int) = { emitInt(magic); emitInt(major); emitInt(minor) }
  
  override def emitInstr(i: Int) = emitInt(i)
  
  override def emitInstr(i: Int, arg: Int) = { emitInt(i); emitInt(arg) }
}
object VMFEmitter {
  def withStream(stream: BinIO.OutStream): VMFEmitter = new VMFEmitter(stream)
}
/* end */

/**
 * Asm Emitter
 */
/* structure AsmEmitter : EMITTER = struct */
class AsmEmitter(val out: TextIO.OutStream) extends EMITTER {
  
  private def emitString(s: String) = TextIO.output(out, s)

  override def emitMeta(magic: Int, major: Int, minor: Int) = emitString(
      "//\n" + "// vm-asm " + major.toString() + "." + minor.toString() + "\n//\n")
         
  override def emitInstr(i: Int) = emitString(
      Instr.instrName(i) + "\n")
  
  override def emitInstr(i: Int, arg: Int) = emitString(
      Instr.instrName(i) + " " + arg.toString() + "\n")
}
object AsmEmitter {
  def withStream(stream: TextIO.OutStream): AsmEmitter = new AsmEmitter(stream)
}
/* end */

 