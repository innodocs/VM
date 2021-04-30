/*
**  emitter.scala
**  vm
**
**  Created by Ovidiu Podisor on 04/05/20.
**  Copyright © 2019-2021 innodocs. All rights reserved.
*/

  
/* signature EMITTER = sig */
sealed trait EMITTER {
  
  case class Label(name: String, offset: Int)
  
  type IOStream
  type MemStream
  
  def withIOStream(s: IOStream) : (EMITTER => Unit) => Int
  def withStream(s: MemStream)  : (EMITTER => Unit) => Int
  
  def newMemStream() : MemStream
  def mergeStream(ms: MemStream) : Unit
  
  def emitMeta(magic: Int, major: Int, minor: Int) : Unit
  def emitInstr(opcode: Int) : Unit
  def emitInstr(opcode: Int, arg: Int) : Unit
  def emitInstr(opcode: Int, arg1: Int, arg2: String) : Unit
  def emitLabel(label: Label) : Unit
  def emitJump(opcode: Int, label: Label) : Unit
  def emitStrings() : Unit
}
/* end */


abstract class EmitterFun[IOS_T, MemS_T] extends EMITTER {
  
  type IOStream = IOS_T
  type MemStream = MemS_T

  sealed abstract class Stream
  case class IOS (s: IOStream)  extends Stream	 
  case class MemS(s: MemStream) extends Stream
  
  var stream: Stream = null
  var pos: Int = 0
   
  private def withStream(
      s: Stream, compileFn: EMITTER => Unit): Int = {
    
    val oldPos = pos
    val oldStream = stream
    
    pos = 0
    stream = s
    
    var streamPos = 0
    try {
      compileFn(this)
    }
    finally {
      streamPos = pos
      pos = oldPos
      stream = oldStream
    }
    
    streamPos
  }

  def withIOStream(stream: IOStream): (EMITTER => Unit) => Int =
       compileFn => withStream(IOS(stream), compileFn) 

  def withStream(stream: MemStream): (EMITTER => Unit) => Int =
       compileFn => withStream(MemS(stream), compileFn)  
}

/*
structure AsmEmitter : EMITTER = struct
*/
object AsmEmitter extends EmitterFun[TextIO.OutStream, StringBuilder] {

  private def emitString(str: String) = {
    pos += str.length()
    stream match {
    case IOS(s)  => TextIO.output(s, str)
    case MemS(s) => s ++= str
    }
  }
  
  override def newMemStream(): MemStream = new StringBuilder(100)
  override def mergeStream(s: MemStream) = { val str = s.toString();
        pos += str.length; emitString(str) }

  override def emitMeta(magic: Int, major: Int, minor: Int) =
    emitString(s"//\n// vm-asm $major.$minor\n//\n")
  override def emitInstr(i: Int) =
    emitString(Instr.instrName(i) + "\n")
  override def emitInstr(i: Int, arg: Int) =
    emitString(Instr.instrName(i) + " " + arg.toString() + "\n")
  override def emitInstr(i: Int, arg1: Int, arg2: String) =
    emitString(Instr.instrName(i) + " \"" + PrettyPrint.formatString(arg2) + "\"\n")
  override def emitLabel(label: Label) =
    emitString(label.name + ":\n")
  override def emitJump(opcode: Int, label: Label) =
    emitString(Instr.instrName(opcode) + " " + label.name + "\n")
  override def emitStrings() = ()
}
/* end (* structure AsmEmitter *) */

/*
structure VMFEmitter : EMITTER = struct
*/
import scala.collection.mutable.ArrayBuffer

object VMFEmitter extends EmitterFun[BinIO.OutStream, ArrayBuffer[Int]] {

  private def emitInt(i: Int) = {
    pos += 1
    stream match {
    case IOS(s)  => BinIO.output(s, i)
    case MemS(s) => s += i
    }
  }
  
  override def newMemStream(): MemStream = new ArrayBuffer[Int](100)
  override def mergeStream(s: MemStream) = s.foreach(emitInt)
  
  override def emitMeta(magic: Int, major: Int, minor: Int) =
    { emitInt(magic); emitInt(major); emitInt(minor) }
  override def emitInstr(i: Int) =
    emitInt(i)
  override def emitInstr(i: Int, arg: Int) =
    { emitInt(i); emitInt(arg) }
  override def emitInstr(i: Int, arg1: Int, arg2: String) =
    { emitInt(i); emitInt(arg1) }
  override def emitLabel(label: Label) = ()
  override def emitJump(opcode: Int, label: Label) = {
      emitInt(opcode)
      label match { case Label(_, offset) => emitInt(offset) }
  }
  override def emitStrings() = StringPool.emit(emitInt)
}
/* end (* structure VMFEmitter *) */

 