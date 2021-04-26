/**
 * stringpool.scala
 * vm-comp
 *
 * Created by Ovidiu Podisor on 04/05/20.
 * Copyright © 2019-2021 innodocs. All rights reserved.
 */

import scala.collection.mutable.HashMap

/*
signature STRING_POOL = sig
*/
sealed trait STRING_POOL {
  type Entry
  val id : Entry => Int
  val str: Entry => String

  val clear : () => Unit
  val size  : () => Int
  val add   : String => Entry
  val lookup: String => Option[Entry]
  val emit  : (Int => Unit) => Unit
}
/* end */

/*
structure StringPool : STRING_POOL
*/
object StringPool extends STRING_POOL {
  type Entry = (String, Int)
  val id = StringPoolImp.id
  val str= StringPoolImp.str

  val clear = StringPoolImp.clear
  val size  = StringPoolImp.size
  val add   = StringPoolImp.add
  val lookup= StringPoolImp.lookup
  val emit  = StringPoolImp.emit
}
/*
= struct
*/
private object StringPoolImp extends STRING_POOL {

  type Entry = StringPool.Entry

  var nextstring = -1
  val stringTable = new HashMap[String, StringPool.Entry]

  val id = (e: StringPool.Entry) => e match {case (s, n) => n }
  val str= (e: StringPool.Entry) => e match {case (s, n) => s }

  val clear = () => { nextstring = -1; stringTable.clear() }
  val size  = () => stringTable.size

  val lookup: (String => Option[StringPool.Entry]) = str => {
    stringTable.get(str)
  }

  val add: (String => StringPool.Entry) = str => lookup(str) match {
    case Some(e) => e
    case None => {
      nextstring += 1
      val e: StringPool.Entry = (str, nextstring)
      stringTable.addOne(str, e)
      e
    }
  }

  val emit: ((Int => Unit) => Unit) = emitInt => {

    val bytes = Array[Byte](0, 0, 0, 0);
    var pos = 0;
    def emitByte(b: Byte): Unit = {
      bytes(pos) = b
      pos += 1
      if (pos == 4) {
        emitInt(bytes(0) << 24 | bytes(1) << 16 | bytes(2) << 8 | bytes(3))
        pos = 0
      }
    }

    val nrStrings = size()
    if (nrStrings > 0) {
      val stringPoolSize = stringTable.foldLeft(0)((size, e) => e match {
        case (str, _) => size + (str.length+1)
      });

      emitInt(stringPoolSize);
      stringTable.toSeq.sortBy(_._2._2).foreach {
        case (str, _) =>
          emitByte(str.length.toByte)
          str.foreach(s => emitByte(s.toByte))
      }

      // add end padding to full int
      if (pos != 0) {
        for (i <- pos until 4)
          emitByte(0)
      }
    }
  }
}
/* end */