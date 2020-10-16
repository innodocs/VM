/*
**  io.scala
**  vm
**
**  Created by Ovidiu Podisor on 02/28/20.
**  Copyright © 2020 innodocs. All rights reserved.
*/


import java.io._
  
object BinIO {
  type OutStream = DataOutputStream
  
  def openOut(fileName: String) : OutStream = 
        new DataOutputStream(new FileOutputStream(fileName));
  def closeOut(os: OutStream) : Unit = os.close()
  
  private type Word = Int
  private def outputWord(os: OutStream, w: Word): Unit = {
    os.write(w >> 24 & 0xFF)
    os.write(w >> 16 & 0xFF)
    os.write(w >>  8 & 0xFF)
    os.write(w       & 0xFF)
  }
  
  def output(os: OutStream, i: Int) = outputWord(os, i)
  //def outputString(os, s: String) = 
}

object TextIO {
  type OutStream = PrintStream
  
  def openOut(fileName: String) : OutStream = new PrintStream(fileName)
  def closeOut(os: OutStream) : Unit = os.close()
  
  //def outputInt(os: outstream, i: Int) = os.print(i)
  def output(os: OutStream, s: String) = os.print(s)
}
