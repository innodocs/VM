/*
**  VMAssembler
**  vm-asm
**
**  Created by Ovidiu Podisor on 02/28/20.
**  Copyright © 2020 innodocs. All rights reserved.
*/

import java.io._
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

object VMAssembler {

  // code file markers
  val MAGIC = 0x12345678
  val MAJOR_VERSION = 0x00000001
  val MINOR_VERSION = 0x00000002

  // instruction set
  val NOP = 0x00
  val HALT = 0x01

  // load/save globals
  val GLBLS = 0x10
  val ILOADG = 0x11
  val ISTOREG = 0x12

  // stack ops
  val IPUSHC = 0x20
  val IPOP = 0x21
  val ISWAP = 0x22
  val IDUP = 0x23

  // branch ops
  val GOTO = 0x30
  val IF_ICMPEQ = 0x31
  val IF_ICMPNE = 0x32
  val IF_ICMPLT = 0x33
  val IF_ICMPLE = 0x34
  val IF_ICMPGT = 0x35
  val IF_ICMPGE = 0x36
  val IF_IEQ = 0x37
  val IF_INE = 0x38

  // arithmetic ops
  val IADD = 0x40
  val ISUB = 0x41
  val IMULT = 0x42
  val IDIV = 0x43
  val IMOD = 0x44
  val IPOW = 0x45

  // logical ops
  val IAND = 0x4A
  val IOR = 0x4B

  // comparison ops
  val ICMP = 0x50
  val ICMPEQ = 0x51
  val ICMPNE = 0x52
  val ICMPLT = 0x53
  val ICMPLE = 0x54
  val ICMPGT = 0x55
  val ICMPGE = 0x56

  // unary ops
  val INEG = 0x60
  val INOT = 0x61

  // built-in functions
  val IPRINT = 0x100


  /**
   * opcode streams
   */
  private object OpCodeStream {

    val stream = new ArrayBuffer[Int](100)

    def offset: Int = stream.length

    def emit(opCode: Int): Unit = stream += opCode

    def emit(opCode: Int, arg: Int): Unit = stream += opCode += arg

    def update(offset: Int, newValue: Int): Unit = stream.update(offset, newValue)

    /**
     * write stream to output file    
     */
    def close(): Unit = {
      try {
        for (opCode <- stream)
          out.writeInt(opCode)
        out.flush()
        out.close()
      } catch {
        case e: IOException => error("could not write to output file '" + outFileName + "'")
      }
    }
  }

  /**
   * jump labels
   */
  private class Label(val name: String, var offset: Int = -1) {

    def isDefined(): Boolean = offset != -1

    val jumpSites = new ArrayBuffer[Int](2)

    def addJumpSite(offset: Int): Unit = jumpSites += offset

    override def toString: String = {
      s"Label($name, $offset, " + jumpSites.toString + ")"
    }
  }

  private object Labels {

    val S = OpCodeStream

    val labels = new HashMap[String, Label]()

    def defineLabel(name: String): Unit = {
      val o = labels.get(name)
      o match {
        case None =>
          val l = new Label(name, S.offset)
          labels += (name -> l)
        case Some(l) =>
          if (l.isDefined())
            return error(s"error: label '$name' is already defined, exiting")
          l.offset = S.offset
      }
    }

    def offset(name: String): Int = {
      val l = labels.get(name) match {
        case None => val l = new Label(name); labels += (name -> l); l
        case Some(l) => l
      }

      l.addJumpSite(S.offset)
      l.offset
    }

    def adjustLabels(): Unit = {
      for ((n, l) <- labels) {
        if (l.offset == -1)
          return error("error: unresolved jump label '" + l.name + "', exiting")
        for (jOffset <- l.jumpSites) {
          S.update(jOffset + 1, l.offset - jOffset)
        }
      }
    }
  }

  private class asmListener extends VMAssemblerBaseListener {

    val S = OpCodeStream
    val L = Labels

    def intValue(ctx: TerminalNode): Int = ctx.getText().toInt

    override def exitProg(ctx: VMAssemblerParser.ProgContext): Unit = {
      L.adjustLabels()
      S.close()
    }

    override def enterProg(ctx: VMAssemblerParser.ProgContext): Unit = {
      S.emit(MAGIC)
      S.emit(MAJOR_VERSION)
      S.emit(MINOR_VERSION)
    }

    override def enterDecls(ctx: VMAssemblerParser.DeclsContext): Unit = {
      if (ctx.GLBLS() != null)
        S.emit(GLBLS, intValue(ctx.INT()))
    }

    override def enterLabel(ctx: VMAssemblerParser.LabelContext): Unit = {
      L.defineLabel(ctx.LABEL().getText())
    }

    override def enterBranchInstr(ctx: VMAssemblerParser.BranchInstrContext): Unit = {

      val label = ctx.LABEL().getText();
      val offset = L.offset(label)

      ctx.getStart().getType() match {
        // branch ops
        case VMAssemblerParser.GOTO => S.emit(GOTO, offset)
        case VMAssemblerParser.IF_ICMPEQ => S.emit(IF_ICMPEQ, offset)
        case VMAssemblerParser.IF_ICMPNE => S.emit(IF_ICMPNE, offset)
        case VMAssemblerParser.IF_ICMPLT => S.emit(IF_ICMPLT, offset)
        case VMAssemblerParser.IF_ICMPGE => S.emit(IF_ICMPGE, offset)
        case VMAssemblerParser.IF_ICMPGT => S.emit(IF_ICMPGT, offset)
        case VMAssemblerParser.IF_ICMPLE => S.emit(IF_ICMPLE, offset)
        case VMAssemblerParser.IF_IEQ => S.emit(IF_IEQ, offset)
        case VMAssemblerParser.IF_INE => S.emit(IF_INE, offset)
      }
    }

    override def enterInstr(ctx: VMAssemblerParser.InstrContext): Unit = {

      if (ctx.branch_instr() != null)
        return;

      ctx.getStart().getType() match {
        case VMAssemblerParser.HALT => S.emit(HALT)

        // load/store globals
        case VMAssemblerParser.ILOADG => S.emit(ILOADG, intValue(ctx.INT()))
        case VMAssemblerParser.ISTOREG => S.emit(ISTOREG, intValue(ctx.INT()))

        // stack ops
        case VMAssemblerParser.IPUSHC => S.emit(IPUSHC, intValue(ctx.INT()))
        case VMAssemblerParser.IPOP => S.emit(IPOP)
        case VMAssemblerParser.ISWAP => S.emit(ISWAP)
        case VMAssemblerParser.IDUP => S.emit(IDUP)

        // arithmetic ops
        case VMAssemblerParser.IADD => S.emit(IADD)
        case VMAssemblerParser.ISUB => S.emit(ISUB)
        case VMAssemblerParser.IMULT => S.emit(IMULT)
        case VMAssemblerParser.IDIV => S.emit(IDIV)
        case VMAssemblerParser.IMOD => S.emit(IMOD)
        case VMAssemblerParser.IPOW => S.emit(IPOW)

        // logical ops
        case VMAssemblerParser.IAND => S.emit(IAND)
        case VMAssemblerParser.IOR => S.emit(IOR)

        // comparison ops
        case VMAssemblerParser.ICMP => S.emit(ICMP)
        case VMAssemblerParser.ICMPEQ => S.emit(ICMPEQ)
        case VMAssemblerParser.ICMPNE => S.emit(ICMPNE)
        case VMAssemblerParser.ICMPLT => S.emit(ICMPLT)
        case VMAssemblerParser.ICMPGE => S.emit(ICMPGE)
        case VMAssemblerParser.ICMPGT => S.emit(ICMPGT)
        case VMAssemblerParser.ICMPLE => S.emit(ICMPLE)

        // unary ops
        case VMAssemblerParser.INEG => S.emit(INEG)
        case VMAssemblerParser.INOT => S.emit(INOT)

        // built-in functions
        case VMAssemblerParser.IPRINT => S.emit(IPRINT, intValue(ctx.INT()))
      }
    }
  }


  var inFileName: String = null;
  var outFileName: String = null;
  var in: CharStream = null;
  var out: DataOutputStream = null;

  def main(args: Array[String]): Unit = {

    // handle args
    if (args.length > 0 && args(0).charAt(0) == '-') {
      args(0).charAt(1) match {
        case 'h' => warning(showUsage = true)
        case '?' => warning(showUsage = true)
        case 'v' => warning(version())
        case _ => error("bad option -" + args(0).charAt(1) + "\n", true)
      }
    }
    if (args.length < 2) {
      error("too few arguments", true);
    }

    inFileName = args(0);
    outFileName = args(1);

    try {
      in = CharStreams.fromFileName(inFileName);
    } catch {
      case e: Exception => error("could not open input file '" + inFileName + "'")
    }
    try {
      out = new DataOutputStream(new FileOutputStream(new File(outFileName)));
    } catch {
      case e: Exception => error("could not open output file '" + outFileName + "'")
    }

    // create a lexer that feeds off of input CharStream
    val lexer = new VMAssemblerLexer(in);

    // create a buffer of tokens pulled from the lexer
    val tokens = new CommonTokenStream(lexer);

    // create a parser that feeds off the tokens buffer
    val parser = new VMAssemblerParser(tokens);
    val tree = parser.prog();
    //System.out.println(tree.toStringTree(parser)); // print LISP-style tree

    // create a generic parse tree walker that can trigger callbacks
    val walker = new ParseTreeWalker();

    // walk the tree created during the parse, trigger callbacks
    walker.walk(new asmListener(), tree);
  }


  def errorF(os: PrintStream, errCode: Int)(msg: String, showUsage: Boolean): Unit = {
    if (msg.length > 0) os.println(msg)
    if (showUsage) usage(os)
    System.exit(errCode)
  }

  def error(msg: String = "", showUsage: Boolean = false) =
    errorF(System.err, 1)(msg, showUsage)

  def warning(msg: String = "", showUsage: Boolean = false) =
    errorF(System.out, 0)(msg, showUsage)

  def usage(os: PrintStream): Unit = {
    os.print(
      "Usage: vm-asm [options] asm-file vm-file\n"
        + "\n"
        + "options:\n"
        + "\t-v               version\n"
        + "\t-h               show this help\n"
        + "\n"
    )
  }

  def version(): String = {
    s"VM assembler version $MAJOR_VERSION.$MINOR_VERSION" +
      " -- Copyright 2002-2020, InnoDocs & Innovative Systems, Inc."
  }
}