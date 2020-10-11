/*
**  VMAssembler
**  vm-asm
**
**  Created by Ovidiu Podisor on 02/18/20.
**  Copyright © 2020 innodocs. All rights reserved.
*/

import java.io._;
import org.antlr.v4.runtime._;
import org.antlr.v4.runtime.tree._;

object VMAssembler {
	
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
	 * opcode streams
	 */
  private object OpCodeStream {
    
    private def writeInt(i: Int): Unit = {
      try {
        out.writeInt(i)
			} catch {
			  case e: IOException => error("could not write to output file '" + outFileName + "'")
			}
    }
  
    def emit(opCode: Int): Unit = writeInt(opCode)
    def emit(opCode: Int, arg: Int): Unit = { writeInt(opCode); writeInt(arg) }
    
    def close(): Unit = { 
      try {
        out.flush()
        out.close()
      } catch {
        case e: IOException => error("could not close output file '" + outFileName + "'")
      }
    }
  }
	
  
	/**
	 * parser listener
	 */
	private class asmListener extends VMAssemblerBaseListener {
	  
	  val S = OpCodeStream
		
	  override def exitProg(ctx: VMAssemblerParser.ProgContext): Unit = {
		  S.close()
		}
	  
		override def enterProg(ctx: VMAssemblerParser.ProgContext): Unit = {
  		S.emit(MAGIC);
  		S.emit(MAJOR_VERSION);
  		S.emit(MINOR_VERSION);
		}

		override def enterDecls(ctx: VMAssemblerParser.DeclsContext): Unit = {
  		if (ctx.GLBLS() != null) {
  		  S.emit(GLBLS);
  			S.emit(Integer.valueOf(ctx.INT().getText()));
  		}
		}

		override def enterInstr(ctx: VMAssemblerParser.InstrContext): Unit = {

  		ctx.getStart().getType() match {
  		case VMAssemblerParser.HALT    => S.emit(HALT)
  		case VMAssemblerParser.ILOADG  => S.emit(ILOADG,  Integer.valueOf(ctx.INT().getText()))
  		case VMAssemblerParser.ISTOREG => S.emit(ISTOREG, Integer.valueOf(ctx.INT().getText()))
  		case VMAssemblerParser.IPUSHC  => S.emit(IPUSHC,  Integer.valueOf(ctx.INT().getText()))
  		case VMAssemblerParser.IPOP    => S.emit(IPOP)
  		case VMAssemblerParser.ISWAP   => S.emit(ISWAP)
  		case VMAssemblerParser.IDUP    => S.emit(IDUP)
  		case VMAssemblerParser.IADD    => S.emit(IADD)
  		case VMAssemblerParser.ISUB    => S.emit(ISUB)
  		case VMAssemblerParser.IMULT   => S.emit(IMULT)
  		case VMAssemblerParser.IDIV    => S.emit(IDIV)
  		case VMAssemblerParser.IMOD    => S.emit(IMOD)
  		case VMAssemblerParser.INEG    => S.emit(INEG)
  		case VMAssemblerParser.IPRINT  => S.emit(IPRINT,  Integer.valueOf(ctx.INT().getText()))				
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
	      case 'h' => warning(showUsage=true)
	      case '?' => warning(showUsage=true)
	      case 'v' => warning(version())
	      case _   => error("bad option -" + args(0).charAt(1) + "\n", true)
	    }
	  }
		if (args.length < 2)
			error("too few arguments", true)
		
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
		if (msg.length > 0)  os.println(msg)
		if (showUsage)       usage(os)
		System.exit(errCode)
	}
	def error(msg: String = "", showUsage: Boolean = false) = errorF(System.err, 1)(msg, showUsage)
	def warning(msg: String = "", showUsage: Boolean = false) = errorF(System.out, 0)(msg, showUsage)
	
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