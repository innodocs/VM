/*
 **  VMAssembler
 **  vm-asm
 **
 **  Created by Ovidiu Podisor on 02/18/20.
 **  Copyright © 2020 innodocs. All rights reserved.
 */

import java.io.*;
import java.util.function.*;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;


public class VMAssembler {

  // code file markers
  public final static int MAGIC         = 0x12345678;
  public final static int MAJOR_VERSION = 0x00000001;
  public final static int MINOR_VERSION = 0x00000001;
  
  // instruction set  
  public final static int NOP     = 0x00;
  public final static int HALT    = 0x01;
  
  // load/save globals
  public final static int GLBLS   = 0x10;
  public final static int ILOADG  = 0x11;
  public final static int ISTOREG = 0x12;
  
  // stack ops
  public final static int IPUSHC  = 0x20;
  public final static int IPOP    = 0x21;
  public final static int ISWAP   = 0x22;
  public final static int IDUP    = 0x23;
  
  // arithmetic ops
  public final static int IADD    = 0x40;
  public final static int ISUB    = 0x41;
  public final static int IMULT   = 0x42;
  public final static int IDIV    = 0x43;
  public final static int IMOD    = 0x44;
  
  // unary ops
  public final static int INEG    = 0x60;
  
  // built-in functions
  public final static int IPRINT  = 0x100;

  
  /**
   * opcode streams
   */
  private static class OpCodeStream {
    
    private void writeInt(int i) {
      try {
        out.writeInt(i);
      }
      catch (IOException e) {
        error("could not write to output file '" + outFileName + "'");
      }
    }
  
    void emit(int opCode) { writeInt(opCode); }
    void emit(int opCode, int arg) { writeInt(opCode); writeInt(arg); }
    
    void close() {
      try {
        out.flush();
        out.close();
      }
      catch (IOException e) {
        error("could not close output file '" + outFileName + "'");
      }
    }
  }  

  private static class asmListener extends VMAssemblerBaseListener {
    
    OpCodeStream S = new OpCodeStream();

    @Override
    public void enterProg(VMAssemblerParser.ProgContext ctx) {
      S.emit(MAGIC);
      S.emit(MAJOR_VERSION);
      S.emit(MINOR_VERSION);
    }

    @Override
    public void enterDecls(VMAssemblerParser.DeclsContext ctx) {
      if (ctx.GLBLS() != null)
        S.emit(GLBLS, Integer.valueOf(ctx.INT().getText()));
    }

    @Override
    public void enterInstr(VMAssemblerParser.InstrContext ctx) {
      switch (ctx.getStart().getType()) {
      case VMAssemblerParser.HALT   : S.emit(HALT);  break;
      case VMAssemblerParser.ILOADG : S.emit(ILOADG,  Integer.valueOf(ctx.INT().getText())); break;
      case VMAssemblerParser.ISTOREG: S.emit(ISTOREG, Integer.valueOf(ctx.INT().getText())); break;
      case VMAssemblerParser.IPUSHC : S.emit(IPUSHC,  Integer.valueOf(ctx.INT().getText())); break;
      case VMAssemblerParser.IPOP   : S.emit(IPOP);  break;
      case VMAssemblerParser.ISWAP  : S.emit(ISWAP); break;
      case VMAssemblerParser.IDUP   : S.emit(IDUP);  break;
      case VMAssemblerParser.IADD   : S.emit(IADD);  break;
      case VMAssemblerParser.ISUB   : S.emit(ISUB);  break;
      case VMAssemblerParser.IMULT  : S.emit(IMULT); break;
      case VMAssemblerParser.IDIV   : S.emit(IDIV);  break;
      case VMAssemblerParser.IMOD   : S.emit(IMOD);  break;
      case VMAssemblerParser.INEG   : S.emit(INEG);  break;
      case VMAssemblerParser.IPRINT : S.emit(IPRINT, Integer.valueOf(ctx.INT().getText())); break;
      }
    }
  }

  static String inFileName;
  static String outFileName;
  static CharStream in;
  static DataOutputStream out;

  static void error(String msg) {
    System.err.println("\n" + msg + "\n");
    System.exit(1);
  }

  public static void main(String args[]) throws Exception {

    // handle args
    if (args.length > 0 && args[0].charAt(0) == '-') {
      switch(args[0].charAt(1)) {
      case 'h': warning("", true);         break;
      case '?': warning("", true);         break;
      case 'v': warning(version(), false); break;
      default:  error("bad option -" + args[0].charAt(1) + "\n", true);  break;
      }
    }
    if (args.length < 2)
      error("too few arguments", true);

    inFileName = args[0];
    outFileName = args[1];

    try {
      in = CharStreams.fromFileName(inFileName);
    } catch (Exception e) {
      error("could not open input file '" + inFileName + "'");
    }
    try {
      out = new DataOutputStream(new FileOutputStream(new File(outFileName)));
    } catch (Exception e) {
      error("could not open output file '" + outFileName + "'");
    }

    // create a lexer that feeds off of input CharStream
    VMAssemblerLexer lexer = new VMAssemblerLexer(in);

    // create a buffer of tokens pulled from the lexer
    CommonTokenStream tokens = new CommonTokenStream(lexer);

    // create a parser that feeds off the tokens buffer
    VMAssemblerParser parser = new VMAssemblerParser(tokens);

    ParseTree tree = parser.prog();
    // System.out.println(tree.toStringTree(parser)); // print LISP-style tree

    // create a generic parse tree walker that can trigger callbacks
    ParseTreeWalker walker = new ParseTreeWalker();

    // walk the tree created during the parse, trigger callbacks
    walker.walk(new asmListener(), tree);
  }
  
  
  static BiFunction<PrintStream, Integer, BiConsumer<String, Boolean>> errorF =
    (PrintStream os, Integer errCode) -> {
      
      BiConsumer<String, Boolean> fn = (String msg, Boolean showUsage) -> {
        if (msg.length() > 0)
          os.println(msg);
        if (showUsage)
          usage(os);
        System.exit(errCode);
      };
    
      return fn;   
    };
  
  static void error(String msg, Boolean showUsage) {
    errorF.apply(System.err, 1).accept(msg, showUsage);
  }
  static void warning(String msg, Boolean showUsage) {
    errorF.apply(System.out, 0).accept(msg, showUsage);
  }
  
  static void usage(PrintStream os) {
    os.print(
        "Usage: vm-asm [options] asm-file vm-file\n" 
      + "\n"
      + "options:\n"
      + "\t-v               version\n"
      + "\t-h               show this help\n"
      + "\n"
    );
  }
  
  static String version() {
    return
      "VM assembler version " + MAJOR_VERSION + "." + MINOR_VERSION +
      " -- Copyright 2002-2020, InnoDocs & Innovative Systems, Inc.";
  }     
}