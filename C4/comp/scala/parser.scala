/*
**  parser.scala
**  vm-comp
**
**  Created by Ovidiu Podisor on 02/28/20.
**  Copyright © 2020 innodocs. All rights reserved.
*/

/*
structure Parser : sig
    val parse : string -> Absyn.stm
  end
*/

sealed trait PARSER_SIG {
  val parse : String => Absyn.Stm
}

object Parser extends PARSER_SIG {
  override val parse = ParserImp.parse
}

/*
 * = struct
 */
private object ParserImp extends PARSER_SIG {

  import org.antlr.v4.runtime._;
  import org.antlr.v4.runtime.tree._;

  val parse : String => Absyn.Stm = inFileName => {

		// create a parser that feeds off the tokens buffer
		val parser = new GAPParser(
		               new CommonTokenStream(
		                 new GAPLexer(CharStreams.fromFileName(inFileName))));
		val tree = parser.prog();
		//System.out.println(tree.toStringTree(parser)); // print LISP-style tree

		// create a generic parse tree walker that can trigger callbacks
		val walker = new ParseTreeWalker();
		
		// walk the tree created during the parse, trigger callbacks
		val listener = new gapListener();
		walker.walk(listener, tree);
		
		listener.prog;
	}
  
  class gapListener extends GAPBaseListener {
    
    val A = Absyn

    var prog: A.Stm = null;
    
    def Stm(ast: Object): A.Stm = { ast.asInstanceOf[A.Stm] }
    def StmList(ast: Object): List[A.Stm] = { ast.asInstanceOf[List[A.Stm]] }    
    def Exp(ast: Object): A.Exp = { ast.asInstanceOf[A.Exp] }
    def ExpList(ast: Object): List[A.Exp] = { ast.asInstanceOf[List[A.Exp]] }
    
    override def exitProg(ctx: GAPParser.ProgContext): Unit = {
      prog = Stm(ctx.stms().ast)
      //prog = if (stms.length == 1) stms.head else A.SeqStm(stms)
  	  ctx.ast = prog
  	}
    
    def flattenSeqStm(stm: A.Stm, ostms: Option[A.Stm]): A.Stm = {
      ostms match {
        case None => stm
        case Some(stms) => stms match {
          case A.SeqStm(lst) => A.SeqStm(stm :: lst)
          case _             => A.SeqStm(List(stm, stms))
          }
      }
    }
    override def exitEmptyStm(ctx: GAPParser.EmptyStmContext): Unit = {
      ctx.ast =
        flattenSeqStm(
          A.EmptyStm(),
          if (ctx.stms() == null) None else Some(Stm(ctx.stms().ast)))
    }
    override def exitSeqStm(ctx: GAPParser.SeqStmContext): Unit  = {
      ctx.ast =
        flattenSeqStm(
          Stm(ctx.stm().ast),
          if (ctx.stms() == null) None else Some(Stm(ctx.stms().ast)))      
    }
 
    override def exitIfStm(ctx: GAPParser.IfStmContext): Unit = {
      ctx.ast = A.IfStm(
                  Exp(ctx.exp().ast),
                  Stm(ctx.stms(0).ast),
                  if (ctx.ELSE() != null) Some(Stm(ctx.stms(1).ast)) else None)
    }
    
    override def exitWhileStm(ctx: GAPParser.WhileStmContext): Unit = {
      ctx.ast = A.WhileStm(
                  Exp(ctx.exp().ast),
                  Stm(ctx.stms().ast))
    }
    
    override def exitAssignStm(ctx: GAPParser.AssignStmContext): Unit = {
      ctx.ast = A.AssignStm(
                  A.SimpleVar(ctx.ID().getText()),
                  Exp(ctx.exp().ast))
    }
    
    override def exitPrintStm(ctx: GAPParser.PrintStmContext): Unit = {
      ctx.ast = A.PrintStm(ExpList(ctx.exps().ast))
    }
    
    override def exitExps(ctx: GAPParser.ExpsContext): Unit = {
      var ast = List(Exp(ctx.exp().ast));
      if (ctx.COMMA() != null)
          ast = ast ::: ExpList(ctx.exps().ast);
      ctx.ast = ast;
    } 
   
    override def exitBinOpExp(ctx: GAPParser.BinOpExpContext): Unit = {
      var op: A.BinOp = null;
      if      (ctx.POW() != null)  op = A.PowOp
      else if (ctx.TIMES != null)  op = A.TimesOp
      else if (ctx.DIV() != null)  op = A.DivOp
      else if (ctx.MOD   != null)  op = A.ModOp
      else if (ctx.PLUS  != null)  op = A.PlusOp
      else if (ctx.MINUS != null)  op = A.MinusOp
      else if (ctx.EQ    != null)  op = A.EqOp
      else if (ctx.NEQ   != null)  op = A.NeqOp      
      else if (ctx.LT    != null)  op = A.LtOp      
      else if (ctx.LE    != null)  op = A.LeOp
      else if (ctx.GT    != null)  op = A.GtOp      
      else if (ctx.GE    != null)  op = A.GeOp
      else if (ctx.AND   != null)  op = A.AndOp
      else   /*ctx.OR */           op = A.OrOp

      ctx.ast = A.BinOpExp(
                  Exp(ctx.exp(0).ast),
                  op,
                  Exp(ctx.exp(1).ast));
    }
    
    override def exitParenExp(ctx: GAPParser.ParenExpContext): Unit = {
      ctx.ast = Exp(ctx.exp().ast)
    }
    
    override def exitVarExp(ctx: GAPParser.VarExpContext): Unit = {
      ctx.ast = A.VarExp(
                  A.SimpleVar(ctx.ID().getText()))
    }
    override def exitNumExp(ctx: GAPParser.NumExpContext): Unit = {
      ctx.ast = A.NumExp(ctx.INT().getText().toInt)
    }
  }
 
/*
end (* structure Parser *)
*/
}