/*
**  parser.scala
**  vm-comp
**
**  Created by Ovidiu Podisor on 02/15/20.
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
		val parser = new SLPParser(
		               new CommonTokenStream(
		                 new SLPLexer(CharStreams.fromFileName(inFileName))));
		val tree = parser.prog();
		//System.out.println(tree.toStringTree(parser)); // print LISP-style tree

		// create a generic parse tree walker that can trigger callbacks
		val walker = new ParseTreeWalker();
		
		// walk the tree created during the parse, trigger callbacks
		val listener = new slpListener();
		walker.walk(listener, tree);
		
		listener.prog;
	}
  
  class slpListener extends SLPBaseListener {

    var prog: Absyn.Stm = null;
    
    def Stm(ast: Object): Absyn.Stm = { ast.asInstanceOf[Absyn.Stm] }
    def Exp(ast: Object): Absyn.Exp = { ast.asInstanceOf[Absyn.Exp] }
    def ExpList(ast: Object): List[Absyn.Exp] = { ast.asInstanceOf[List[Absyn.Exp]] }
    
    override def exitProg(ctx: SLPParser.ProgContext): Unit = {
      ctx.ast = Stm(ctx.stm().ast);
  	  prog = Stm(ctx.ast);
  	}
    
    override def exitSingleStm(ctx: SLPParser.SingleStmContext): Unit = {
      ctx.ast = Stm(ctx.stm().ast)
    }
    override def exitCompoundStm(ctx: SLPParser.CompoundStmContext): Unit  = {
      ctx.ast = Absyn.CompoundStm(Stm(ctx.stm(0).ast), Stm(ctx.stm(1).ast));
    }
    override def exitAssignStm(ctx: SLPParser.AssignStmContext): Unit = {
      ctx.ast = Absyn.AssignStm(ctx.ID().getText(), Exp(ctx.exp().ast))
    }
    override def exitPrintStm(ctx: SLPParser.PrintStmContext): Unit = {
      ctx.ast = Absyn.PrintStm(ExpList(ctx.exps().ast))
    }
    
    override def exitExps(ctx: SLPParser.ExpsContext): Unit = {
      var ast = List(Exp(ctx.exp().ast));
      if (ctx.COMMA() != null)
          ast = ast ::: ExpList(ctx.exps().ast);
      ctx.ast = ast;
    }

    override def exitOpExp(ctx: SLPParser.OpExpContext): Unit = {
      var op: Absyn.BinOp = null;
      if      (ctx.TIMES() != null) op = Absyn.Times;
      else if (ctx.DIV() != null)   op = Absyn.Div;
      else if (ctx.PLUS  != null)   op = Absyn.Plus;
      else                          op = Absyn.Minus;

      ctx.ast = Absyn.OpExp(Exp(ctx.exp(0).ast), op, Exp(ctx.exp(1).ast));
    }
    override def exitEseqExp(ctx: SLPParser.EseqExpContext): Unit = {
      ctx.ast = Absyn.EseqExp(Stm(ctx.stm().ast), Exp(ctx.exp().ast));
    }  
    override def exitParenExp(ctx: SLPParser.ParenExpContext): Unit = {
      ctx.ast = Exp(ctx.exp().ast)
    }
    override def exitIdExp(ctx: SLPParser.IdExpContext): Unit = {
      ctx.ast = Absyn.IdExp(ctx.ID().getText())
    }
    override def exitNumExp(ctx: SLPParser.NumExpContext): Unit = {
      ctx.ast = Absyn.NumExp(ctx.INT().getText().toInt)
    }
  }
 
/*
end (* structure Parser *)
*/
}