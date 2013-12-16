package ch.fhnw.imlcompiler.transforming

import ch.fhnw.imlcompiler.AST.Program
import ch.fhnw.imlcompiler.AST.Expr
import ch.fhnw.imlcompiler.AST._
import scala.collection.mutable.MutableList
import scala.collection.mutable.ListBuffer
import ch.fhnw.imlcompiler.ContextChecker

// http://en.wikipedia.org/wiki/Set-builder_notation
// TODO either always use List or always ListBuffer...
// TODO type inference of list via return type of list outputfunction
trait ASTTransformers extends ContextChecker {
  
  // TODO nope!!!
  var count = 0;

  def transform(program: Program, context: Context): Program = {

    val transformedDecls = transformDecls(program.cpsDecl, context);
    val transformedCmds = transformCommands(program.commands, transformedDecls, context)

    return Program(program.params, transformedDecls ++ transformedCmds._1, transformedCmds._2)
  }

  def transformDecls(decls: List[Decl], context: Context): List[Decl] =
    decls.foldRight(List[Decl]())((decl, l) => decl match {
      case s: StoreDecl => s :: l
      case p: ProcDecl => {
        val transformed = transformCommands(p.cmds, p.decls, context);
        ProcDecl(p.ident, p.params, p.globImpList, p.decls, transformed._2) :: transformed._1 ::: l
      }
      case f: FunDecl => {
        val transformed = transformCommands(f.cmds, f.cpsDecl, context);
        FunDecl(f.ident, f.params, f.returns, f.importList, f.cpsDecl, transformed._2) :: transformed._1 ::: l
      }
    })

  // TODO write functional!
  def transformCommands(cmds: List[Cmd], decls: List[Decl], context: Context): (List[Decl], List[Cmd]) = {
    val newCmds = ListBuffer[Cmd]();
    val newDecls = ListBuffer[Decl]();

    cmds.foreach(cmd => {
      cmd match {
        case WhileCmd(e, c) => newCmds += WhileCmd(transformExpr(e, newCmds, newDecls, context), c);
        case BecomesCmd(el, er) => newCmds += BecomesCmd(el, transformExpr(er, newCmds, newDecls, context))
        case _ => // TODO impl others!!!!
      }
    })

    (newDecls.toList, newCmds.toList)
  }
  
  // TODO does not work quite right probably if with two while loops (one for > and one for <) needed
  // TODO update scope ?
  // TODO what about nested list comprehensions?
  // TODO from 2 to 100 we need to start with 100 and go back for correct list order [2,3,4,5,...100]
  /**
   * Roughly translates a list expr into the following code (AST).
   *
   * var $x1:int;
   * var $$l1:[int];
   * $x1 init := toExpr;
   * $$l1 init := []
   *
   * while $x1 > fromExpr do
   * 	if WHERE(x) do
   * 		l1 := $x1 :: l1
   * 	else
   * 		skip
   * 	endif;
   *    if toExpr > fromExpr do
   * 		$x1 := $x1 - 1
   *    else
   *    	$x1 := $x1 + 1
   *    endif
   * endwhile
   *
   * primes init := $$l1;
   */
  def transformListExpr(lexpr: ListExpr): (StoreDecl, StoreDecl, List[Cmd], StoreExpr) = {
    val countIdent = Ident("$" + lexpr.i.value + count)
    val listIdent = Ident("$$l" + count)
    count += 1 // TODO nope

    val counterStoreDecl = StoreDecl(Var, TypedIdent(countIdent, IntType))
    val listStoreDecl = StoreDecl(Var, TypedIdent(listIdent, ListType(IntType)))
    val countInitCmd = BecomesCmd(StoreExpr(countIdent, true), lexpr.to)
    val listInitCmd = BecomesCmd(StoreExpr(listIdent, true), LiteralExpr(ListLiteral(List())))

    // TODO output function must be applied!
    val appenderCmd = BecomesCmd(StoreExpr(listIdent, false), DyadicExpr(StoreExpr(countIdent, false), ConcatOpr, StoreExpr(listIdent, false)))
    
    val whereCmd = IfCmd(lexpr.predicate, appenderCmd :: Nil, SkipCmd() :: Nil)
    
    val incrementer = BecomesCmd(StoreExpr(countIdent, false), DyadicExpr(StoreExpr(countIdent, false), PlusOpr, LiteralExpr(IntLiteral(1))))
    val decrementer = BecomesCmd(StoreExpr(countIdent, false), DyadicExpr(StoreExpr(countIdent, false), MinusOpr, LiteralExpr(IntLiteral(1))))

    val incrementerIfCmd = IfCmd(DyadicExpr(lexpr.to, GT, lexpr.from), decrementer :: Nil, incrementer :: Nil)
    
    val whileCmd = WhileCmd(DyadicExpr(StoreExpr(countIdent, false), GT, lexpr.to), whereCmd :: incrementerIfCmd :: Nil)
    val resultExpr = StoreExpr(listIdent, false)

    return (counterStoreDecl, listStoreDecl, countInitCmd :: listInitCmd :: whileCmd :: Nil, resultExpr)
  }

  def transformExpr(expr: Expr, cmds: ListBuffer[Cmd], decls: ListBuffer[Decl], context: Context): Expr = {

    expr match {
      case l: ListExpr => {
        val listExpr = transformListExpr(l);
        cmds ++= listExpr._3;
        decls ++= listExpr._1 :: listExpr._2 :: Nil;
        return listExpr._4;
      }
      case DyadicExpr(lhs, op, rhs) => DyadicExpr(transformExpr(lhs, cmds, decls, context), op, transformExpr(rhs, cmds, decls, context));
      case MonadicExpr(rhs, op) => MonadicExpr(transformExpr(rhs, cmds, decls, context), op);
      case l: LiteralExpr => l
      case s: StoreExpr => s
      case f: FunCallExpr => FunCallExpr(f.i, TupleExpr(f.e.l.map(expr => transformExpr(expr, cmds, decls, context))))
    }
  }

}