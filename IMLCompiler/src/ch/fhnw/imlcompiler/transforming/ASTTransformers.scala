package ch.fhnw.imlcompiler.transforming

import ch.fhnw.imlcompiler.AST.Program
import ch.fhnw.imlcompiler.checking.ProgramContext.Context
import ch.fhnw.imlcompiler.AST.Expr
import ch.fhnw.imlcompiler.AST._
import scala.collection.mutable.MutableList
import ch.fhnw.imlcompiler.checking.ProgramContext.Store
import scala.collection.mutable.ListBuffer

trait ASTTransformers {

  def transform(program: Program, context: Context): Program = {
    return program.copy(cpsDecl = transformDecls(), commands = transformCommands(program.commands, context.globalStoreScope.scope));
  }

  def transformDecls(): List[Decl] = {

    Nil
  }

  def transformCommands(cmds: List[Cmd], scope: ListBuffer[Store]): List[Cmd] = {
    // TODO insert new commands needed

    val newCmds = MutableList[Cmd]();

    cmds.foreach(cmd => {
      if (containsListExpr(cmd)) {
        // TODO update scope
        // TODO create new commands for this list expr
        // TODO change listExpr to StoreExpr
      }

      newCmds += cmd;
    })

    newCmds.toList
  }

  def containsListExpr(cmd: Cmd): Boolean = true

  /*
  	var $x1:int;
	var $$l1:[int];
	$x1 init := FROM;
	$$l1 init := []
	
	while $x1 [< OR >] TO do
		if WHERE(x)
			l1 := $x1 :: l1;
		else 
			skip;
		endif;
		$x1 := $x1 [+ OR -] 1 // $x1 := $x1 - 1 falls end > start
	endwhile
	
	primes init := $$l1;
   */
  def transformListExpr(lexpr: ListExpr, count: Int): (StoreDecl, StoreDecl, List[Cmd], StoreExpr) = {
    val countIdent = Ident("$" + lexpr.i.value + count)
    val listIdent = Ident("$$l" + count)
    val counterStoreDecl = StoreDecl(Var, TypedIdent(countIdent, IntType))
    val listStoreDecl = StoreDecl(Var, TypedIdent(listIdent, ListType(IntType)))
    val countInitCmd = BecomesCmd(StoreExpr(countIdent, true), LiteralExpr(lexpr.from))
    val listInitCmd = BecomesCmd(StoreExpr(listIdent, true), LiteralExpr(ListLiteral(List())))

    val appenderCmd = BecomesCmd(StoreExpr(listIdent, false), DyadicExpr(StoreExpr(countIdent, false), ConcatOpr, StoreExpr(listIdent, false)))
    val whereCmd = IfCmd(lexpr.where, appenderCmd :: Nil, SkipCmd() :: Nil)
    val incrementer = BecomesCmd(StoreExpr(countIdent, false), DyadicExpr(StoreExpr(listIdent, false), if (lexpr.from.v < lexpr.to.v) PlusOpr else MinusOpr, LiteralExpr(IntLiteral(1))))

    val whileCmd = WhileCmd(DyadicExpr(StoreExpr(countIdent, false), if (lexpr.from.v < lexpr.to.v) LT else GT, LiteralExpr(lexpr.to)), whereCmd :: incrementer :: Nil)
    val resultExpr = StoreExpr(listIdent, false)
    
    return (counterStoreDecl, listStoreDecl, countInitCmd :: listInitCmd :: whileCmd :: Nil, resultExpr)
  }

  def transformExpr(expr: Expr, context: Context) = {
    expr match {
      case ListExpr(ret, i, from, to, where) => {
        // TODO returns a new store expr
        // TODO how to inject the new commands/stores needed?
      }
      case DyadicExpr(lhs, _, rhs) =>
      case MonadicExpr(rhs, _) =>
      case _ => {}
    }
  }

}