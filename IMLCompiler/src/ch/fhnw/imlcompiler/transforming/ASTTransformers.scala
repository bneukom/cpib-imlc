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
    return Program(program.params, program.cpsDecl, transformCommands(program.commands, context.globalStoreScope.scope))
  }

  //  def transform(prog:Program): (List[Cmd], List[Decl]) = {
  //  }

  def transformCommands(cmds: List[Cmd], scope: ListBuffer[Store]): List[Cmd] = {
    // TODO insert new commands needed

    val newCmds = ListBuffer[Cmd]();

    cmds.foreach(cmd => {
      cmd match {
        case WhileCmd(e, c) => {
          val l = transformExpr(e, newCmds, scope);
          newCmds += WhileCmd(l, c);
        }

        case BecomesCmd(el, er) => {
          val l = transformExpr(el, newCmds, scope);
          val r = transformExpr(er, newCmds, scope);
          newCmds += BecomesCmd(l, r)
        }
      }

      // TODO update scope
      // TODO create new commands for this list expr
      // TODO change listExpr to StoreExpr

    })

    newCmds.toList
  }

  // TODO from 2 to 100 we need to start with 100 and go back for correct list order [2,3,4,5,...100]
  /*
  	var $x1:int;
	var $$l1:[int];
	$x1 init := toExpr;
	$$l1 init := []
	
	while $x1 > fromExpr do
		if WHERE(x)
			l1 := $x1 :: l1
		else 
			skip
		endif;
		$x1 := $x1 - 1 //
	endwhile
	
	primes init := $$l1;
   */
  def transformListExpr(lexpr: ListExpr, count: Int): (StoreDecl, StoreDecl, List[Cmd], StoreExpr) = {
    val countIdent = Ident("$" + lexpr.i.value + count)
    val listIdent = Ident("$$l" + count)
    val counterStoreDecl = StoreDecl(Var, TypedIdent(countIdent, IntType))
    val listStoreDecl = StoreDecl(Var, TypedIdent(listIdent, ListType(IntType)))
    val countInitCmd = BecomesCmd(StoreExpr(countIdent, true), lexpr.to)
    val listInitCmd = BecomesCmd(StoreExpr(listIdent, true), LiteralExpr(ListLiteral(List())))

    val appenderCmd = BecomesCmd(StoreExpr(listIdent, false), DyadicExpr(StoreExpr(countIdent, false), ConcatOpr, StoreExpr(listIdent, false)))
    val whereCmd = IfCmd(lexpr.where, appenderCmd :: Nil, SkipCmd() :: Nil)
    val incrementer = BecomesCmd(StoreExpr(countIdent, false), DyadicExpr(StoreExpr(listIdent, false), MinusOpr, LiteralExpr(IntLiteral(1))))

    val whileCmd = WhileCmd(DyadicExpr(StoreExpr(countIdent, false), GT, lexpr.to), whereCmd :: incrementer :: Nil)
    val resultExpr = StoreExpr(listIdent, false)

    return (counterStoreDecl, listStoreDecl, countInitCmd :: listInitCmd :: whileCmd :: Nil, resultExpr)
  }

  def transformExpr(expr: Expr, cmds: ListBuffer[Cmd], scope: ListBuffer[Store]): Expr = {
    expr match {
      case l: ListExpr => {
        val listExpr = transformListExpr(l, 0);

        // TODO where to add store decls?
        cmds ++= listExpr._3;

        return listExpr._4;
      }
      case DyadicExpr(lhs, op, rhs) => {
        DyadicExpr(transformExpr(lhs, cmds, scope), op, transformExpr(rhs, cmds, scope));
      }
      case MonadicExpr(rhs, op) => {
        MonadicExpr(transformExpr(rhs, cmds, scope), op);
      }
      // TODO implement rest
    }
  }

}