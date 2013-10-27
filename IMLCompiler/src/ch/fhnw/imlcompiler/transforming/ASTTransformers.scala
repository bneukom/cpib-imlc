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

  def transformExpr(expr: Expr, context: Context) = {
    expr match {
      case ListExpr(ret, i, from, to, where) => {
        // TODO returns a new store expr
        // TODO how to inject the new commands/stores needed?
      }
      case DyadicExpr(lhs, _,rhs) =>
      case MonadicExpr(rhs, _) =>
      case _ => {}
    }
  }

}