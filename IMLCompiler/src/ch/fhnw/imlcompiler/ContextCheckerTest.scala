package ch.fhnw.imlcompiler

import ch.fhnw.imlcompiler.AST._

object ContextCheckerTest extends ContextChecker{
  def main(args: Array[String]) {
    val expr = DyadicExpr(LiteralExpr(IntLiteral(3)), PlusOpr, LiteralExpr(IntLiteral(3)))
    
    TypeChecker.checkExpr(expr);
  }
}