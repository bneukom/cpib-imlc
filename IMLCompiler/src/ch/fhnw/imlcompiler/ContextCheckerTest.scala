package ch.fhnw.imlcompiler

import ch.fhnw.imlcompiler.AST._

object ContextCheckerTest extends ContextCheckers {
  def main(args: Array[String]) {

    val expr = DyadicExpr(LiteralExpr(IntLiteral(3)), PlusOpr, DyadicExpr(LiteralExpr(BoolLiteral(true)), TimesOpr, LiteralExpr(BoolLiteral(true))))

    TypeChecker.checkExpr(expr);
  }
}