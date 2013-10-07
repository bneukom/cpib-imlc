package ch.fhnw.imlcompiler

import scala.util.parsing.input.Positional

object AST {
  case class Program(cmd: Cmd)

  sealed class Cmd
  case class WhileCmd(expr: Expr, cmd: Cmd) extends Cmd

  sealed class Expr
  sealed class DyadicExpr(l: Expr, op: Opr, r: Expr) extends Expr
  sealed class MonadicExpr(l: Expr, op: Opr) extends Expr

  case class Ident(value: String) extends Expr

  case class MultExpr(l: Expr, op: MultOpr, r: Expr) extends DyadicExpr(l, op, r)
  case class DyadicAddExpr(l: Expr, op: AddOpr, r: Expr) extends DyadicExpr(l, op, r)
  case class MonadicAddExpr(op: AddOpr, l: Expr) extends MonadicExpr(l, op)
  case class RelExpr(l: Expr, op: RelOpr, r: Expr) extends DyadicExpr(l, op, r)

  // TODO more

  sealed class Factor extends Expr;
  case class IntLiteral(v: Int) extends Factor
  case class BoolLiteral(v: Boolean) extends Factor

  sealed class Opr
  sealed class RelOpr extends Opr;
  case class EQ extends RelOpr;
  case class NE extends RelOpr;
  case class GT extends RelOpr;
  case class LT extends RelOpr;
  case class GE extends RelOpr;
  case class LE extends RelOpr;

  sealed class BoolOpr extends Opr;
  case class Cand extends BoolOpr;
  case class Cor extends BoolOpr;

  sealed class MultOpr extends Opr
  case class DivOpr extends MultOpr
  case class TimesOpr extends MultOpr
  case class ModOpr extends MultOpr

  sealed class AddOpr extends Opr
  case class PlusOpr extends AddOpr
  case class MinusOpr extends AddOpr

}