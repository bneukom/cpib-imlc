package ch.fhnw.imlcompiler

import scala.util.parsing.input.Positional

object AST {
  case class Program(cmd: Cmd)

  sealed class Cmd
  case class WhileCmd(expr: Expr, cmd: Cmd) extends Cmd

  case class Expr(l: Term1, op: BoolOpr, r: Term1)
  case class Term1(l: Term2, op: RelOpr, r: Term2)
  case class Term2(l: Term3, op: AddOpr, r: Term3)
  case class Term3(l: Factor, op: MultOpr, r: Factor)

  sealed class Factor;
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