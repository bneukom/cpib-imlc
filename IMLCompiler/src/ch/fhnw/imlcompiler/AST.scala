package ch.fhnw.imlcompiler

import scala.util.parsing.input.Positional
import scala.collection.immutable.List

object AST {
  case class Program(cmd: Cmd)

  sealed class Decl
  case class FunDecl extends Decl
  case class ProcDecl extends Decl
  case class CpsDecl extends Decl

  case class GlobImport(f: FlowMode, c: ChangeMode, i: Ident)
  case class GlobImpList(i: List[GlobImport])

  sealed class Cmd
  case class WhileCmd(expr: Expr, cmd: Cmd) extends Cmd

  // modes
  sealed class FlowMode
  case class In extends FlowMode
  case class Out extends FlowMode
  case class InOut extends FlowMode

  sealed class ChangeMode
  case class Const extends ChangeMode
  case class Var extends ChangeMode

  // expressions
  sealed class Expr
  case class DyadicExpr(l: Expr, op: Opr, r: Expr) extends Expr
  sealed class MonadicExpr(l: Expr, op: Opr) extends Expr

  case class Ident(value: String) extends Expr

  case class MonadicAddExpr(op: AddOpr, l: Expr) extends MonadicExpr(l, op)

  sealed class Factor extends Expr;
  sealed class Literal extends Factor;
  case class IntLiteral(v: Int) extends Literal
  case class BoolLiteral(v: Boolean) extends Literal

  // operators
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