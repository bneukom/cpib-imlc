package ch.fhnw.imlcompiler

import scala.util.parsing.input.Positional
import scala.collection.immutable.List

object AST {
  case class Program(cpsDecl:Option[CpsDecl], cmd: CpsCmd)

  case class TypedIdent(i: Ident, t: Type)

  case class Parameter(f: Option[FlowMode], m: Option[MechMode], c: Option[ChangeMode], t: TypedIdent)
  case class ParamList(p: List[Parameter])

  case class Decl(c: ChangeMode, i: TypedIdent, m: MethodDecl)
  sealed class MethodDecl;
  case class FunDecl(ident: Ident, params: ParamList, cm: Option[ChangeMode], retIdent: TypedIdent, importList: Option[GlobImpList], cpsDecl: Option[CpsDecl], cmd: CpsCmd) extends MethodDecl
  case class ProcDecl extends MethodDecl
  case class CpsDecl(d: List[Decl])

  case class GlobImport(f: FlowMode, c: ChangeMode, i: Ident)
  case class GlobImpList(i: List[GlobImport])

  sealed class Cmd
  case class BecomesCmd(lhs: Expr, rhs: Expr) extends Cmd
  case class WhileCmd(expr: Expr, cmd: CpsCmd) extends Cmd
  case class IfCmd(expr: Expr, ifCmd: CpsCmd, elseCmd: CpsCmd) extends Cmd
  case class SkipCmd() extends Cmd
  case class CallCmd(i: Ident, e: TupleExpr) extends Cmd
  case class InputCmd(expr: Expr) extends Cmd
  case class OutputCmd(expr: Expr) extends Cmd
  case class CpsCmd(cl: List[Cmd])

  // modes
  sealed class FlowMode
  case class In extends FlowMode
  case class Out extends FlowMode
  case class InOut extends FlowMode

  sealed class MechMode
  case class Ref extends MechMode
  case class Copy extends MechMode

  sealed class ChangeMode
  case class Const extends ChangeMode
  case class Var extends ChangeMode

  case class TupleExpr(l: List[Expr])

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

  // type
  sealed class Type
  case class IntType extends Type
  case class BoolType extends Type

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