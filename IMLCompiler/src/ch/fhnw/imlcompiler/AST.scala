package ch.fhnw.imlcompiler

import scala.util.parsing.input.Positional
import scala.collection.immutable.List

object AST {
  case class Program(cpsDecl: Option[CpsDecl], cmd: CpsCmd)

  case class TypedIdent(i: Ident, t: Type)

  case class Parameter(f: Option[FlowMode], m: Option[MechMode], c: Option[ChangeMode], t: TypedIdent)
  case class ParamList(p: List[Parameter])

  sealed class Decl
  case class VarDecl(c: ChangeMode, i: TypedIdent) extends Decl;
  case class FunDecl(ident: Ident, params: ParamList, cm: Option[ChangeMode], retIdent: TypedIdent, importList: Option[GlobImpList], cpsDecl: Option[CpsDecl], cmd: CpsCmd) extends Decl
  case class ProcDecl(ident: Ident, params: ParamList, globImpList: Option[GlobImpList], cpsDecl: Option[CpsDecl], cmd: CpsCmd) extends Decl
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
  case object In extends FlowMode
  case object Out extends FlowMode
  case object InOut extends FlowMode

  sealed class MechMode
  case object Ref extends MechMode
  case object Copy extends MechMode

  sealed class ChangeMode
  case object Const extends ChangeMode
  case object Var extends ChangeMode

  case class TupleExpr(l: List[Expr])

  // expressions
  sealed class Expr
  case class DyadicExpr(l: Expr, op: Opr, r: Expr) extends Expr
  case class MonadicExpr(l: Expr, op: Opr) extends Expr

  case class Ident(value: String)
  case class FunCallExpr(i: Ident, e: TupleExpr) extends Expr;
  case class StoreExpr(i: Ident, isInitialization: Boolean) extends Expr;
  case class LiteralExpr(l: Literal) extends Expr;

  sealed class Factor;
  sealed class Literal extends Factor;
  case class IntLiteral(v: Int) extends Literal
  case class BoolLiteral(v: Boolean) extends Literal

  // type
  sealed class Type
  case object IntType extends Type
  case object BoolType extends Type

  // operators
  sealed class Opr
  sealed class RelOpr extends Opr;
  case object EQ extends RelOpr;
  case object NE extends RelOpr;
  case object GT extends RelOpr;
  case object LT extends RelOpr;
  case object GE extends RelOpr;
  case object LE extends RelOpr;

  sealed class BoolOpr extends Opr;
  case object Cand extends BoolOpr;
  case object Cor extends BoolOpr;

  sealed class MultOpr extends Opr
  case object DivOpr extends MultOpr
  case object TimesOpr extends MultOpr
  case object ModOpr extends MultOpr

  sealed class AddOpr extends Opr
  case object PlusOpr extends AddOpr
  case object MinusOpr extends AddOpr

  case object Not extends Opr

}