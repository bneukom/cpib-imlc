package ch.fhnw.imlcompiler

import scala.util.parsing.input.Positional
import scala.collection.immutable.List
import sun.org.mozilla.javascript.internal.ast.AstNode

object AST {

  class ASTNode extends Positional

  case class Program(cpsDecl: Option[CpsDecl], cmd: CpsCmd) extends ASTNode

  case class TypedIdent(i: Ident, t: Type) extends ASTNode

  case class Parameter(f: Option[FlowMode], m: Option[MechMode], c: Option[ChangeMode], t: TypedIdent) extends ASTNode
  case class ParamList(p: List[Parameter]) extends ASTNode

  sealed abstract class Decl extends ASTNode
  case class VarDecl(c: ChangeMode, i: TypedIdent) extends Decl;
  case class FunDecl(ident: Ident, params: ParamList, cm: Option[ChangeMode], retIdent: TypedIdent, importList: Option[GlobImpList], cpsDecl: Option[CpsDecl], cmd: CpsCmd) extends Decl
  case class ProcDecl(ident: Ident, params: ParamList, globImpList: Option[GlobImpList], cpsDecl: Option[CpsDecl], cmd: CpsCmd) extends Decl

  case class CpsDecl(declList: List[Decl])

  case class GlobImport(f: FlowMode, c: ChangeMode, i: Ident) extends ASTNode
  case class GlobImpList(i: List[GlobImport]) extends ASTNode

  sealed class Cmd extends ASTNode
  case class BecomesCmd(lhs: Expr, rhs: Expr) extends Cmd
  case class WhileCmd(expr: Expr, cmd: CpsCmd) extends Cmd
  case class IfCmd(expr: Expr, ifCmd: CpsCmd, elseCmd: CpsCmd) extends Cmd
  case class SkipCmd() extends Cmd
  case class CallCmd(i: Ident, e: TupleExpr) extends Cmd
  case class InputCmd(expr: Expr) extends Cmd
  case class OutputCmd(expr: Expr) extends Cmd
  case class CpsCmd(cl: List[Cmd]) extends ASTNode

  // modes
  sealed class FlowMode extends ASTNode
  case object In extends FlowMode
  case object Out extends FlowMode
  case object InOut extends FlowMode

  sealed class MechMode extends ASTNode
  case object Ref extends MechMode
  case object Copy extends MechMode

  sealed class ChangeMode extends ASTNode
  case object Const extends ChangeMode
  case object Var extends ChangeMode

  case class TupleExpr(l: List[Expr]) extends ASTNode

  // expressions
  sealed abstract class Expr extends ASTNode
  case class DyadicExpr(l: Expr, op: Opr, r: Expr) extends Expr
  case class MonadicExpr(l: Expr, op: Opr) extends Expr

  case class Ident(value: String) extends ASTNode
  case class FunCallExpr(i: Ident, e: TupleExpr) extends Expr;
  case class StoreExpr(i: Ident, isInitialization: Boolean) extends Expr;
  case class LiteralExpr(l: Literal) extends Expr;

  abstract sealed class Factor extends ASTNode;
  abstract sealed class Literal extends Factor;
  case class IntLiteral(v: Int) extends Literal
  case class BoolLiteral(v: Boolean) extends Literal

  // type
  sealed class Type
  case object IntType extends Type { override def toString() = "int32" }
  case object BoolType extends Type { override def toString() = "bool" }

  // operators
  abstract sealed class Opr
  abstract sealed class RelOpr extends Opr;
  case object EQ extends RelOpr;
  case object NE extends RelOpr;
  case object GT extends RelOpr;
  case object LT extends RelOpr;
  case object GE extends RelOpr;
  case object LE extends RelOpr;

  abstract sealed class BoolOpr extends Opr;
  case object Cand extends BoolOpr;
  case object Cor extends BoolOpr;

  abstract sealed class MultOpr extends Opr
  case object DivOpr extends MultOpr { override def toString() = "div" }
  case object TimesOpr extends MultOpr { override def toString() = "times" }
  case object ModOpr extends MultOpr { override def toString() = "mod" }

  abstract sealed class AddOpr extends Opr
  case object PlusOpr extends AddOpr { override def toString() = "plus" }
  case object MinusOpr extends AddOpr { override def toString() = "minus" }

  case object Not extends Opr

}