package ch.fhnw.imlcompiler

import scala.util.parsing.input.Positional
import scala.collection.immutable.List
import sun.org.mozilla.javascript.internal.ast.AstNode

object AST {

  val keywords = List("bool", "call", "const", "copy", "debugin", "debugout", "div", "do", "else", "endfun", "endif", "endproc", "endprogram", "endwhile", "false", "fun", "global", "if", "in", "init", "inout", "int", "local", "mod", "not", "out", "proc", "program", "ref", "returns", "skip", "then", "true", "var", "while")

  class ASTNode extends Positional

  case class Program(params: List[ProgParameter], cpsDecl: List[Decl], cmd: List[Cmd]) extends ASTNode

  case class TypedIdent(i: Ident, t: Type) extends ASTNode

  case class Parameter(f: Option[FlowMode], m: Option[MechMode], c: Option[ChangeMode], ti: TypedIdent) extends ASTNode
  case class ProgParameter(f: Option[FlowMode], c: Option[ChangeMode], ti: TypedIdent) extends ASTNode

  sealed abstract class Decl extends ASTNode
  case class StoreDecl(c: ChangeMode, ti: TypedIdent) extends Decl;
  case class FunDecl(ident: Ident, params: List[Parameter], returns: StoreDecl, importList: List[GlobImport], cpsDecl: List[StoreDecl], cmd: List[Cmd]) extends Decl
  case class ProcDecl(ident: Ident, params: List[Parameter], globImpList: List[GlobImport], cpsDecl: List[StoreDecl], cmd: List[Cmd]) extends Decl

  case class GlobImport(f: Option[FlowMode], c: Option[ChangeMode], i: Ident) extends ASTNode

  sealed abstract class Cmd extends ASTNode
  case class BecomesCmd(lhs: Expr, rhs: Expr) extends Cmd
  case class WhileCmd(expr: Expr, cmd: List[Cmd]) extends Cmd
  case class IfCmd(expr: Expr, ifCmd: List[Cmd], elseCmd: List[Cmd]) extends Cmd
  case class SkipCmd() extends Cmd
  case class CallCmd(i: Ident, e: TupleExpr) extends Cmd
  case class InputCmd(expr: Expr) extends Cmd
  case class OutputCmd(expr: Expr) extends Cmd

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

  // TODO class not needed
  case class TupleExpr(l: List[Expr]) extends ASTNode

  case class Ident(value: String) extends ASTNode

  // expressions
  sealed abstract class Expr extends ASTNode
  case class DyadicExpr(l: Expr, op: Opr, r: Expr) extends Expr
  case class MonadicExpr(l: Expr, op: Opr) extends Expr
  case class FunCallExpr(i: Ident, e: TupleExpr) extends Expr;
  case class StoreExpr(i: Ident, isInitialization: Boolean) extends Expr;
  case class LiteralExpr(l: Literal) extends Expr;
  case class ListExpr(l: List[Either[ListExpr, Literal]])
  // TODO ListExpr?

  abstract sealed class Factor extends ASTNode;
  abstract sealed class Literal extends Factor;
  case class IntLiteral(v: Int) extends Literal
  case class BoolLiteral(v: Boolean) extends Literal

  // type
  sealed class Type extends Positional
  case class ListType(t: Type) extends Type { override def toString() = "[" + t + "]" }
  sealed class AtomType extends Type
  case object IntType extends AtomType { override def toString() = "int32" }
  case object BoolType extends AtomType { override def toString() = "bool" }

  // operators
  abstract sealed class Opr extends Positional
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
  case object Not extends BoolOpr;

  abstract sealed class MultOpr extends Opr
  case object DivOpr extends MultOpr { override def toString() = "div" }
  case object TimesOpr extends MultOpr { override def toString() = "times" }
  case object ModOpr extends MultOpr { override def toString() = "mod" }

  abstract sealed class AddOpr extends Opr
  case object PlusOpr extends AddOpr { override def toString() = "plus" }
  case object MinusOpr extends AddOpr { override def toString() = "minus" }

  abstract sealed class ListOpr extends Opr
  case object HeadOpr extends ListOpr { override def toString() = "head" }
  case object TailOpr extends ListOpr { override def toString() = "head" }
  case object ConcatOpr extends ListOpr { override def toString() = "::" }

}