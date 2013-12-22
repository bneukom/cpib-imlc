package ch.fhnw.imlcompiler

import scala.util.parsing.input.Positional
import scala.collection.immutable.List
import sun.org.mozilla.javascript.internal.ast.AstNode

object AST {

  final val keywords = List("bool", "call", "const", "copy", "debugin", "debugout", "div", "do", "else", "endfun", "endif", "endproc", "endprogram", "endwhile", "false", "fun", "global", "if", "in", "init", "inout", "int", "local", "mod", "not", "out", "proc", "program", "ref", "returns", "skip", "then", "true", "var", "while", "head", "tail", "length")

  class ASTNode extends Positional

  case class Program(name: Ident, params: List[ProgParameter], cpsDecl: List[Decl], commands: List[Cmd]) extends ASTNode

  case class TypedIdent(i: Ident, t: Type) extends ASTNode {
    def copy(): TypedIdent = {
    	val r = TypedIdent(i,t);
    	r.pos = pos;
    	return r;
    }
  }

  case class Parameter(f: Option[FlowMode], m: Option[MechMode], c: Option[ChangeMode], ti: TypedIdent) extends ASTNode
  case class ProgParameter(f: Option[FlowMode], c: Option[ChangeMode], ti: TypedIdent) extends ASTNode

  sealed abstract class Decl extends ASTNode
  case class StoreDecl(c: ChangeMode, ti: TypedIdent) extends Decl;
  case class FunDecl(ident: Ident, params: List[Parameter], returns: StoreDecl, importList: List[GlobImport], cpsDecl: List[StoreDecl], cmds: List[Cmd]) extends Decl
  case class ProcDecl(ident: Ident, params: List[Parameter], globImpList: List[GlobImport], decls: List[StoreDecl], cmds: List[Cmd]) extends Decl

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

  // TODO class not needed REMOVE!!!
  case class TupleExpr(l: List[Expr]) extends ASTNode

  case class Ident(value: String) extends ASTNode

  // expressions
  sealed abstract class Expr extends ASTNode
  case class DyadicExpr(l: Expr, op: DyadicOpr, r: Expr) extends Expr
  case class MonadicExpr(l: Expr, op: MonadicOpr) extends Expr
  case class FunCallExpr(i: Ident, e: TupleExpr) extends Expr;
  case class StoreExpr(i: Ident, isInitialization: Boolean) extends Expr;
  case class LiteralExpr(l: Literal) extends Expr;
  case class ListExpr(output: Expr, i: Ident, from: Expr, to: Expr, predicate: Expr) extends Expr;

  abstract sealed class Factor extends ASTNode;
  abstract sealed class Literal extends Factor;
  case class IntLiteral(v: Int) extends Literal
  case class BoolLiteral(v: Boolean) extends Literal
  case class ListLiteral(l: List[Expr]) extends Literal

  // type
  abstract sealed class Type extends Positional {
    def matches(other: Type): Boolean = this == other
  }
  case class ListType(t: Type) extends Type {
    override def toString() = "[" + t + "]";
    override def matches(other: Type): Boolean = {
      // [[[int]]] should also match [int|bool] or [[int|bool]] etc.
      if (deepType(other) == Any && listLevel(other) <= listLevel(this)) return true;

      other match {
        case ListType(otherType) => otherType.matches(t);
        case _ => other.matches(this);
      }
    }

    private def listLevel(l: Type, level: Int = 0): Int =
      l match {
        case ListType(i) => return listLevel(i, level + 1);
        case _ => level
      }

    private def deepType(l: Type): Type =
      l match {
        case ListType(i) => return deepType(i)
        case _ => l
      }
  }

  abstract sealed class AtomType extends Type { override def matches(other: Type) = other == this || other == Any }
  case object IntType extends AtomType { override def toString() = "int" }
  case object BoolType extends AtomType { override def toString() = "bool" }

  // phantom type
  case object Any extends AtomType {
    //    override def toString() = IntType.toString + "|" + BoolType.toString
    override def toString() = "any"
    override def matches(other: Type) = other.isInstanceOf[AtomType]
  }

  // operators
  abstract sealed class Opr extends Positional
  abstract sealed trait MonadicOpr extends Opr;
  abstract sealed trait DyadicOpr extends Opr;

  abstract sealed class RelOpr extends Opr with DyadicOpr
  case object EQ extends RelOpr;
  case object NE extends RelOpr;
  case object GT extends RelOpr;
  case object LT extends RelOpr;
  case object GE extends RelOpr;
  case object LE extends RelOpr;

  abstract sealed class BoolOpr extends Opr with DyadicOpr;
  case object Cand extends BoolOpr
  case object Cor extends BoolOpr
  case object Not extends Opr with MonadicOpr; // TODO not a boolopr?

  abstract sealed class MultOpr extends Opr with DyadicOpr
  case object DivOpr extends MultOpr { override def toString() = "div" }
  case object TimesOpr extends MultOpr { override def toString() = "times" }
  case object ModOpr extends MultOpr { override def toString() = "mod" }

  abstract sealed class AddOpr extends Opr with DyadicOpr with MonadicOpr
  case object PlusOpr extends AddOpr { override def toString() = "plus" }
  case object MinusOpr extends AddOpr { override def toString() = "minus" }

  abstract sealed class MonadicListOpr extends Opr with MonadicOpr
  case object HeadOpr extends MonadicListOpr { override def toString() = "head" }
  case object TailOpr extends MonadicListOpr { override def toString() = "tail" }
  case object LengthOpr extends MonadicListOpr { override def toString() = "length" }

  abstract sealed class DyadicListOpr extends Opr with DyadicOpr
  case object ConsOpr extends DyadicListOpr { override def toString() = "::" }

  def main(args: Array[String]) {
    val l1 = ListType(ListType(ListType(IntType)))
    val l2 = ListType(ListType(IntType))

    println(l1 + " matches " + l2 + ": " + l1.matches(l2))
    println(l2 + " matches " + l1 + ": " + l2.matches(l1))

  }
}