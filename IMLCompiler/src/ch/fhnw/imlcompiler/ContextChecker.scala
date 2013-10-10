package ch.fhnw.imlcompiler

import ch.fhnw.imlcompiler.AST._
import ch.fhnw.imlcompiler.AST.AddOpr
import scala.collection.immutable.Map
import scala.collection.mutable.HashMap
import ch.fhnw.imlcompiler.AST.ASTNode

trait ContextCheckers {
  def check(prog: Program) {
    ScopeChecker.check(prog);
    TypeChecker.check(prog);
  }

  case class TypeCheckerException(n: ASTNode, epxected: Type) extends CompilerException("Type error: " + epxected + " expected at " + n.pos.line + ":" + n.pos.column + "\n" + n.pos.longString + "\nAST: " + n)
  case class ScopeCheckerException(n: ASTNode, outOfScope:ASTNode) extends CompilerException(n)

  /**
   * Checks if variable access is in scope and identifiers are unique.
   */
  object ScopeChecker extends ContextCheckers {

  }

  // TODO merge with scope checker?
  // TODO into single context checker which returns context used by code generator?
  /**
   * Checks expression type errors.
   */
  object TypeChecker extends ContextCheckers {

    // TODO global fun/procs and variables
    // TODO local variables
    case class TypeScope(decls: HashMap[Ident, Decl])

    val scope: TypeScope = TypeScope(new HashMap());

    override def check(prog: Program) = {
      checkCpsDecl(prog.cpsDecl);
      checkCpsCmd(prog.cmd);
    }

    def getType(l: Literal): Type = {
      l match {
        case IntLiteral(_) => IntType
        case BoolLiteral(_) => BoolType
      }
    }

    def expectedOperandType(o: Opr): Type = {
      o match {
        case PlusOpr | MinusOpr => IntType
        case DivOpr | TimesOpr | ModOpr => IntType
        case EQ | NE | GT | LT | GE | LE => IntType
        case Cand | Cor | Not => BoolType
      }
    }

    /**
     * Returns the expected operand
     */
    def returnType(e: Expr): Type = {
      e match {
        case DyadicExpr(_, opr, _) => returnType(opr)
        case MonadicExpr(_, opr) => returnType(opr)
        case LiteralExpr(l) => getType(l)
        // case FunCallExpr(i, _) => scope.decls.get(i) match { case FunDecl(_) => } TODO if FunDecl is not in global store -> ScopeException!
      }
    }

    /**
     * Returns the Type a given Operator returns
     */
    def returnType(o: Opr): Type = {
      o match {
        case PlusOpr | MinusOpr => IntType
        case DivOpr | TimesOpr | ModOpr => IntType
        case EQ | NE | GT | LT | GE | LE => BoolType
        case Cand | Cor | Not => BoolType
      }
    }

    def checkExpr(e: Expr) {
      e match {
        case DyadicExpr(lhs, opr, rhs) =>
          checkExpr(lhs); checkExpr(rhs); if (returnType(lhs) != expectedOperandType(opr) || returnType(rhs) != expectedOperandType(opr)) throw TypeCheckerException(e, expectedOperandType(opr))
        case MonadicExpr(lhs, opr) =>
          checkExpr(lhs); if (returnType(lhs) != expectedOperandType(opr)) throw TypeCheckerException(e, expectedOperandType(opr))
        case LiteralExpr(_) => {}
        case FunCallExpr(_, el) => {} // TODO type of every expression must be compatible with target type from function and also expression must not have type errors
      }
    }

    def checkCpsCmd(cmd: CpsCmd) {
      //      cmd match {
      //        case 
      //      }
    }

    def checkCpsDecl(cpsDecl: Option[CpsDecl]) = {
      cpsDecl match {
        case Some(x) => x.declList.foreach(x => checkDecl(x))
        case None =>
      }
    }

    def checkDecl(decl: Decl) {
      decl match {
        case VarDecl(_, i) =>
      }
    }
  }

}