package ch.fhnw.imlcompiler

import ch.fhnw.imlcompiler.AST._
import ch.fhnw.imlcompiler.AST.AddOpr

trait ContextChecker {
  def check(prog: Program) {
  }

  object TypeChecker extends ContextChecker {
    override def check(prog: Program) = {
      checkCpsDecl(prog.cpsDecl);
      checkCpsCmd(prog.cmd);
    }

    def getType(l: LiteralExpr): Type = {
      getType(l.l)
    }

    def getType(l: Literal): Type = {
      l match {
        case IntLiteral(_) => IntType
        case BoolLiteral(_) => BoolType
      }
    }

    def getType(o: Opr): Type = {
      o match {
        case PlusOpr => IntType;
        case MinusOpr => IntType;
        case TimesOpr => IntType;
        case DivOpr => IntType;
        case _ => IntType;
      }
    }

    def getType(e: Expr): Type = {
      e match {
        case DyadicExpr(_, opr, _) => getType(opr)
        case MonadicExpr(_, opr) => getType(opr)
        case LiteralExpr(l) => getType(l);
      }
    }

    def checkExpr(e: Expr) {
      e match {
        case DyadicExpr(lhs, _, rhs) => {
          checkExpr(lhs)
          checkExpr(rhs)

          if (getType(lhs) != getType(rhs)) {
            throw new RuntimeException("Type error: " + e)
          }

        }
        case LiteralExpr(_) => {}
        case FunCallExpr(_, el) => {}
      }
    }

    def checkCpsCmd(cmd: CpsCmd) {
      //      cmd match {
      //        case 
      //      }
    }

    def checkCpsDecl(cpsDecl: Option[CpsDecl]) = {}
    //    	cpsDecl match {
    //    	  case Some[d:CpsDecl] => 
    //    	}

  }
}