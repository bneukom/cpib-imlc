package ch.fhnw.imlcompiler

import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList
import ch.fhnw.imlcompiler.AST._

// TODO do not use exceptions but rather a CheckResult which is either a CheckError or a IMLContext (which is then used by the code generator)
trait ContextCheckers {

  case class DuplicateIdentException(ident: Ident) extends CompilerException("(" + ident.pos.line + ":" + ident.pos.column + ") '" + ident.value + "' is already defined\n\n" + ident.pos.longString + "\nAST: " + ident)
  case class InvalidDeclException(decl: Decl) extends CompilerException("(" + decl.pos.line + ":" + decl.pos.column + ") " + decl.pos.longString + "invalid decleration of " + decl + "\n\nAST: " + decl)
  case class OperandTypeErrorException(n: ASTNode, expected: Type, opr: Opr) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") wrong operand for operator: " + opr + " required: " + expected + "\n\n" + n.pos.longString + "\nAST: " + n)
  case class TypeMismatchError(n: ASTNode, required: Type, found: Type) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") type mismatch; found: " + found + " required: " + required + "\n\n" + n.pos.longString + "\nAST: " + n)
  case class UndefinedVariableException(n: Ident) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") undefined variable '" + n.value + "' used\n\n" + n.pos.longString + "\nAST: " + n);
  case class UndefinedMethodException(n: Ident) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") undefined method '" + n.value + "' used\n\n" + n.pos.longString + "\nAST: " + n);
  case class ProcReturnException(n: Ident) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") proc method '" + n.value + "' do not have return types\n\n" + n.pos.longString + "\nAST: " + n);
  case class InvalidParamaterAmount(n: TupleExpr, required: Int, actual: Int) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") invalid amount of parameters " + actual + " required: " + required + "\n\n" + n.pos.longString + "\nAST: " + n);
  case class InvalidParamater(n: Expr, required: Type, actual: Type) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") invalid parameter type '" + actual + "' required: " + required + "\n\n" + n.pos.longString + "\nAST: " + n);

  case class Context(globalVarScope: GlobalVarScope, localVarScope: LocalVarScopes, globalMethodScope: GlobalMethodScope)

  // TODO initialized state of variables must be stored
  case class GlobalVarScope(decls: MutableList[TypedIdent])
  case class LocalVarScopes(scopes: HashMap[Ident, MutableList[TypedIdent]]) // includes global imports
  case class GlobalMethodScope(decls: HashMap[Ident, Decl])

  private val globalVarScope: GlobalVarScope = GlobalVarScope(new MutableList());
  private val localVarScope: LocalVarScopes = LocalVarScopes(new HashMap());
  private val globalMethodScope: GlobalMethodScope = GlobalMethodScope(new HashMap());

  def check(prog: Program): Context = {
    // load global declarations first (so we don't need any forward declarations)
    loadGlobalCpsDecl(prog.cpsDecl);

    println("Scope loaded: ")
    println(globalVarScope)
    println(localVarScope)
    println(globalMethodScope)
    println()

    // check all defined methods for possible errors (type, flow, etc.)
    checkMethods(prog.cpsDecl);

    // check the main of the program
    checkCpsCmd(prog.cmd, globalVarScope.decls);

    return Context(globalVarScope, localVarScope, globalMethodScope)
  }

  def getType(l: Literal): Type = {
    l match {
      case IntLiteral(_) => IntType
      case BoolLiteral(_) => BoolType
    }
  }

  // TODO for lists we need lhs and rhs operand types
  def expectedOperandType(o: Opr): Type = {
    o match {
      case PlusOpr | MinusOpr => IntType
      case DivOpr | TimesOpr | ModOpr => IntType
      case EQ | NE | GT | LT | GE | LE => IntType
      case Cand | Cor | Not => BoolType
    }
  }

  def returnType(e: Expr, scope: MutableList[TypedIdent]): Type = {
    e match {
      case DyadicExpr(_, opr, _) => returnType(opr)
      case MonadicExpr(_, opr) => returnType(opr)
      case LiteralExpr(l) => getType(l)
      case StoreExpr(i, _) => {
        scope.find(x => x.i == i) match {
          case None => throw UndefinedVariableException(i);
          case Some(typedIdent) => typedIdent.t;
        }
      }
      case FunCallExpr(i, _) => {
        globalMethodScope.decls.find(x => x == i) match {
          case None => throw UndefinedMethodException(i)
          case Some(m) => {
            m._2 match {
              case FunDecl(_, _, _, r, _, _, _) => r.t
              case _ => throw ProcReturnException(i)
            }
          }
        }
      }
    }
  }

  def returnType(o: Opr): Type = {
    o match {
      case PlusOpr | MinusOpr => IntType
      case DivOpr | TimesOpr | ModOpr => IntType
      case EQ | NE | GT | LT | GE | LE => BoolType
      case Cand | Cor | Not => BoolType
    }
  }

  def checkExpr(e: Expr, scope: MutableList[TypedIdent]) {
    e match {
      case DyadicExpr(lhs, opr, rhs) =>
        checkExpr(lhs, scope); checkExpr(rhs, scope);
        if (returnType(lhs, scope) != expectedOperandType(opr) || returnType(rhs, scope) != expectedOperandType(opr)) throw OperandTypeErrorException(e, expectedOperandType(opr), opr)
      case MonadicExpr(lhs, opr) =>
        checkExpr(lhs, scope);
        if (returnType(lhs, scope) != expectedOperandType(opr)) throw OperandTypeErrorException(e, expectedOperandType(opr), opr)
      case LiteralExpr(_) => {}
      case FunCallExpr(_, el) => {} // TODO type of every expression must be compatible with target type from function and also expression must not have type errors
      case StoreExpr(i, _) => {} // TODO lowest level nothing more to check here?
    }
  }

  def checkCpsCmd(cpsCmd: CpsCmd, scope: MutableList[TypedIdent]) = cpsCmd.cl.foreach(cmd => checkCmd(cmd, scope))
  def checkCmd(cmd: Cmd, scope: MutableList[TypedIdent]) {
    cmd match {
      case BecomesCmd(lhs, rhs) => {
        checkExpr(lhs, scope); checkExpr(rhs, scope);
        if (returnType(lhs, scope) != returnType(rhs, scope)) throw TypeMismatchError(cmd, returnType(lhs, scope), returnType(rhs, scope))
      }
      case SkipCmd() => {}
      case CallCmd(i, t) => {
        globalMethodScope.decls.get(i) match {
          case None => throw UndefinedMethodException(i)
          case Some(decl) => {
            checkTupleArgs(decl, t, scope);
          }
        }
      }
      case IfCmd(e, ifcmd, elsecmd) =>
        if (returnType(e, scope) != BoolType) throw TypeMismatchError(cmd, BoolType, returnType(e, scope)); checkExpr(e, scope); checkCpsCmd(ifcmd, scope); checkCpsCmd(elsecmd, scope)
      case WhileCmd(e, cmd) =>
        if (returnType(e, scope) != BoolType) throw TypeMismatchError(cmd, BoolType, returnType(e, scope)); checkExpr(e, scope); checkCpsCmd(cmd, scope);
      case InputCmd(e) => // TODO implement
      case OutputCmd(e) => // TODO implement

    }
  }

  def checkTupleArgs(decl: Decl, tupleExpr: TupleExpr, scope: MutableList[TypedIdent]) {
    // check if types of passed values matches with the proc/fun
    decl match {
      case VarDecl(_, i) => throw new CompilerException(i.i + " is not a proc or fun");
      case FunDecl(identifier, params, _, _, imports, cpsDecl, cmd) => checkMethodTupleArgs(tupleExpr, params, scope)
      case ProcDecl(identifier, params, imports, cpsDecl, cmd) => checkMethodTupleArgs(tupleExpr, params, scope)
    }
  }

  def checkMethodTupleArgs(tupleExpr: TupleExpr, params: ParamList, scope: MutableList[TypedIdent]) {
    if (tupleExpr.l.size != params.p.size) throw InvalidParamaterAmount(tupleExpr, params.p.size, tupleExpr.l.size);

    val o = tupleExpr.l.zip(params.p);
    o.foreach(element => {
      val exprReturnType = returnType(element._1, scope);
      if (exprReturnType != element._2.t.t) throw InvalidParamater(element._1, element._2.t.t, exprReturnType)
      checkExpr(element._1, scope)
    })
  }

  def loadGlobalCpsDecl(cpsDecl: Option[CpsDecl]) = cpsDecl.foreach(x => x.declList.foreach(decl => loadGlobalDecl(decl)))
  def loadGlobalDecl(decl: Decl) {
    decl match {
      case VarDecl(_, i) => globalVarScope.decls += i;
      case FunDecl(identifier, params, _, _, imports, cpsDecl, cmd) => loadMethodDecl(identifier, decl, cpsDecl, params, cmd);
      case ProcDecl(identifier, params, imports, cpsDecl, cmd) => loadMethodDecl(identifier, decl, cpsDecl, params, cmd);

    }
  }

  def checkMethods(cpsDecl: Option[CpsDecl]) = cpsDecl.foreach(x => x.declList.foreach(decl => checkMethod(decl)))
  def checkMethod(decl: Decl) {
    decl match {
      case FunDecl(identifier, params, _, _, imports, cpsDecl, cmd) => checkCpsCmd(cmd, localVarScope.scopes.getOrElse(identifier, new MutableList()));
      case ProcDecl(identifier, params, imports, cpsDecl, cmd) => checkCpsCmd(cmd, localVarScope.scopes.getOrElse(identifier, new MutableList()));
      case VarDecl(_, i) =>

    }
  }

  def loadMethodDecl(identifier: Ident, methodDecl: Decl, cpsDecl: Option[CpsDecl], paramList: ParamList, cpsCmd: CpsCmd) {
    if (globalMethodScope.decls.contains(identifier)) throw new DuplicateIdentException(identifier)

    globalMethodScope.decls += (identifier -> methodDecl)
    loadLocalCpsDecl(identifier, cpsDecl)
    loadParamDecl(identifier, paramList)
  }

  def loadParamDecl(ident: Ident, params: ParamList) {
    params.p.foreach(param => {
      localVarScope.scopes.get(ident) match {
        case None => localVarScope.scopes.put(ident, new MutableList[TypedIdent]() += param.t)
        case Some(x) => if (x.find(ti => ti.i == param.t.i).isEmpty) x += param.t else throw new DuplicateIdentException(param.t.i)
      }
    })
  }

  def loadLocalCpsDecl(ident: Ident, cpsDecl: Option[CpsDecl]) = {
    cpsDecl.foreach(x => x.declList.foreach(decl => loadLocalDecl(ident, decl)))
  }

  def loadLocalDecl(ident: Ident, decl: Decl) {
    decl match {
      case VarDecl(_, i) => {
        localVarScope.scopes.get(ident) match {
          case None => localVarScope.scopes.put(ident, new MutableList[TypedIdent] += i)
          case Some(scopeList) => if (!scopeList.contains(i)) scopeList += i else throw new DuplicateIdentException(i.i)
        }
      }
      // can not declare anything else inside method declarations
      case _ => throw InvalidDeclException(decl);
    }
  }
}