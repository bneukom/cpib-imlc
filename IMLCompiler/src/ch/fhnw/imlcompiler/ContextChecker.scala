package ch.fhnw.imlcompiler

import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList
import ch.fhnw.imlcompiler.AST._

// TODO do not use exceptions but rather a CheckResult which is either a CheckError or a IMLContext (which is then used by the code generator)
// TODO merge with scope checker (and other checkers too)?
// TODO into single context checker which returns context used by code generator?
trait ContextCheckers {

  case class DuplicateIdentException(ident: Ident) extends CompilerException("Duplicate decleration of " + ident + " at " + ident.pos.line + ":" + ident.pos.column + "\n" + ident.pos.longString + "\nAST: " + ident)
  case class InvalidDeclException(decl: Decl) extends CompilerException("Invalid decleration of " + decl + " at " + decl.pos.line + ":" + decl.pos.column + "\n" + decl.pos.longString + "\nAST: " + decl)
  case class TypeErrorException(n: ASTNode, epxected: Type) extends CompilerException("Type " + epxected + " expected at " + n.pos.line + ":" + n.pos.column + "\n" + n.pos.longString + "\nAST: " + n)
  case class IncompatibleTypeException(n: ASTNode) extends CompilerException("Incompatible types at " + n.pos.line + ":" + n.pos.column + "\n" + n.pos.longString + "\nAST: " + n)
  case class UndefinedVariableException(n: Ident) extends CompilerException("Undefined variable '" + n.value + "' used at " + n.pos.line + ":" + n.pos.column + "\n" + n.pos.longString + "\nAST: " + n);

  // TODO how to make a nice call chain with these case classes?
  abstract sealed class ContextResult
  case class ContextFailure
  case class Context()

  // TODO global fun/procs and variables
  // TODO local variables
  // TODO initialized state of variables must be stored
  case class GlobalVarScope(decls: MutableList[TypedIdent])
  case class LocalVarScopes(scopes: HashMap[Ident, MutableList[TypedIdent]]) // includes global imports
  case class GlobalMethodScope(decls: MutableList[Ident])

  private val globalVarScope: GlobalVarScope = GlobalVarScope(new MutableList());
  private val localVarScope: LocalVarScopes = LocalVarScopes(new HashMap());
  private val globalMethodScope: GlobalMethodScope = GlobalMethodScope(new MutableList());

  def check(prog: Program) {
    // load global decls
    globalCpsDecl(prog.cpsDecl);

    // check the main of the program
    checkCpsCmd(prog.cmd, globalVarScope.decls);

    println(globalVarScope)
    println(globalMethodScope)
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
      // TODO implement other expressions
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
        if (returnType(lhs, scope) != expectedOperandType(opr) || returnType(rhs, scope) != expectedOperandType(opr)) throw TypeErrorException(e, expectedOperandType(opr))
      case MonadicExpr(lhs, opr) =>
        checkExpr(lhs, scope);
        if (returnType(lhs, scope) != expectedOperandType(opr)) throw TypeErrorException(e, expectedOperandType(opr))
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
        if (returnType(lhs, scope) != returnType(rhs, scope)) throw IncompatibleTypeException(cmd)
      }
      case SkipCmd() => {}
      // TODO implement others
    }
  }

  def globalCpsDecl(cpsDecl: Option[CpsDecl]) = cpsDecl.foreach(x => x.declList.foreach(decl => globalDecl(decl)))

  def globalDecl(decl: Decl) {
    decl match {
      case VarDecl(_, i) => globalVarScope.decls += i;
      case FunDecl(identifier, params, _, _, imports, cpsDecl, cmd) => methodDecl(identifier, cpsDecl, params, cmd);
      case ProcDecl(identifier, params, imports, cpsDecl, cmd) => methodDecl(identifier, cpsDecl, params, cmd);

    }
  }

  def methodDecl(identifier: Ident, cpsDecl: Option[CpsDecl], paramList: ParamList, cpsCmd: CpsCmd) {
    globalMethodScope.decls += identifier
    localCpsDecl(identifier, cpsDecl)
    paramDecl(identifier, paramList)
    checkCpsCmd(cpsCmd, localVarScope.scopes.getOrElse(identifier, new MutableList()));
  }

  def paramDecl(ident: Ident, params: ParamList) {
    params.p.foreach(param => {
      localVarScope.scopes.get(ident) match {
        case None => localVarScope.scopes.put(ident, new MutableList[TypedIdent]() += param.t)
        case Some(x) => if (!x.contains(param.t)) x += param.t else throw new DuplicateIdentException(param.t.i)
      }
    })
  }

  def localCpsDecl(ident: Ident, cpsDecl: Option[CpsDecl]) = {
    cpsDecl.foreach(x => x.declList.foreach(decl => localDecl(ident, decl)))
  }

  def localDecl(ident: Ident, decl: Decl) {
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