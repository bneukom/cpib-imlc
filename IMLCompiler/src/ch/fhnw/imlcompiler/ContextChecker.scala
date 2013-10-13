package ch.fhnw.imlcompiler

import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList
import ch.fhnw.imlcompiler.AST._

// TODO do not use exceptions but rather a CheckResult which is either a CheckError or a IMLContext (which is then used by the code generator)
trait ContextCheckers {

  case class DuplicateIdentException(ident: Ident) extends CompilerException("(" + ident.pos.line + ":" + ident.pos.column + ") '" + ident.value + "' is already defined\n\n" + ident.pos.longString + "\nAST: " + ident)
  case class InvalidDeclException(decl: Decl) extends CompilerException("(" + decl.pos.line + ":" + decl.pos.column + ") " + decl.pos.longString + "invalid decleration of " + decl + "\n\nAST: " + decl)
  // TODO change to TypeMismatchError (for lists lhs and rhs will not be the same "operand")
  case class OperandTypeErrorException(n: ASTNode, expected: Type, opr: Opr) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") wrong operand for operator: " + opr + " required: " + expected + "\n\n" + n.pos.longString + "\nAST: " + n)
  case class TypeMismatchError(n: ASTNode, required: Type, found: Type) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") type mismatch; found: " + found + " required: " + required + "\n\n" + n.pos.longString + "\nAST: " + n)
  case class UndefinedVariableException(n: Ident) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") undefined variable '" + n.value + "' used\n\n" + n.pos.longString + "\nAST: " + n);
  case class UndefinedMethodException(n: Ident) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") undefined method '" + n.value + "' used\n\n" + n.pos.longString + "\nAST: " + n);
  case class ProcReturnException(n: Ident) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") proc method '" + n.value + "' do not have return types\n\n" + n.pos.longString + "\nAST: " + n);
  case class InvalidParamaterAmount(n: TupleExpr, required: Int, actual: Int) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") invalid amount of parameters " + actual + " required: " + required + "\n\n" + n.pos.longString + "\nAST: " + n);
  case class InvalidParamater(n: Expr, required: Type, actual: Type) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") invalid parameter type '" + actual + "' required: " + required + "\n\n" + n.pos.longString + "\nAST: " + n);
  case class DuplicateInitialize(n: Ident) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") store: '" + n.value + "' has already been initialized\n\n" + n.pos.longString + "\nAST: " + n);
  case class StoreNotInitialized(n: Ident) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") store: '" + n.value + "' has not been initialized\n\n" + n.pos.longString + "\nAST: " + n);
  case class InvalidLValue(n: Expr) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") expression: '" + n + "' is not a valid lvalue\n\n" + n.pos.longString + "\nAST: " + n);
  case class ConstModification(n: Ident) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") identifier '" + n.value + "' is const and can not be modified\n\n" + n.pos.longString + "\nAST: " + n);

  case class Context(globalVarScope: GlobalStoreScope, localVarScope: LocalStoreScopes, globalMethodScope: GlobalMethodScope)

  // mech and flowmode only used for parameters
  // parameters are by default initialized!
  case class Store(typedIdent: TypedIdent, mech: Option[MechMode], change: Option[ChangeMode], flow: Option[FlowMode], var initialzed: Boolean)

  case class GlobalStoreScope(scope: MutableList[Store])
  case class LocalStoreScopes(scope: HashMap[Ident, MutableList[Store]]) // includes global imports and parameters
  case class GlobalMethodScope(decls: HashMap[Ident, Decl])

  private val globalStoreScope: GlobalStoreScope = GlobalStoreScope(new MutableList());
  private val localStoreScope: LocalStoreScopes = LocalStoreScopes(new HashMap());
  private val globalMethodScope: GlobalMethodScope = GlobalMethodScope(new HashMap());

  def check(prog: Program): Context = {
    // load global declarations first (so we don't need any forward declarations)
    loadGlobalCpsDecl(prog.cpsDecl);

    println("Scope loaded: ")
    println(globalStoreScope)
    println(localStoreScope)
    println(globalMethodScope)
    println()

    // check all defined methods for possible errors (type, flow, initialized, etc.)
    //    checkMethods(prog.cpsDecl);

    // check the main of the program for the same errors
    checkCpsCmd(prog.cmd, globalStoreScope.scope);

    return Context(globalStoreScope, localStoreScope, globalMethodScope)
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

  def returnType(e: Expr, scope: MutableList[Store]): Type = {
    e match {
      case DyadicExpr(_, opr, _) => returnType(opr)
      case MonadicExpr(_, opr) => returnType(opr)
      case LiteralExpr(l) => getType(l)
      case StoreExpr(i, _) => {
        scope.find(x => x.typedIdent.i == i) match {
          case None => throw UndefinedVariableException(i);
          case Some(store) => store.typedIdent.t;
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

  def checkExpr(e: Expr, scope: MutableList[Store], lvalue: Boolean = false) {
    e match {
      case DyadicExpr(lhs, opr, rhs) =>
        checkExpr(lhs, scope);
        checkExpr(rhs, scope);
        if (returnType(lhs, scope) != expectedOperandType(opr) || returnType(rhs, scope) != expectedOperandType(opr)) throw OperandTypeErrorException(e, expectedOperandType(opr), opr)
      case MonadicExpr(lhs, opr) =>
        checkExpr(lhs, scope);
        if (returnType(lhs, scope) != expectedOperandType(opr)) throw OperandTypeErrorException(e, expectedOperandType(opr), opr)
      case LiteralExpr(_) => {}
      case FunCallExpr(_, el) => {} // TODO type of every expression must be compatible with target type from function and also expression must not have type errors
      case StoreExpr(i, init) => {
        scope.find(s => s.typedIdent.i == i) match {
          case None => throw UndefinedVariableException(i);
          case Some(s) => {
            if (init) {
              // has already been initialized
              if (s.initialzed) throw DuplicateInitialize(i);
              s.initialzed = true;
            } else {
              // access before init
              if (!s.initialzed) throw StoreNotInitialized(i);

              // attempt of const modification (note that during initialization const modification is possible)
              if (lvalue && s.change.forall(c => c == Const)) throw ConstModification(i);
            }
          }
        }
      }
    }
  }

  /**
   * Checks a composition of commands (main or method)
   */
  def checkCpsCmd(cpsCmd: CpsCmd, scope: MutableList[Store]) = {
    cpsCmd.cl.foreach(cmd => checkCmd(cmd, scope))
  }
  def checkCmd(cmd: Cmd, scope: MutableList[Store]) {
    cmd match {
      case BecomesCmd(lhs, rhs) => {
        lhs match {
          case StoreExpr(expr, isInit) => checkExpr(lhs, scope, true);
          case _ => throw InvalidLValue(lhs);
        }
        checkExpr(rhs, scope);

        // TODO lhs must not be declared const!
        // If checkCmd was called from WhileCmd init is NOT ALLOWED! (flag)
        if (returnType(lhs, scope) != returnType(rhs, scope)) throw TypeMismatchError(cmd, returnType(lhs, scope), returnType(rhs, scope))
      }
      case SkipCmd() => {}
      case CallCmd(i, t) => {
        globalMethodScope.decls.get(i) match {
          case None => throw UndefinedMethodException(i)
          case Some(decl) => {
            checkParameters(decl, t, scope);
          }
        }

        // TODO also check if all used variables have been initialized!
      }
      case IfCmd(e, ifcmd, elsecmd) => {
        // check if expression returns a bool
        if (returnType(e, scope) != BoolType) throw TypeMismatchError(cmd, BoolType, returnType(e, scope));
        checkExpr(e, scope);
        checkCpsCmd(ifcmd, scope);
        checkCpsCmd(elsecmd, scope)

        // TODO e must be initialized!
      }
      case WhileCmd(e, cmd) => {
        // check if expression returns a bool
        if (returnType(e, scope) != BoolType) throw TypeMismatchError(cmd, BoolType, returnType(e, scope)); checkExpr(e, scope); checkCpsCmd(cmd, scope);

        // TODO e must be initialized!
      }
      case InputCmd(e) => {
        e match {
          case StoreExpr(i, init) => checkExpr(e, scope, true)
          case _ => throw InvalidLValue(e)
        }
      }
      case OutputCmd(e) => {
        checkExpr(e, scope);
      }

    }
  }

  def checkParameters(decl: Decl, tupleExpr: TupleExpr, scope: MutableList[Store]) {
    decl match {
      case StoreDecl(_, i) => throw new CompilerException(i.i + " is not a proc or fun");
      case FunDecl(identifier, params, _, _, imports, cpsDecl, cmd) => checkMethodParameters(tupleExpr, params, scope)
      case ProcDecl(identifier, params, imports, cpsDecl, cmd) => checkMethodParameters(tupleExpr, params, scope)
    }
  }

  def checkMethodParameters(tupleExpr: TupleExpr, params: ParamList, scope: MutableList[Store]) {
    if (tupleExpr.l.size != params.p.size) throw InvalidParamaterAmount(tupleExpr, params.p.size, tupleExpr.l.size);

    val o = tupleExpr.l.zip(params.p);
    o.foreach(element => {

      // check if the given expression is valid
      checkExpr(element._1, scope)

      // TODO default values??
      val flowMode = element._2.f.getOrElse(() => In);
      val changeMode = element._2.c.getOrElse(() => In);

      // check flow mode (IML36)
      //      flowMode match {
      //        case In => {
      //        }
      //      }

      // check type of argument
      val exprReturnType = returnType(element._1, scope);
      if (exprReturnType != element._2.ti.t) throw InvalidParamater(element._1, element._2.ti.t, exprReturnType)

    })
  }

  def loadGlobalCpsDecl(cpsDecl: Option[CpsDecl]) = cpsDecl.foreach(x => x.declList.foreach(decl => loadGlobalDecl(decl)))
  def loadGlobalDecl(decl: Decl) {
    decl match {
      case StoreDecl(c, i) => globalStoreScope.scope += Store(i, None, Some(c), None, false);
      case FunDecl(identifier, params, _, _, imports, cpsDecl, cmd) => loadMethodDecl(identifier, decl, cpsDecl, params, cmd);
      case ProcDecl(identifier, params, imports, cpsDecl, cmd) => loadMethodDecl(identifier, decl, cpsDecl, params, cmd);

    }
  }

  def checkMethods(cpsDecl: Option[CpsDecl]) = cpsDecl.foreach(x => x.declList.foreach(decl => checkMethod(decl)))
  def checkMethod(decl: Decl) {
    decl match {
      case FunDecl(identifier, params, _, _, imports, cpsDecl, cmd) => checkCpsCmd(cmd, localStoreScope.scope.getOrElse(identifier, new MutableList()));
      case ProcDecl(identifier, params, imports, cpsDecl, cmd) => checkCpsCmd(cmd, localStoreScope.scope.getOrElse(identifier, new MutableList()));
      case StoreDecl(_, init) =>

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

      // IN parameters are initialized by default (so no further initialization is allowed) (IML26)
      val initialized = param.f.forall(f => f == In || f == InOut)
      val paramStore = Store(param.ti, param.m, param.c, param.f, initialized);
      localStoreScope.scope.get(ident) match {
        case None => localStoreScope.scope.put(ident, new MutableList[Store]() += paramStore)
        case Some(x) => if (x.find(ti => ti.typedIdent == param.ti.i).isEmpty) x += paramStore else throw new DuplicateIdentException(param.ti.i)
      }

      // TODO check for invalid combinations!
    })
  }

  def loadLocalCpsDecl(ident: Ident, cpsDecl: Option[CpsDecl]) = {
    cpsDecl.foreach(x => x.declList.foreach(decl => loadLocalDecl(ident, decl)))
  }

  def loadLocalDecl(ident: Ident, decl: Decl) {
    decl match {
      case StoreDecl(c, i) => {
        val localStore = Store(i, None, Some(c), None, false);
        localStoreScope.scope.get(ident) match {
          case None => localStoreScope.scope.put(ident, new MutableList[Store] += localStore)
          case Some(scopeList) => if (!scopeList.contains(i)) scopeList += localStore else throw new DuplicateIdentException(i.i)
        }
      }
      // can not declare anything else inside method declarations
      case _ => throw InvalidDeclException(decl);
    }
  }
}