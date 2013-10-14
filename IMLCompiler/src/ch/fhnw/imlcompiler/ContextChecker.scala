package ch.fhnw.imlcompiler

import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList
import ch.fhnw.imlcompiler.AST._

// TODO create a CallRoot class (Cmd, IfCmd, WhileCmd, ???) instead of the boolean
// TODO do not use exceptions but rather a CheckResult which is either a CheckError or a IMLContext (which is then used by the code generator)
trait ContextCheckers {

  case class DuplicateIdentException(ident: Ident) extends CompilerException("(" + ident.pos.line + ":" + ident.pos.column + ") '" + ident.value + "' is already defined\n\n" + ident.pos.longString + "\nAST: " + ident)
  case class InvalidDeclException(decl: Decl) extends CompilerException("(" + decl.pos.line + ":" + decl.pos.column + ") " + decl.pos.longString + "invalid decleration of " + decl + "\n\nAST: " + decl)
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
  case class InStoreInitialization(n: Ident, f: FlowMode) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") invalid attempt of initializing " + f.toString().toLowerCase() + " mode store: " + n.value + "\n\n" + n.pos.longString + "\nAST: " + n);
  case class LoopedInit(n: Ident) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") invalid attempt of initializing store: " + n.value + " inside a loop body\n\n" + n.pos.longString + "\nAST: " + n);

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
    loadProgParams(prog.params);

    println("Scope loaded: ")
    println(globalStoreScope)
    println(localStoreScope)
    println(globalMethodScope)
    println()

    // check all defined methods for possible errors (type, flow, initialized, etc.)
    checkMethods(prog.cpsDecl);

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
              case FunDecl(_, _, r, _, _, _) => r.ti.t
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

  // TODO branched initialization (what if store was initialized only in if block or if and else both?)
  def checkExpr(e: Expr, scope: MutableList[Store], lvalue: Boolean = false, loopedExpr: Boolean = false) {
    e match {
      case DyadicExpr(lhs, opr, rhs) =>
        checkExpr(lhs, scope);
        checkExpr(rhs, scope);
        if (returnType(lhs, scope) != expectedOperandType(opr)) throw TypeMismatchError(e, expectedOperandType(opr), returnType(lhs, scope))
        if (returnType(rhs, scope) != expectedOperandType(opr)) throw TypeMismatchError(e, expectedOperandType(opr), returnType(rhs, scope))
      case MonadicExpr(lhs, opr) =>
        checkExpr(lhs, scope);
        if (returnType(lhs, scope) != expectedOperandType(opr)) throw TypeMismatchError(e, expectedOperandType(opr), returnType(opr))
      case LiteralExpr(_) => {}
      case FunCallExpr(_, el) => {} // TODO type of every expression must be compatible with target type from function and also expression must not have type errors
      case StoreExpr(i, init) => {
        scope.find(s => s.typedIdent.i == i) match {
          case None => throw UndefinedVariableException(i);
          case Some(s) => {
            if (init) {
              // can not initialize a store inside of a loop
              if (loopedExpr) {
                throw LoopedInit(i)
              }

              // attempt of initializing in/inout store
              s.flow.foreach(f => if (f == In || f == InOut) throw InStoreInitialization(i, f))

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
  def checkCpsCmd(cpsCmd: List[Cmd], scope: MutableList[Store], loopedCmd: Boolean = false) = {
    cpsCmd.foreach(cmd => checkCmd(cmd, scope, loopedCmd))
  }
  def checkCmd(cmd: Cmd, scope: MutableList[Store], loopedCmd: Boolean = false) {
    cmd match {
      case BecomesCmd(lhs, rhs) => {
        lhs match {
          case StoreExpr(expr, isInit) => checkExpr(lhs, scope, true, loopedCmd);
          case _ => throw InvalidLValue(lhs);
        }
        checkExpr(rhs, scope, loopedExpr = loopedCmd);

        // TODO lhs must not be declared const!
        if (returnType(lhs, scope) != returnType(rhs, scope)) throw TypeMismatchError(cmd, returnType(lhs, scope), returnType(rhs, scope))
      }
      case SkipCmd() => {}
      case CallCmd(i, t) => {
        globalMethodScope.decls.get(i) match {
          case None => throw UndefinedMethodException(i)
          case Some(decl) => {
            checkParameters(decl, t, scope, loopedCmd);
          }
        }
      }
      case IfCmd(e, ifcmd, elsecmd) => {
        // check if expression returns a bool
        if (returnType(e, scope) != BoolType) throw TypeMismatchError(cmd, BoolType, returnType(e, scope));
        checkExpr(e, scope, loopedExpr = loopedCmd);
        checkCpsCmd(ifcmd, scope, loopedCmd);
        checkCpsCmd(elsecmd, scope, loopedCmd)
      }
      case WhileCmd(e, cmd) => {
        // check if expression returns a bool
        if (returnType(e, scope) != BoolType) throw TypeMismatchError(e, BoolType, returnType(e, scope));
        checkExpr(e, scope, loopedExpr = true);
        checkCpsCmd(cmd, scope, loopedCmd = true);
      }
      case InputCmd(e) => {
        e match {
          case StoreExpr(i, init) => checkExpr(e, scope, true, loopedExpr = loopedCmd)
          case _ => throw InvalidLValue(e)
        }
      }
      case OutputCmd(e) => {
        checkExpr(e, scope, loopedExpr = loopedCmd);
      }

    }
  }

  def checkParameters(decl: Decl, tupleExpr: TupleExpr, scope: MutableList[Store], looped: Boolean = false) {
    decl match {
      case StoreDecl(_, i) => throw new CompilerException(i.i + " is not a proc or fun");
      case FunDecl(identifier, params, r, imports, cpsDecl, cmd) => checkMethodParameters(tupleExpr, params, scope, looped)
      case ProcDecl(identifier, params, imports, cpsDecl, cmd) => checkMethodParameters(tupleExpr, params, scope, looped)
    }
  }

  def checkMethodParameters(tupleExpr: TupleExpr, params: List[Parameter], scope: MutableList[Store], looped: Boolean = false) {
    if (tupleExpr.l.size != params.size) throw InvalidParamaterAmount(tupleExpr, params.size, tupleExpr.l.size);

    val o = tupleExpr.l.zip(params);
    o.foreach(element => {

      // check if the given expression is valid
      checkExpr(element._1, scope, loopedExpr = looped)

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

  def loadGlobalCpsDecl(cpsDecl: List[Decl]) = cpsDecl.foreach(decl => loadGlobalDecl(decl))
  def loadGlobalDecl(decl: Decl) {
    decl match {
      case StoreDecl(c, ti) => {
        // multiple vars with same name
        if (globalStoreScope.scope.find(s => s.typedIdent.i == ti.i).isDefined) throw DuplicateIdentException(ti.i)

        // init = false => always has to be initialized
        globalStoreScope.scope += Store(ti, None, Some(c), None, false);
      }
      case FunDecl(identifier, params, ret, imports, cpsDecl, cmd) => loadMethodDecl(identifier, decl, cpsDecl, params, cmd);
      case ProcDecl(identifier, params, imports, cpsDecl, cmd) => loadMethodDecl(identifier, decl, cpsDecl, params, cmd);

    }
  }

  def checkMethods(cpsDecl: List[Decl]) = cpsDecl.foreach(decl => checkMethod(decl))
  def checkMethod(decl: Decl) {
    decl match {
      case FunDecl(identifier, params, ret, imports, cpsDecl, cmd) => checkCpsCmd(cmd, localStoreScope.scope.getOrElse(identifier, new MutableList()));
      case ProcDecl(identifier, params, imports, cpsDecl, cmd) => checkCpsCmd(cmd, localStoreScope.scope.getOrElse(identifier, new MutableList()));
      case StoreDecl(_, init) =>

    }
  }

  def loadMethodDecl(identifier: Ident, methodDecl: Decl, cpsDecl: List[Decl], paramList: List[Parameter], cpsCmd: List[Cmd]) {
    // multiple methods with same name
    if (globalMethodScope.decls.contains(identifier)) throw new DuplicateIdentException(identifier)

    globalMethodScope.decls += (identifier -> methodDecl)

    val localScope = localStoreScope.scope.getOrElseUpdate(identifier, new MutableList[Store]());

    loadLocalParams(paramList, localScope)
    loadLocalCpsDecl(identifier, cpsDecl, localScope)
  }

  // TODO almost the same as loadLocalParamDecl
  def loadProgParams(params: List[ProgParameter]) {
    params.foreach(param => {
      // IN parameters are initialized by default (so no further initialization is allowed), OUT parameters need to be initialized (IML36)
      val initialized = param.f.forall(f => f == In || f == InOut)
      val paramStore = Store(param.ti, None, param.c, param.f, initialized);
      if (globalStoreScope.scope.find(s => s.typedIdent.i == param.ti.i).isEmpty) globalStoreScope.scope += paramStore; else throw new DuplicateIdentException(param.ti.i);
    })
  }

  def loadLocalParams(params: List[Parameter], scope: MutableList[Store]) {
    params.foreach(param => {

      // IN parameters are initialized by default (so no further initialization is allowed), OUT parameters need to be initialized  (IML36)
      val initialized = param.f.forall(f => f == In || f == InOut)
      val paramStore = Store(param.ti, param.m, param.c, param.f, initialized);

      if (scope.find(s => s.typedIdent.i == param.ti.i).isEmpty) scope += paramStore; else throw new DuplicateIdentException(param.ti.i);

      // TODO check for invalid combinations!
    })
  }

  def loadLocalCpsDecl(ident: Ident, cpsDecl: List[Decl], scope: MutableList[Store]) = {
    cpsDecl.foreach(decl => loadLocalDecl(ident, decl, scope))
  }

  def loadLocalDecl(ident: Ident, decl: Decl, scope: MutableList[Store]) {
    decl match {
      case StoreDecl(c, ti) => {
        val localStore = Store(ti, None, Some(c), None, false);
        if (scope.find(s => s.typedIdent.i == ti.i).isEmpty) scope += localStore
        else throw new DuplicateIdentException(ti.i)
      }
      // can not declare anything else inside method declarations
      case _ => throw InvalidDeclException(decl);
    }
  }
}