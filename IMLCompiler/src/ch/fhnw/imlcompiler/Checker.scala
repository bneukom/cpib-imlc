package ch.fhnw.imlcompiler

import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList
import ch.fhnw.imlcompiler.AST._

// TODO create a CallRoot class (Cmd, IfCmd, WhileCmd, ???) instead of the boolean
// TODO do not use exceptions but rather a CheckResult which is either a CheckError or a IMLContext (which is then used by the code generator)
trait Checkers {

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
  case class NoGlobalStoreFound(n: Ident) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") no global store found for imported store: " + n.value + "\n\n" + n.pos.longString + "\nAST: " + n);

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
    loadProgParams(prog.params);
    loadGlobalCpsDecl(prog.cpsDecl);

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

  def operandType(o: Opr): Type = {
    o match {
      case PlusOpr | MinusOpr => IntType
      case DivOpr | TimesOpr | ModOpr => IntType
      case EQ | NE | GT | LT | GE | LE => IntType
      case Cand | Cor | Not => BoolType
    }
  }

  def returnType(l: Literal): Type = {
    l match {
      case IntLiteral(_) => IntType
      case BoolLiteral(_) => BoolType
      case list: ListLiteral => listType(list)._1
    }
  }

  def listType(list: ListLiteral, depth: Int = 0): (ListType, Int) = {

    val children = MutableList[Tuple2[ListType, Int]]();

    for (e <- list.l) {

      children += (e match {
        case s: ListLiteral => {
          val l = listType(s, depth + 1)

          (ListType(l._1), l._2);
        }
        case IntLiteral(_) => (ListType(IntType), depth)
        case BoolLiteral(_) => (ListType(BoolType), depth)
      })
    }

    val sorted = children.sortWith((a, b) => {
      // if level is the same real types are higher (Int and Bool) than Any
      if (a._2 == b._2) deepType(a._1) != Any && deepType(b._1) == Any
      else a._2 > b._2
    });

    if (!sorted.isEmpty) sorted.head else (ListType(Any), depth);
  }

  def deepType(l: Type): Type = {
    l match {
      case ListType(i) => return deepType(i)
      case _ => l
    }
  }

  def depth(list: ListLiteral, d: Int, maxDepth: Int): Int = {
    for (e <- list.l) {
      e match {
        case s: ListLiteral => return depth(s, d + 1, if (d + 1 > maxDepth) d + 1 else maxDepth)
        case _ =>
      }
    }

    return maxDepth + 1;
  }

  def dyadicReturnType(lhs: Expr, o: DyadicOpr, rhs: Expr, scope: MutableList[Store]) = {
    o match {
      case PlusOpr | MinusOpr => IntType
      case DivOpr | TimesOpr | ModOpr => IntType
      case EQ | NE | GT | LT | GE | LE => BoolType
      case Cand | Cor => BoolType
      case ConcatOpr => ListType(returnType(lhs, scope))
    }
  }

  def monadicReturnType(o: MonadicOpr, rhs: Expr, scope: MutableList[Store]) = {
    o match {
      case PlusOpr | MinusOpr => IntType
      case Not => BoolType
      case HeadOpr => {
        val rhsReturn = returnType(rhs, scope);
        rhsReturn match {
          case ListType(t) => t
          case _ => throw new CompilerException("what?"); // TODO
        }
      }
      case TailOpr => returnType(rhs, scope);
      case SizeOpr => IntType
    }
  }

  def returnType(e: Expr, scope: MutableList[Store]): Type = {
    e match {
      case DyadicExpr(lhs, opr, rhs) => dyadicReturnType(lhs, opr, rhs, scope)
      case MonadicExpr(rhs, opr) => monadicReturnType(opr, rhs, scope)
      case LiteralExpr(l) => returnType(l)
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

  // TODO branched initialization (what if store was initialized only in if block or if and else both?)
  def checkExpr(e: Expr, scope: MutableList[Store], lvalue: Boolean = false, loopedExpr: Boolean = false) {
    // TODO pass if is lvalue and looped expr to ckeckExpr sub calls?
    e match {
      case DyadicExpr(lhs, opr: DyadicListOpr, rhs) => {
        checkExpr(rhs, scope);

        val r = returnType(rhs, scope)
        val l = returnType(lhs, scope);

        r match {
          case ListType(t) => if (!t.matches(l)) throw TypeMismatchError(e, t, l)
          case _ => throw TypeMismatchError(e, ListType(l), r)
        }
      }
      case MonadicExpr(lhs, opr: MonadicListOpr) => {}
      case DyadicExpr(lhs, opr, rhs) =>
        checkExpr(lhs, scope);
        checkExpr(rhs, scope);
        if (returnType(lhs, scope) != operandType(opr)) throw TypeMismatchError(e, operandType(opr), returnType(lhs, scope))
        if (returnType(rhs, scope) != operandType(opr)) throw TypeMismatchError(e, operandType(opr), returnType(rhs, scope))
      case MonadicExpr(lhs, opr) =>
        checkExpr(lhs, scope);
        if (returnType(lhs, scope) != operandType(opr)) throw TypeMismatchError(e, operandType(opr), returnType(lhs, scope))
      case LiteralExpr(e) => {
        e match {
          case l: ListLiteral => checkListLiteral(l)
          case _ =>
        }
      }
      case FunCallExpr(_, el) =>
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

  def checkCpsCmd(cpsCmd: List[Cmd], scope: MutableList[Store], loopedCmd: Boolean = false) = {
    cpsCmd.foreach(cmd => checkCmd(cmd, scope, loopedCmd))
  }
  def checkCmd(cmd: Cmd, scope: MutableList[Store], loopedCmd: Boolean = false) {
    cmd match {
      case BecomesCmd(lhs, rhs) => {
        // rhs must be checked before lhs due to the possibility of uninitialized stores
        checkExpr(rhs, scope, loopedExpr = loopedCmd);

        lhs match {
          case StoreExpr(expr, isInit) => {
            // TODO check for const
            checkExpr(lhs, scope, true, loopedCmd);
          }
          case _ => throw InvalidLValue(lhs);
        }

        val typeRhs = returnType(rhs, scope);
        val typeLhs = returnType(lhs, scope);
        if (!typeLhs.matches(typeRhs)) {
          throw TypeMismatchError(cmd, typeLhs, typeRhs)
        }
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

  def checkListLiteral(listLiteral: ListLiteral) {
    listLiteral.l.foreach(l => {
      l match {
        case sub: ListLiteral => checkListLiteral(sub)
        case _ =>
      }

      val ret = returnType(l)
      listLiteral.l.foreach(l2 => {
        val ret2 = returnType(l2)
        if (!ret.matches(ret2)) throw TypeMismatchError(l, ret, ret2);
      })
    })
  }

  def checkParameters(decl: Decl, tupleExpr: TupleExpr, scope: MutableList[Store], looped: Boolean = false) {
    decl match {
      case StoreDecl(_, i) => throw new CompilerException("internal compiler error");
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

  def loadGlobalCpsDecl(cpsDecl: List[Decl]) = {
    // load store decls before methods
    cpsDecl.foreach(decl => {
      decl match {
        case StoreDecl(c, ti) => {
          // multiple vars with same name
          if (globalStoreScope.scope.find(s => s.typedIdent.i == ti.i).isDefined) throw DuplicateIdentException(ti.i)

          // init = false => always has to be initialized
          globalStoreScope.scope += Store(ti, None, Some(c), None, false);
        }
        case _ =>
      }
    });

    // load fun/proc decls
    cpsDecl.foreach(decl => {
      decl match {
        case FunDecl(identifier, params, ret, imports, cpsDecl, cmd) => loadMethodDecl(identifier, decl, cpsDecl, params, imports, cmd);
        case ProcDecl(identifier, params, imports, cpsDecl, cmd) => loadMethodDecl(identifier, decl, cpsDecl, params, imports, cmd);
        case _ =>
      }
    });
  }

  def checkMethods(cpsDecl: List[Decl]) = cpsDecl.foreach(decl => checkMethod(decl))
  def checkMethod(decl: Decl) {
    decl match {
      case FunDecl(identifier, params, ret, imports, cpsDecl, cmd) => checkCpsCmd(cmd, localStoreScope.scope.getOrElse(identifier, new MutableList()));
      case ProcDecl(identifier, params, imports, cpsDecl, cmd) => checkCpsCmd(cmd, localStoreScope.scope.getOrElse(identifier, new MutableList()));
      case StoreDecl(_, init) =>

    }
  }

  def loadMethodDecl(identifier: Ident, methodDecl: Decl, cpsDecl: List[Decl], paramList: List[Parameter], imports: List[GlobImport], cpsCmd: List[Cmd]) {
    // multiple methods with same name
    if (globalMethodScope.decls.contains(identifier)) throw new DuplicateIdentException(identifier)

    globalMethodScope.decls += (identifier -> methodDecl)

    val localScope = localStoreScope.scope.getOrElseUpdate(identifier, new MutableList[Store]());

    loadMethodGlobals(imports, localScope);
    loadLocalParams(paramList, localScope)
    loadLocalCpsDecl(identifier, cpsDecl, localScope)
  }

  def loadMethodGlobals(imports: List[GlobImport], scope: MutableList[Store]) {
    imports.foreach(imp => {
      if (scope.find(x => x.typedIdent.i == imp.i).isDefined) throw DuplicateIdentException(imp.i);

      globalStoreScope.scope.find(x => x.typedIdent.i == imp.i) match {
        case None => throw NoGlobalStoreFound(imp.i);
        case Some(global) => {
          // get the type from the global store
          val typedIdent = TypedIdent(imp.i, global.typedIdent.t);
          typedIdent.pos = imp.i.pos;

          // in/out stores are initialized by default
          val initialized = imp.f.forall(f => f == In || f == InOut)
          scope += Store(typedIdent, None, imp.c, imp.f, initialized);
        }
      }

    })
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