package ch.fhnw.imlcompiler

import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList
import ch.fhnw.imlcompiler.AST._
import ch.fhnw.imlcompiler.checking.ProgramContext._

// TODO create a CallRoot class (Cmd, IfCmd, WhileCmd, ???) instead of the boolean
// TODO do not use exceptions but rather a CheckResult which is either a CheckError or a IMLContext (which is then used by the code generator)
trait SemanticAnalysis {

  case class DuplicateIdentException(ident: Ident) extends CompilerException("(" + ident.pos.line + ":" + ident.pos.column + ") '" + ident.value + "' is already defined\n\n" + ident.pos.longString + "\nAST: " + ident)
  case class InvalidDeclException(decl: Decl) extends CompilerException("(" + decl.pos.line + ":" + decl.pos.column + ") invalid decleration of " + decl.getClass().getSimpleName + "\n\n" + decl.pos.longString + "\nAST: " + decl)
  case class TypeMismatchError(n: ASTNode, required: Type, found: Type) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") type mismatch; found: " + found + " required: " + required + "\n\n" + n.pos.longString + "\nAST: " + n)
  case class UndefinedStoreException(n: Ident) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") undefined store '" + n.value + "' used\n\n" + n.pos.longString + "\nAST: " + n);
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


  private val globalStoreScope: GlobalStoreScope = GlobalStoreScope(new MutableList());
  private val localStoreScope: LocalStoreScopes = LocalStoreScopes(new HashMap());
  private val globalMethodScope: GlobalMethodScope = GlobalMethodScope(new HashMap());

  def checkSemantics(prog: Program): Context = {
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

  def checkDyadicOpr(lhs: Expr, o: DyadicOpr, rhs: Expr, scope: MutableList[Store]) = {
    val lhsType = returnType(lhs, scope);
    val rhsType = returnType(rhs, scope)
    o match {
      case ConcatOpr => {
        rhsType match {
          case ListType(t) => if (!t.matches(lhsType)) throw TypeMismatchError(lhs, t, lhsType)
          case _ => throw TypeMismatchError(rhs, ListType(lhsType), rhsType)
        }
      }
      case t => {
        val expectedType = t match {
          case PlusOpr | MinusOpr | DivOpr | TimesOpr | ModOpr | EQ | NE | GT | LT | GE | LE => IntType
          case Cand | Cor => BoolType
          case _ => throw new CompilerException("internal compiler error");
        }

        if (!returnType(lhs, scope).matches(expectedType)) throw TypeMismatchError(lhs, expectedType, lhsType)
        if (!returnType(rhs, scope).matches(expectedType)) throw TypeMismatchError(rhs, expectedType, rhsType)
      }
    }
  }

  def checkMonadicOpr(rhs: Expr, o: MonadicOpr, scope: MutableList[Store]) {
    val rhsType = returnType(rhs, scope);

    o match {
      case MinusOpr | PlusOpr => {
        if (!returnType(rhs, scope).matches(IntType)) throw TypeMismatchError(rhs, IntType, rhsType)
      }
      case HeadOpr | TailOpr | SizeOpr => {
        // TODO minor error: any not always correct here (any "only" matches int|bool but here a list would also be possible)
        rhsType match { case ListType(x) => {} case invalid => throw TypeMismatchError(rhs, ListType(Any), rhsType) }
      }
      case Not => {
        if (!returnType(rhs, scope).matches(BoolType)) throw TypeMismatchError(rhs, BoolType, rhsType)
      }
    }
  }

  def returnType(l: Literal, scope: MutableList[Store]): Type = {
    l match {
      case IntLiteral(_) => IntType
      case BoolLiteral(_) => BoolType
      case list: ListLiteral => listType(list, scope)._1
    }
  }

  def listType(list: ListLiteral, scope: MutableList[Store], depth: Int = 0): (Type, Int) = {

    val children = MutableList[Tuple2[Type, Int]]();

    for (e <- list.l) {

      val retType = returnType(e, scope)
      children += (
        e match {
          case LiteralExpr(lit) => {
            lit match {
              case s: ListLiteral => {
                val l = listType(s, scope, depth + 1)

                (ListType(l._1), l._2);
              }
              case IntLiteral(_) => (ListType(IntType), depth)
              case BoolLiteral(_) => (ListType(BoolType), depth)
            }
          }
          case e => (ListType(returnType(e, scope)), depth)

        })
    }

    val sorted = children.sortWith((a, b) => {
      // if level is the same real types (Int and Bool) are before Any
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
          case _ => throw new CompilerException("what?"); // TODO ??? 
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
      case LiteralExpr(l) => returnType(l, scope)
      case StoreExpr(i, _) => {
        scope.find(x => x.typedIdent.i == i) match {
          case None => throw UndefinedStoreException(i);
          case Some(store) => store.typedIdent.t;
        }
      }
      case ListExpr(_, _, _, _, _) => ListType(IntType)
      case FunCallExpr(i, _) => {
        globalMethodScope.decls.find(x => x._1 == i) match {
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
      case DyadicExpr(lhs, opr, rhs) =>
        checkExpr(lhs, scope);
        checkExpr(rhs, scope);

        checkDyadicOpr(lhs, opr, rhs, scope);
      case MonadicExpr(rhs, opr) =>
        checkExpr(rhs, scope);

        checkMonadicOpr(rhs, opr, scope);
      case LiteralExpr(e) => {
        e match {
          case l: ListLiteral => checkListLiteral(l, scope, lvalue, loopedExpr)
          case _ =>
        }
      }
      case ListExpr(ret, i, from, to, where) => {
        // anonymous ident has to be unique
        if (scope.find(s => s.typedIdent.i == i).isDefined) throw DuplicateIdentException(i)

        // check from and where expressions (they should not be able to access the anonymous store)
        val fromType = returnType(from, scope); if (!fromType.matches(IntType)) throw TypeMismatchError(from, IntType, fromType);
        val toType = returnType(to, scope); if (!toType.matches(IntType)) throw TypeMismatchError(to, IntType, toType);

        // add the anonymous identifier to the current scope for this test
        val anonymousStore = Store(TypedIdent(i, IntType), Some(Copy), Some(Var), None, true);
        scope += anonymousStore;

        // check the return type
        checkExpr(ret, scope, lvalue, loopedExpr)
        val retType = returnType(ret, scope); if (!retType.matches(IntType)) throw TypeMismatchError(ret, IntType, retType);

        checkExpr(where, scope, lvalue, loopedExpr)
        val whereType = returnType(where, scope); if (!whereType.matches(BoolType)) throw TypeMismatchError(where, BoolType, whereType);
        
        // TODO remove anonymousStore from scope
        
      }
      case FunCallExpr(ident, tupleExpr) => {
        globalMethodScope.decls.get(ident) match {
          case None => throw UndefinedMethodException(ident)
          case Some(decl) => {
            checkParameters(decl, tupleExpr, scope, loopedExpr);
          }
        }
      }
      case StoreExpr(i, init) => {
        // TODO initalized test here or in FlowAnalysis.scala?
        scope.find(s => s.typedIdent.i == i) match {
          case None => throw UndefinedStoreException(i);
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

  // TODO this method is no more needed
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

  def checkListLiteral(listLiteral: ListLiteral, scope: MutableList[Store], lvalue: Boolean = false, loopedExpr: Boolean = false) {
    // first check if all expressions are valid
    listLiteral.l.foreach(e => {
      checkExpr(e, scope, lvalue, loopedExpr)
    })

    // check if types of (e cross e) matches
    listLiteral.l.foreach(e => {
      val ret = returnType(e, scope)
      listLiteral.l.foreach(e2 => {
        val ret2 = returnType(e2, scope)
        if (!ret.matches(ret2)) {
          throw TypeMismatchError(e2, ret, ret2);
        }
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
        case FunDecl(identifier, params, ret, imports, cpsDecl, cmd) => {
          loadMethodDecl(identifier, decl, cpsDecl, params, imports, cmd);

          // also load return value into local scope (IML40)
          loadLocalDecl(ret.ti.i, ret, localStoreScope.scope.getOrElseUpdate(identifier, new MutableList[Store]()))
        }
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

  // TODO this method is no more needed
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