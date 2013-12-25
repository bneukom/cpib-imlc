package ch.fhnw.imlcompiler

import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList
import scala.collection.mutable.LinkedList
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.DoubleLinkedList
import ch.fhnw.imlcompiler.AST._
import scala.collection.mutable.HashSet
import scala.annotation.tailrec

// TODO debugout l init should not be possible!
trait ContextChecker {

  // TODO do not use exceptions but rather a CheckResult which is either a CheckError or a IMLContext (which is then used by the code generator)
  case class DuplicateIdentException(ident: Ident) extends CompilerException("(" + ident.pos.line + ":" + ident.pos.column + ") '" + ident.value + "' is already defined\n\n" + ident.pos.longString + "\nAST: " + ident)
  case class InvalidDeclException(decl: Decl) extends CompilerException("(" + decl.pos.line + ":" + decl.pos.column + ") invalid decleration of " + decl.getClass().getSimpleName + "\n\n" + decl.pos.longString + "\nAST: " + decl)
  case class TypeMismatchError(n: ASTNode, required: Type, found: Type) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") type mismatch; found: " + found + " required: " + required + "\n\n" + n.pos.longString + "\nAST: " + n)
  case class UndefinedStoreException(n: Ident) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") undefined store '" + n.value + "' used\n\n" + n.pos.longString + "\nAST: " + n);
  case class UndefinedMethodException(n: Ident) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") undefined method '" + n.value + "' used\n\n" + n.pos.longString + "\nAST: " + n);
  case class ProcReturnException(n: Ident) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") proc method '" + n.value + "' do not have return types\n\n" + n.pos.longString + "\nAST: " + n);
  case class BranchNotAllInitialized(c: IfCmd, s: Store) extends CompilerException("(" + c.pos.line + ":" + c.pos.column + ") the store " + s.typedIdent.i.value + " might not have been initialized \n\nAST: " + c);
  case class InvalidParamaterAmount(n: TupleExpr, required: Int, actual: Int) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") invalid amount of parameters " + actual + " required: " + required + "\n\n" + n.pos.longString + "\nAST: " + n);
  case class InvalidParamater(n: Expr, required: Type, actual: Type) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") invalid parameter type '" + actual + "' required: " + required + "\n\n" + n.pos.longString + "\nAST: " + n);
  case class DuplicateInitialize(n: Ident) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") store: '" + n.value + "' has already been initialized\n\n" + n.pos.longString + "\nAST: " + n);
  case class StoreNotInitialized(n: Ident) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") store: '" + n.value + "' has not been initialized\n\n" + n.pos.longString + "\nAST: " + n);
  case class InvalidstoreAssignment(n: Expr) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") invalid storeAssignment\n\n" + n.pos.longString + "\nAST: " + n);
  case class ConstModification(n: Ident) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") identifier '" + n.value + "' is const and can not be modified\n\n" + n.pos.longString + "\nAST: " + n);
  case class InStoreInitialization(n: Ident, f: FlowMode) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") invalid attempt of initializing " + f.toString().toLowerCase() + " mode store: " + n.value + "\n\n" + n.pos.longString + "\nAST: " + n);
  case class LoopedInit(n: Ident) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") invalid attempt of initializing store: " + n.value + " inside a loop body\n\n" + n.pos.longString + "\nAST: " + n);
  case class NoGlobalStoreFound(n: Ident) extends CompilerException("(" + n.pos.line + ":" + n.pos.column + ") no global store found for imported store: " + n.value + "\n\n" + n.pos.longString + "\nAST: " + n);

  def contextCheck(prog: Program): SymbolTable = {
    val symbolTable = new SymbolTable;

    // load global declarations first (so we don't need any forward declarations)
    loadProgParams(prog.params)(symbolTable);
    loadGlobalCpsDecl(prog.cpsDecl)(symbolTable);

    // check all defined methods and the main program for possible errors (type, flow, initialized, etc.)
    checkRoutines(prog.cpsDecl)(symbolTable);
    checkCpsCmd(prog.commands, symbolTable.globalScope)(symbolTable);

    return symbolTable;
  }

  def checkDyadicOpr(lhs: Expr, o: DyadicOpr, rhs: Expr, scope: Scope)(implicit symbolTable: SymbolTable) = {
    val lhsType = returnType(lhs, scope);
    val rhsType = returnType(rhs, scope)
    o match {
      case ConsOpr => {
        rhsType match {
          case ListType(t) => if (!t.matches(lhsType)) throw TypeMismatchError(lhs, t, lhsType)
          case _ => throw TypeMismatchError(rhs, ListType(lhsType), rhsType)
        }
      }
      case t => {
        val expectedType = t match {
          case PlusOpr | MinusOpr | DivOpr | TimesOpr | ModOpr | EQ | NE | GT | LT | GE | LE => IntType
          case Cand | Cor => BoolType
          case _ => throw new IllegalStateException
        }

        if (!returnType(lhs, scope).matches(expectedType)) throw TypeMismatchError(lhs, expectedType, lhsType)
        if (!returnType(rhs, scope).matches(expectedType)) throw TypeMismatchError(rhs, expectedType, rhsType)
      }
    }
  }

  def checkMonadicOpr(rhs: Expr, o: MonadicOpr, scope: Scope)(implicit symbolTable: SymbolTable) {
    val rhsType = returnType(rhs, scope);

    o match {
      case MinusOpr | PlusOpr => {
        if (!returnType(rhs, scope).matches(IntType)) throw TypeMismatchError(rhs, IntType, rhsType)
      }
      case HeadOpr | TailOpr | LengthOpr => {
        // TODO minor error: any not always correct here (any "only" matches int|bool but here a list would also be possible)
        rhsType match { case ListType(x) => {} case invalid => throw TypeMismatchError(rhs, ListType(Any), rhsType) }
      }
      case Not => {
        if (!returnType(rhs, scope).matches(BoolType)) throw TypeMismatchError(rhs, BoolType, rhsType)
      }
    }
  }

  def returnType(l: Literal, scope: Scope)(implicit symbolTable: SymbolTable): Type = {
    l match {
      case IntLiteral(_) => IntType
      case BoolLiteral(_) => BoolType
      case list: ListLiteral => listType(list, scope)._1
    }
  }

  def listType(list: ListLiteral, scope: Scope, depth: Int = 0)(implicit symbolTable: SymbolTable): (Type, Int) = {

    val children = ListBuffer[Tuple2[Type, Int]]();

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

  def listLevel(l: Type, level: Int = 0): Int =
    l match {
      case ListType(i) => return listLevel(i, level + 1);
      case _ => level
    }

  def deepType(l: Type): Type = {
    l match {
      case ListType(i) => return deepType(i)
      case _ => l
    }
  }

  def dyadicReturnType(lhs: Expr, o: DyadicOpr, rhs: Expr, scope: Scope)(implicit symbolTable: SymbolTable) = {
    o match {
      case PlusOpr | MinusOpr => IntType
      case DivOpr | TimesOpr | ModOpr => IntType
      case EQ | NE | GT | LT | GE | LE => BoolType
      case Cand | Cor => BoolType
      case ConsOpr => ListType(returnType(lhs, scope))
    }
  }

  def monadicReturnType(o: MonadicOpr, rhs: Expr, scope: Scope)(implicit symbolTable: SymbolTable) = {
    o match {
      case PlusOpr | MinusOpr => IntType
      case Not => BoolType
      case HeadOpr => {
        val rhsReturn = returnType(rhs, scope);
        rhsReturn match {
          case ListType(t) => t
          case _ => throw new IllegalStateException
        }
      }
      case TailOpr => returnType(rhs, scope);
      case LengthOpr => IntType
    }
  }

  def returnType(e: Expr, scope: Scope)(implicit context: SymbolTable): Type = {
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
      //      case ListExpr(o, _, _, _, _) => ListType(returnType(o, scope)) // infer type of list via output expression
      case ListExpr(o, _, _, _, _) => ListType(IntType) // infer type of list via output expression
      case FunCallExpr(i, _) => {
        context.routines.find(x => x._1 == i) match {
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

  def checkExpr(e: Expr, scope: Scope, storeAssignment: Boolean = false, loopedExpr: Boolean = false, elseBranch: Boolean = false, initializedStores: HashSet[Store])(implicit symbolTable: SymbolTable) {
    // TODO pass if is storeAssignment and looped expr to ckeckExpr sub calls?
    e match {
      case DyadicExpr(lhs, opr, rhs) =>
        checkExpr(lhs, scope, storeAssignment, loopedExpr, elseBranch, initializedStores);
        checkExpr(rhs, scope, false, loopedExpr, elseBranch, initializedStores);

        checkDyadicOpr(lhs, opr, rhs, scope);
      case MonadicExpr(rhs, opr) =>
        checkExpr(rhs, scope, false, loopedExpr, elseBranch, initializedStores);

        checkMonadicOpr(rhs, opr, scope);
      case LiteralExpr(e) => {
        e match {
          case l: ListLiteral => checkListLiteral(l, scope, false, loopedExpr, elseBranch, initializedStores)
          case _ =>
        }
      }
      case ListExpr(ret, i, from, to, where) => {
        // anonymous ident has to be unique
        if (scope.find(s => s.typedIdent.i == i).isDefined) throw DuplicateIdentException(i)

        // add the anonymous identifier to the current scope for this test
        val anonymousStore = new Store(TypedIdent(i, IntType), Some(Copy), Some(Var), None);
        val tempScope = Scope(scope.stores :+ anonymousStore)
        val tempInitialized = initializedStores + anonymousStore;

        // check from and where expressions (they should not be able to access the anonymous store)
        val fromType = returnType(from, tempScope); if (!fromType.matches(IntType)) throw TypeMismatchError(from, IntType, fromType);
        val toType = returnType(to, tempScope); if (!toType.matches(IntType)) throw TypeMismatchError(to, IntType, toType);

        // check the return type
        checkExpr(ret, tempScope, false, loopedExpr, elseBranch, tempInitialized)
        val retType = returnType(ret, tempScope); if (!retType.matches(IntType)) throw TypeMismatchError(ret, IntType, retType);

        checkExpr(where, tempScope, false, loopedExpr, elseBranch, tempInitialized)
        val whereType = returnType(where, tempScope); if (!whereType.matches(BoolType)) throw TypeMismatchError(where, BoolType, whereType);
      }
      case FunCallExpr(ident, tupleExpr) => {
        symbolTable.routines.get(ident) match {
          case None => throw UndefinedMethodException(ident)
          case Some(decl) => {
            checkParameters(decl, tupleExpr, scope, loopedExpr, elseBranch, initializedStores);
          }
        }
      }
      case storeExpr: StoreExpr => {
        scope.find(s => s.typedIdent.i == storeExpr.i) match {
          case None => throw UndefinedStoreException(storeExpr.i);
          case Some(s) => {
            if (storeExpr.isInitialization) {

              // can not initialize a store inside of a loop
              if (loopedExpr) throw LoopedInit(storeExpr.i)

              // attempt of initializing in/inout store
              s.flow.foreach(f => if (f == In || f == InOut) throw InStoreInitialization(storeExpr.i, f))

              if (initializedStores.find(s => s.typedIdent.i == storeExpr.i).isDefined) throw DuplicateInitialize(storeExpr.i);
              if (!storeAssignment) throw InvalidstoreAssignment(storeExpr)

              initializedStores += s;
            } else {
              val store = initializedStores.find(s => s.typedIdent.i == storeExpr.i);
              val sc = scope.find(s => s.typedIdent.i == storeExpr.i);

              // const modification
              if (storeAssignment && s.change.forall(c => c == Const)) {
                throw ConstModification(storeExpr.i);
              }

              if (!store.isDefined && !sc.forall(f => f.flow == Some(In) || f.flow == Some(InOut))) throw StoreNotInitialized(storeExpr.i);
            }
          }
        }
      }
    }
  }

  // TODO this method is no more needed
  def checkCpsCmd(cpsCmd: List[Cmd], scope: Scope, loopedCmd: Boolean = false, elseBranch: Boolean = false, initialized: HashSet[Store] = HashSet())(implicit context: SymbolTable) = cpsCmd.foreach(cmd => checkCmd(cmd, scope, loopedCmd, elseBranch, initialized))

  def checkCmd(cmd: Cmd, scope: Scope, loopedCmd: Boolean = false, elseBranch: Boolean = false, initializedStores: HashSet[Store] = HashSet())(implicit context: SymbolTable) {
    cmd match {
      case BecomesCmd(lhs, rhs) => {

        // rhs must be checked before lhs due to the possibility of uninitialized stores
        checkExpr(rhs, scope, false, loopedCmd, elseBranch, initializedStores);

        lhs match {
          case StoreExpr(i, isInit) => {
            checkExpr(lhs, scope, true, loopedCmd, elseBranch, initializedStores);
          }
          case _ => throw InvalidstoreAssignment(lhs);
        }

        val typeRhs = returnType(rhs, scope);
        val typeLhs = returnType(lhs, scope);
        if (!typeLhs.matches(typeRhs)) {
          throw TypeMismatchError(cmd, typeLhs, typeRhs)
        }

      }
      case SkipCmd() => {}
      case CallCmd(i, t) => {
        context.routines.get(i) match {
          case None => throw UndefinedMethodException(i)
          case Some(decl) => {
            checkParameters(decl, t, scope, loopedCmd, elseBranch, initializedStores);
          }
        }
      }
      case ifCmd: IfCmd => {
        if (returnType(ifCmd.expr, scope) != BoolType) throw TypeMismatchError(cmd, BoolType, returnType(ifCmd.expr, scope));
        checkExpr(ifCmd.expr, scope, false, loopedCmd, elseBranch, initializedStores);

        val ifInits = initializedStores.clone;
        val elseInits = initializedStores.clone;

        checkCpsCmd(ifCmd.ifCmd, scope, loopedCmd, false, ifInits);
        checkCpsCmd(ifCmd.elseCmd, scope, loopedCmd, true, elseInits);

        initializedStores ++= (ifInits ++ elseInits);

        val diff = ifInits.diff(elseInits)

        if (diff.size > 0) throw BranchNotAllInitialized(ifCmd, diff.head)
      }
      case WhileCmd(e, cmd) => {
        if (returnType(e, scope) != BoolType) throw TypeMismatchError(e, BoolType, returnType(e, scope));
        checkExpr(e, scope, false, loopedCmd, elseBranch, initializedStores);
        checkCpsCmd(cmd, scope, loopedCmd = true, elseBranch, initializedStores);
      }
      case InputCmd(e) => {
        e match {
          case StoreExpr(i, init) => checkExpr(e, scope, true, loopedCmd, elseBranch, initializedStores)
          case _ => throw InvalidstoreAssignment(e)
        }
      }
      case OutputCmd(e) => {
        checkExpr(e, scope, false, loopedCmd, elseBranch, initializedStores);
      }

    }
  }

  def checkListLiteral(listLiteral: ListLiteral, scope: Scope, storeAssignment: Boolean = false, loopedExpr: Boolean = false, elseBranch: Boolean = false, initializedStores: HashSet[Store])(implicit context: SymbolTable) {
    // first check if all expressions are valid
    listLiteral.l.foreach(e => {
      checkExpr(e, scope, storeAssignment, loopedExpr, elseBranch, initializedStores)
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

  def checkParameters(decl: Decl, tupleExpr: TupleExpr, scope: Scope, looped: Boolean = false, elseBranch: Boolean = false, initialized: HashSet[Store])(implicit context: SymbolTable) {
    decl match {
      case StoreDecl(_, i) => throw new IllegalStateException
      case FunDecl(identifier, params, r, imports, cpsDecl, cmd) => checkMethodParameters(tupleExpr, params, scope, looped, elseBranch, initialized)
      case ProcDecl(identifier, params, imports, cpsDecl, cmd) => checkMethodParameters(tupleExpr, params, scope, looped, elseBranch, initialized)
    }
  }

  def checkMethodParameters(tupleExpr: TupleExpr, params: List[Parameter], scope: Scope, looped: Boolean = false, elseBranch: Boolean = false, initialized: HashSet[Store])(implicit context: SymbolTable) {
    if (tupleExpr.l.size != params.size) throw InvalidParamaterAmount(tupleExpr, params.size, tupleExpr.l.size);

    val o = tupleExpr.l.zip(params);
    o.foreach(element => {

      // check if the given expression is valid
      checkExpr(element._1, scope, false, looped, elseBranch, initialized)

      // TODO default values?? (
      val flowMode = element._2.f.getOrElse(() => In);
      val changeMode = element._2.c.getOrElse(() => Const);

      // TODO 
      // check flow mode (IML36)
      //      flowMode match {
      //        case In => {
      //        }
      //      }

      // check type of argument
      val exprReturnType = returnType(element._1, scope);
      if (!exprReturnType.matches(element._2.ti.t)) throw InvalidParamater(element._1, element._2.ti.t, exprReturnType)
    })
  }

  def loadGlobalCpsDecl(cpsDecl: List[Decl])(implicit context: SymbolTable) = {
    // load store decls before methods
    cpsDecl.foreach(decl => {
      decl match {
        case StoreDecl(c, ti) => {
          // multiple vars with same name
          if (context.globalScope.find(s => s.typedIdent.i == ti.i).isDefined) throw DuplicateIdentException(ti.i)

          context.globalScope += new Store(ti, None, Some(c), None);
        }
        case _ => // ignore
      }
    });

    // load fun/proc decls
    cpsDecl.foreach(decl => {

      decl match {
        case FunDecl(identifier, params, ret, imports, cpsDecl, cmd) => {
          loadRoutineDecl(identifier, decl, cpsDecl, params, imports, cmd);

          // also load return value into local scope (IML40)
          loadLocalDecl(ret.ti.i, ret, context.getLocalStoreScope(identifier))
        }
        case ProcDecl(identifier, params, imports, cpsDecl, cmd) => loadRoutineDecl(identifier, decl, cpsDecl, params, imports, cmd);
        case _ => // ignore
      }
    });
  }

  def checkRoutines(cpsDecl: List[Decl])(implicit context: SymbolTable) = cpsDecl.foreach(decl => checkRoutine(decl))
  def checkRoutine(decl: Decl)(implicit context: SymbolTable) {
    decl match {
      case FunDecl(identifier, params, ret, imports, cpsDecl, cmd) => {

        val initialized = HashSet[Store]();
        checkCpsCmd(cmd, context.getLocalStoreScope(identifier), initialized = initialized);

        if (initialized.find(s => s.typedIdent.i == ret.ti.i).isEmpty) throw StoreNotInitialized(ret.ti.i);
      }
      case ProcDecl(identifier, params, imports, cpsDecl, cmd) => checkCpsCmd(cmd, context.getLocalStoreScope(identifier));
      case StoreDecl(_, init) =>

    }
  }

  def loadRoutineDecl(identifier: Ident, methodDecl: Decl, cpsDecl: List[Decl], paramList: List[Parameter], imports: List[GlobImport], cpsCmd: List[Cmd])(implicit symbolTable: SymbolTable) {
    // multiple methods with same name
    if (symbolTable.routines.contains(identifier)) throw new DuplicateIdentException(identifier)

    symbolTable.routines += (identifier -> methodDecl)
    val localScope = symbolTable.getLocalStoreScope(identifier);

    loadRoutineGlobalImports(imports, localScope);
    loadLocalParams(paramList, localScope)
    loadLocalCpsDecl(identifier, cpsDecl, localScope)
  }

  def loadRoutineGlobalImports(imports: List[GlobImport], scope: Scope)(implicit context: SymbolTable) {
    imports.foreach(imp => {
      if (scope.exists(imp.i)) throw DuplicateIdentException(imp.i);

      context.globalScope.find(x => x.typedIdent.i == imp.i) match {
        case None => throw NoGlobalStoreFound(imp.i);
        case Some(global) => {
          // get the type from the global store
          val typedIdent = global.typedIdent.copy;

          scope += new Store(typedIdent, None, imp.c, imp.f, globImp = true);
        }
      }

    })
  }

  // TODO almost the same as loadLocalParamDecl
  def loadProgParams(params: List[ProgParameter])(implicit context: SymbolTable) {
    params.foreach(param => {
      val paramStore = new Store(param.ti, None, param.c, param.f);
      if (!context.globalScope.exists(param.ti.i)) context.globalScope += paramStore; else throw new DuplicateIdentException(param.ti.i);
    })
  }

  def loadLocalParams(params: List[Parameter], scope: Scope) {
    params.foreach(param => {

      val paramStore = new Store(param.ti, Some(param.m.getOrElse(Copy)), Some(param.c.getOrElse(Const)), Some(param.f.getOrElse(In)));

      if (!scope.exists(param.ti.i)) scope += paramStore; else throw new DuplicateIdentException(param.ti.i);

      // TODO check for invalid combinations!
    })
  }

  // TODO this method is no more needed
  def loadLocalCpsDecl(ident: Ident, cpsDecl: List[Decl], scope: Scope) = {
    cpsDecl.foreach(decl => loadLocalDecl(ident, decl, scope))
  }

  def loadLocalDecl(ident: Ident, decl: Decl, scope: Scope) {
    decl match {
      case StoreDecl(c, ti) => {
        val localStore = new Store(ti, None, Some(c), None);
        if (!scope.exists(ti.i)) scope += localStore
        else throw new DuplicateIdentException(ti.i)
      }
      // can not declare anything else inside method declarations
      case _ => throw InvalidDeclException(decl);
    }
  }

}

class CompilerException(v: String) extends RuntimeException(v) {
  def this(n: ASTNode) = this(n.pos.longString)
}

case class Store(val typedIdent: TypedIdent, val mech: Option[MechMode], val change: Option[ChangeMode], val flow: Option[FlowMode], val globImp: Boolean = false, val synthetic: Boolean = false)

case class Scope(private val _stores: ListBuffer[Store] = ListBuffer()) {
  def get(s: String) = _stores.find(_.typedIdent.i.value == s).get
  def get(i: Ident) = _stores.find(_.typedIdent.i == i).get
  def exists(i: Ident) = _stores.exists(_.typedIdent.i == i)
  def +=(s: Store) = _stores += s;
  def find(p: Store => Boolean) = _stores.find(p)
  def stores() = _stores;
}
case class SymbolTable {
  private val _globalStores = Scope();
  private val _localStores = HashMap[Ident, Scope]();
  private val _routines = HashMap[Ident, Decl]();

  def globalScope() = _globalStores;
  def routines() = _routines;

  def getLocalStoreScope(s: Ident): Scope = return _localStores.getOrElseUpdate(s, Scope())
  def getLocalStoreScope(s: String): Scope = return _localStores.getOrElseUpdate(Ident(s), Scope())

  def getRoutine(i: Ident): Decl = return _routines.get(i).get

}